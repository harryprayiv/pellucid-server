{-# LANGUAGE QuasiQuotes #-}

{- | The database schema, as a list of migrations that are applied in order
and never edited once released. A change to the schema is a new entry.

The schema is this server's own. It is not Pixelfed's: an existing
instance would come across through an importer, which is not written.

Where a spec invariant can be a constraint, it is one:

  * OneFollowPerPair is the primary key of @follows@
  * NobodyFollowsThemselves is a CHECK on @follows@
  * PublicClientsHoldNoSecret is a CHECK on @oauth_clients@
  * a media row has a post exactly when it is attached
  * an account has a leaving date exactly when it is leaving
  * every lifecycle column accepts only the states its entity declares

Ids come from @next_id()@: milliseconds since 2026-01-01 in the high
bits and a counter in the low sixteen, so ids sort by creation time,
which is what a cursor relies on.

The statements carry no SQL comments. The quasi-quoter joins lines, and
a @--@ comment would swallow the rest of its statement.
-}
module Pellucid.Store.Schema (
  migrations,
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

migrations :: [(Int, Text, Query)]
migrations =
  [ (1, "accounts, oauth, media, posts, follows", initial)
  ]

initial :: Query
initial =
  [sql|
    CREATE SEQUENCE id_sequence;

    CREATE FUNCTION next_id() RETURNS bigint LANGUAGE sql VOLATILE AS $$
      SELECT ((floor(extract(epoch FROM clock_timestamp()) * 1000)::bigint - 1767225600000) << 16)
             | (nextval('id_sequence') & 65535)
    $$;

    CREATE TABLE accounts (
      id bigint PRIMARY KEY DEFAULT next_id(),
      username text NOT NULL,
      username_key text NOT NULL,
      home text,
      display_name text NOT NULL,
      note text NOT NULL DEFAULT '',
      locked boolean NOT NULL DEFAULT false,
      is_moderator boolean NOT NULL DEFAULT false,
      status text NOT NULL DEFAULT 'active'
        CHECK (status IN ('active', 'suspended', 'leaving', 'departed')),
      moved_to bigint REFERENCES accounts (id),
      leaving_since timestamptz,
      password_hash text,
      created_at timestamptz NOT NULL DEFAULT now(),
      CHECK (username_key = lower(username)),
      CHECK ((status = 'leaving') = (leaving_since IS NOT NULL)),
      CHECK (home IS NOT NULL OR password_hash IS NOT NULL)
    );
    CREATE UNIQUE INDEX accounts_local_username ON accounts (username_key) WHERE home IS NULL;
    CREATE UNIQUE INDEX accounts_remote_username ON accounts (username_key, home) WHERE home IS NOT NULL;

    CREATE TABLE oauth_clients (
      id bigint PRIMARY KEY DEFAULT next_id(),
      client_key text NOT NULL UNIQUE,
      name text NOT NULL,
      kind text NOT NULL CHECK (kind IN ('public_client', 'confidential_client')),
      redirect_uris text[] NOT NULL CHECK (cardinality(redirect_uris) > 0),
      scopes text NOT NULL,
      secret_hash text,
      website text,
      created_at timestamptz NOT NULL DEFAULT now(),
      CHECK ((kind = 'public_client') = (secret_hash IS NULL))
    );

    CREATE TABLE authorization_codes (
      id bigint PRIMARY KEY DEFAULT next_id(),
      code_hash text NOT NULL UNIQUE,
      client_id bigint NOT NULL REFERENCES oauth_clients (id),
      account_id bigint NOT NULL REFERENCES accounts (id),
      redirect_uri text NOT NULL,
      scopes text NOT NULL,
      pkce_challenge text,
      issued_at timestamptz NOT NULL,
      status text NOT NULL DEFAULT 'issued' CHECK (status IN ('issued', 'redeemed', 'stale'))
    );

    CREATE TABLE access_tokens (
      id bigint PRIMARY KEY DEFAULT next_id(),
      token_hash text NOT NULL UNIQUE,
      client_id bigint NOT NULL REFERENCES oauth_clients (id),
      account_id bigint NOT NULL REFERENCES accounts (id),
      code_id bigint REFERENCES authorization_codes (id),
      scopes text NOT NULL,
      issued_at timestamptz NOT NULL,
      status text NOT NULL DEFAULT 'valid' CHECK (status IN ('valid', 'revoked', 'lapsed'))
    );
    CREATE INDEX access_tokens_account ON access_tokens (account_id);
    CREATE INDEX access_tokens_code ON access_tokens (code_id);

    CREATE TABLE posts (
      id bigint PRIMARY KEY DEFAULT next_id(),
      author_id bigint NOT NULL REFERENCES accounts (id),
      caption text NOT NULL,
      visibility text NOT NULL CHECK (visibility IN ('public', 'unlisted', 'followers_only', 'direct')),
      sensitive boolean NOT NULL DEFAULT false,
      spoiler_text text NOT NULL DEFAULT '',
      comments_disabled boolean NOT NULL DEFAULT false,
      in_reply_to bigint REFERENCES posts (id),
      published_at timestamptz NOT NULL,
      edit_count integer NOT NULL DEFAULT 0 CHECK (edit_count >= 0),
      status text NOT NULL DEFAULT 'published' CHECK (status IN ('published', 'withdrawn', 'taken_down'))
    );
    CREATE INDEX posts_author ON posts (author_id, id DESC);
    CREATE INDEX posts_public ON posts (id DESC) WHERE status = 'published' AND visibility = 'public' AND in_reply_to IS NULL;

    CREATE TABLE media (
      id bigint PRIMARY KEY DEFAULT next_id(),
      owner_id bigint NOT NULL REFERENCES accounts (id),
      post_id bigint REFERENCES posts (id),
      position integer,
      storage_key text NOT NULL UNIQUE,
      mime text NOT NULL,
      extension text NOT NULL,
      size_bytes bigint NOT NULL CHECK (size_bytes > 0),
      width integer,
      height integer,
      alt_text text NOT NULL DEFAULT '',
      uploaded_at timestamptz NOT NULL,
      status text NOT NULL
        CHECK (status IN ('received', 'processing', 'ready', 'attached', 'unusable', 'purged')),
      CHECK ((status = 'attached') = (post_id IS NOT NULL))
    );
    CREATE INDEX media_post ON media (post_id, position);
    CREATE INDEX media_owner_unattached ON media (owner_id, uploaded_at) WHERE status IN ('ready', 'unusable');

    CREATE TABLE follows (
      follower_id bigint NOT NULL REFERENCES accounts (id),
      target_id bigint NOT NULL REFERENCES accounts (id),
      status text NOT NULL CHECK (status IN ('requested', 'following')),
      created_at timestamptz NOT NULL DEFAULT now(),
      PRIMARY KEY (follower_id, target_id),
      CHECK (follower_id <> target_id)
    );
    CREATE INDEX follows_target ON follows (target_id, status);
  |]
