# pellucid-server

The server for Pellucid: a photo-sharing server that speaks the client API Pixelfed's apps speak, written in Haskell on WAI and Warp with PostgreSQL. The client lives in the `pellucid` repository. What the server is meant to do is in `.inference/pellucid-server.allium`, which is checked by `allium check` and updated with every change to the code.

This is the first batch. It is one slice through the whole stack, tested end to end. It must not be put on the internet yet, for the reasons under What does not.

## What works

A person can sign in, post a photo and read it back.

- An app registers itself (`POST /api/v1/apps`), as a confidential client with a secret or, with `token_endpoint_auth_method=none`, as a public client that must use PKCE.
- The person signs in on a page the server draws and allows the app (`/oauth/authorize`). The app exchanges the code for a token (`/oauth/token`) and can give the token back (`/oauth/revoke`).
- Uploads (`POST /api/v2/media`) are JPEG or PNG. Before a byte is stored the file is stripped of everything that is not the picture: EXIF with its position and device, the embedded thumbnail, XMP, IPTC, comments, and anything after the end of the image. A JPEG keeps its orientation and nothing else. The file is never decoded, so the picture is the uploader's own bit for bit.
- A post is a caption and one to ten uploads (`POST /api/v1/statuses`), public, unlisted or for followers only. Its author can delete it.
- The home timeline is the person's own posts and those of the accounts they follow, newest first, with nothing inserted. The public timeline is the public posts of local accounts. Every list pages by an opaque cursor in a `Link` header.
- Following and unfollowing, relationships, an account and its posts.
- A followers-only post is readable by its author and by accepted followers. For anyone else it is a 404, the same as a post that does not exist.

## What does not

Comments, likes, shares, bookmarks, hashtags, notifications, direct messages, blocks and mutes, edits, reports and moderation, export, account deletion, federation. None of it is built. A request to post a comment is answered with 501 and is not published as something else.

Things that are wrong or missing in what is built are listed in the spec as `KnownIssue` entries. The ones that matter most:

- **The server itself limits nothing.** Each password attempt costs one Argon2id computation and that is all. The NixOS module's proxy limits requests per address to the sign-in page and to client registration, which slows one address and does nothing about many. This must be fixed before the server faces the internet.
- **GIF, WebP and MP4 are refused.** Only types with a metadata scrubber are accepted, and only JPEG and PNG have one. There is no video.
- **No thumbnails.** `preview_url` is the full file.
- **Nothing runs on a clock.** An upload that is never posted and never taken back stays on disk.
- **The Pellucid client has not signed in to it yet.** The client accepts the server's nodeinfo name, `pellucid`, and its decoders read this server's answers. It is https only and trusts the public certificate authorities and no others, so it reaches a deployed server with a real certificate and nothing else.
- **The NixOS module has been evaluated and never run.** Whether the service starts under its systemd restrictions, whether nginx accepts the configuration and whether the certificate is issued have not been seen.

## Layout

```
.inference/pellucid-server.allium   the spec
app/Main.hs                         the command line
src/Pellucid/Domain/                the rules' decisions, as pure functions
src/Pellucid/Store/                 PostgreSQL: schema, queries, transactions
src/Pellucid/Http/                  parameters, route table, entities, pages
src/Pellucid/Http/Routes/           the handlers, by area
test/                               unit and property tests, and one session end to end
nix/pellucid-server.nix             the committed cabal2nix derivation
nix/module.nix                      the NixOS module
scripts/check.fish                  the gate
scripts/dev.fish                    a local database and server
```

Each rule in the spec that is built has a function in `src/Pellucid/Domain` named for it in a comment: `authorize` is rule AuthorizationGranted, `redeem` is TokenIssued, `checkPublish` is PostPublished, `canSee` is the reading rule, `decideFollow` is the two follow rules, `scrub` is MediaProcessingStarted. The functions take facts and return a decision. They touch no database and no clock, which is what lets the tests cover them case by case. The store carries a decision out inside one transaction.

Routes are a table of values, not types. Pixelfed's own clients send the same field in a query string, a form, a multipart body or JSON depending on the client and the route, so every request's inputs are merged into one set of named values and a route describes what it needs with an applicative parser over that set. A request with three bad fields is answered with all three.

## Building and testing

Everything comes from the dev shell. Run these on winsmuth, from the repository root.

```fish
nix develop
fish scripts/check.fish
```

The gate checks formatting, builds with warnings as errors and runs the tests. `fourmolu.yaml` holds the settings cheeblr's backend uses. The files of the first batch were sent before the formatter had been run, so the gate fails at its first step until this has been run once:

```fish
nix develop --command fourmolu --mode inplace app src test
```

It rewrites 34 of the 39 Haskell files, mostly the indentation of `where` blocks and the layout of import lists. The test suite starts a PostgreSQL of its own in a temporary directory and removes it afterwards, so nothing needs to be running. PostgreSQL does not run as root, so the tests cannot either.

After changing the dependencies in `pellucid-server.cabal`:

```fish
nix run .#cabal2nix
```

## Running it locally

```fish
fish scripts/dev.fish db-start
fish scripts/dev.fish create-account harry
fish scripts/dev.fish serve
```

The first command creates a PostgreSQL in `.dev-postgres` that listens on a socket in that directory and nowhere else. The second asks for a password of at least twelve characters. The third serves on `http://127.0.0.1:8080`. To see that it answers:

```fish
curl http://127.0.0.1:8080/api/nodeinfo/2.0.json
```

Stop the database with `fish scripts/dev.fish db-stop`. Deleting `.dev-postgres` and `.dev-media` removes every trace.

## Deploying

The flake's `nixosModules.default` runs the server on a NixOS machine: the service as its own user on 127.0.0.1, PostgreSQL on the same machine reached over its socket with no password, and nginx in front with a certificate from Let's Encrypt by DNS-01. DNS-01 means the machine does not have to be reachable from outside. Only the DNS record for the name is public, and it can point at a private address. This is how the Pellucid client can be tested against a server at home: the client accepts only https with a certificate from a public authority.

A machine that imports the module sets:

```nix
services.pellucid-server = {
  enable = true;
  domain = "photos.example.org";
  acme = {
    email = "you@example.org";
    dnsProvider = "cloudflare";
    environmentFile = config.sops.secrets."acme-dns".path;
  };
  openFirewall = true;
};
```

The values are examples. `dnsProvider` is the provider's lego name and `environmentFile` holds its credentials as lego names them. Three things are left to the machine's own configuration: `security.acme.acceptTerms`, which is the operator's to accept; persistence, because on a machine whose root is wiped at boot `/var/lib/pellucid-server` and PostgreSQL's data directory must be among the paths that are kept; and nginx's global settings, which the module does not touch.

On the machine, an account is created with the server's own command line, run as the service's user:

```fish
sudo pellucid-server-manage create-account harry
```

The module has been evaluated for a whole machine against the locked nixpkgs, with no failed assertion and no warning. It has never run. It is meant for a private network: what the server lacks before it can face the internet is listed above.

## Settings

Each setting is a flag or an environment variable. The database URL should come from the environment: a flag is visible to every user of the machine in the process list. A password for a new account is read from standard input and is never an argument.

| Flag | Environment | Default | |
|---|---|---|---|
| `--database-url` | `PELLUCID_DATABASE_URL` | `postgresql:///pellucid` | libpq connection string or URI |
| `--base-url` | `PELLUCID_BASE_URL` | `http://127.0.0.1:8080` | where clients reach the server; every URL it writes starts with this |
| `--title` | `PELLUCID_TITLE` | `Pellucid` | the name on the sign-in page |
| `--media-dir` | `PELLUCID_MEDIA_DIR` | `media` | where uploads are stored |
| `--host` | `PELLUCID_HOST` | `127.0.0.1` | address to listen on; the default is this machine only, for a reverse proxy to reach |
| `--port` | `PELLUCID_PORT` | `8080` | |

Commands: `serve` applies migrations and listens. `migrate` applies migrations and stops. `create-account NAME [--moderator]` creates an account.

## How secrets are kept

A token, an authorization code and a client secret are 32 random bytes from the operating system. Each is handed out once and stored as its SHA-256, so a copy of the database holds nothing that can be used to sign in. A password is stored as an Argon2id hash. A media file is stored under a random 256-bit key, and knowing its URL is what lets a reader fetch it, which is how an image tag loads a picture from a followers-only post without a token.

## Licence

AGPL-3.0-or-later. The licence text is not in the repository yet.
