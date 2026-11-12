#!/usr/bin/env fish
# A database and a server for trying things out on this machine. Run from
# the repository root inside `nix develop`.
#
#   fish scripts/dev.fish db-start              create (the first time) and start a PostgreSQL in .dev-postgres
#   fish scripts/dev.fish db-stop
#   fish scripts/dev.fish create-account NAME   asks for a password of at least twelve characters
#   fish scripts/dev.fish serve                 http://127.0.0.1:8080
#
# The database listens on a socket inside .dev-postgres and nowhere else,
# and trusts whoever can reach that socket. It is for development only.

set root (pwd)
set cluster $root/.dev-postgres
set -x PELLUCID_DATABASE_URL "host=$cluster dbname=postgres user=postgres"
set -x PELLUCID_MEDIA_DIR $root/.dev-media

switch "$argv[1]"
    case db-start
        if not test -d $cluster/data
            mkdir -p $cluster; or exit 1
            initdb -D $cluster/data -A trust -U postgres --no-sync -E UTF8 --locale=C >/dev/null; or exit 1
        end
        if pg_ctl -D $cluster/data status >/dev/null 2>&1
            echo "the database is already running"
        else
            pg_ctl -D $cluster/data -w -o "-c listen_addresses='' -c unix_socket_directories='$cluster'" -l $cluster/postgres.log start; or exit 1
        end
    case db-stop
        pg_ctl -D $cluster/data -m fast -w stop; or exit 1
    case create-account
        cabal run -v0 pellucid-server -- create-account $argv[2..]; or exit 1
    case serve
        cabal run -v0 pellucid-server -- serve; or exit 1
    case '*'
        echo "usage: fish scripts/dev.fish db-start | db-stop | create-account NAME | serve"
        exit 2
end
