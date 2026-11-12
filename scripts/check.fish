#!/usr/bin/env fish
# The pellucid-server gate. Run from the repository root inside
# `nix develop`. Exits non-zero at the first failure.
#
# The test suite starts a PostgreSQL of its own in a temporary directory
# and removes it afterwards; nothing needs to be running. PostgreSQL does
# not run as root, so neither does this.

function step --argument-names name
    set_color --bold
    echo "== $name"
    set_color normal
end

step "fourmolu (formatting)"
fourmolu --mode check app src test; or exit 1

step "cabal build (warnings are errors)"
cabal build all --ghc-options=-Werror; or exit 1

step "cabal test"
cabal test all --ghc-options=-Werror --test-show-details=direct; or exit 1

echo "all checks passed"