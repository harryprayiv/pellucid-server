{
  description = "pellucid-server: a photo-sharing server that speaks the Pixelfed client API";

  # SHAPE
  #
  # The same as open-slop's: nixpkgs' Haskell infrastructure with one
  # compiler pinned, an overlay that adds the package, shellFor for the dev
  # shell. Not haskell.nix: the package has about twenty ordinary
  # dependencies, all of them in nixpkgs.
  #
  # The derivation is COMMITTED at nix/pellucid-server.nix and regenerated
  # with `nix run .#cabal2nix`, so a consumer (neoblade-config, whose checks
  # evaluate every host) never needs import-from-derivation.
  #
  # OUTPUTS A CONSUMER USES
  #
  #   overlays.default        pkgs.pellucid-server.server, the executable alone
  #   packages.default        the same
  #   nixosModules.default    services.pellucid-server: the service behind
  #                           nginx with a certificate by DNS-01 and a local
  #                           PostgreSQL. See nix/module.nix
  #   checks.tests            the package built with its test suite run
  #
  # The module is for a machine on a private network. What the server lacks
  # before it can face the internet is listed as known issues in
  # .inference/pellucid-server.allium.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      compiler = "ghc96";

      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      overlay = final: prev:
        let
          hlib = final.haskell.lib;
          fs = final.lib.fileset;

          hpkgs = final.haskell.packages.${compiler}.extend (hfinal: hprev: {
            pellucid-server = hlib.overrideCabal (hfinal.callPackage ./nix/pellucid-server.nix { }) (old: {
              # Only what cabal reads, so that editing the README or the spec
              # does not rebuild the server.
              src = fs.toSource {
                root = ./.;
                fileset = fs.unions [
                  ./pellucid-server.cabal
                  ./app
                  ./src
                  ./test
                ];
              };
              # The test suite starts a PostgreSQL of its own in a temporary
              # directory and needs initdb and pg_ctl to do it.
              testToolDepends = (old.testToolDepends or [ ]) ++ [ final.postgresql ];
            });
          });
        in
        {
          pellucid-server = {
            haskellPackages = hpkgs;
            server = hlib.justStaticExecutables (hlib.dontHaddock hpkgs.pellucid-server);
          };
        };

      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };
    in
    {
      overlays.default = overlay;

      nixosModules.default = import ./nix/module.nix { inherit self; };

      packages = forAllSystems (system: {
        default = (pkgsFor system).pellucid-server.server;
      });

      checks = forAllSystems (system: {
        tests = (pkgsFor system).pellucid-server.haskellPackages.pellucid-server;
      });

      apps = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          # Regenerate nix/pellucid-server.nix after the cabal file's
          # dependencies change. cabal2nix writes `src = ./.;`, which from
          # inside nix/ would name the wrong directory; the overlay replaces
          # src in any case, and the line is corrected so the file is right
          # when read by itself.
          cabal2nix = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "pellucid-server-cabal2nix" ''
                set -euo pipefail
                cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                ${pkgs.cabal2nix}/bin/cabal2nix . > nix/pellucid-server.nix.new
                ${pkgs.gnused}/bin/sed -i -E 's@src = \./\.;@src = ../.;@' nix/pellucid-server.nix.new
                mv nix/pellucid-server.nix.new nix/pellucid-server.nix
                echo "wrote nix/pellucid-server.nix"
              ''
            );
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          hpkgs = pkgs.pellucid-server.haskellPackages;
        in
        {
          # `cabal build all`, `cabal test`, `fish scripts/check.fish`.
          default = hpkgs.shellFor {
            packages = p: [ p.pellucid-server ];

            nativeBuildInputs = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              pkgs.cabal2nix
              # initdb and pg_ctl for the test suite's own database, and psql.
              pkgs.postgresql
            ];
          };
        });

      formatter = forAllSystems (system: (pkgsFor system).nixpkgs-fmt);
    };
}