# The NixOS module: pellucid-server as a systemd service behind nginx, with
# a certificate from Let's Encrypt and a PostgreSQL database on the same
# machine.
#
# WHAT IT SETS UP
#
#   * a system user, pellucid-server, that owns nothing but its state
#     directory, /var/lib/pellucid-server (uploads live in media/ under it)
#   * a PostgreSQL database and role of the same name. The server connects
#     over the socket in /run/postgresql and is recognised by its system
#     user, so there is no database password anywhere
#   * the service, listening on 127.0.0.1 only. It is allowed to talk to
#     this machine and to nothing else, which is all it does until
#     federation exists
#   * an nginx virtual host for the domain that terminates TLS, passes
#     requests on, and limits how often one address may ask for the sign-in
#     page or register a client
#   * a certificate by DNS-01, the same way the Pixelfed lab instance gets
#     one: the machine does not have to be reachable from outside, only the
#     DNS record is public
#   * pellucid-server-manage, the server's own command line run as the
#     service user with the service's settings
#
# WHAT IT LEAVES TO THE MACHINE'S OWN CONFIGURATION
#
#   * security.acme.acceptTerms. Accepting Let's Encrypt's terms is the
#     operator's act, not a module's
#   * persistence. On a machine whose root is wiped at boot,
#     /var/lib/pellucid-server and PostgreSQL's data directory must be
#     among the paths that are kept
#   * nginx's global settings. Nothing here changes how other virtual hosts
#     behave
#
# The rate limit is the only limit there is (the spec's KnownIssue
# no_sign_in_throttle): it counts requests per address at the proxy. The
# server itself counts nothing.
{ self }:
{ config, lib, pkgs, ... }:
let
  cfg = config.services.pellucid-server;

  user = "pellucid-server";
  stateDir = "/var/lib/pellucid-server";

  environment = {
    PELLUCID_DATABASE_URL = "postgresql:///${user}?host=/run/postgresql";
    PELLUCID_BASE_URL = "https://${cfg.domain}";
    PELLUCID_TITLE = cfg.title;
    PELLUCID_MEDIA_DIR = "${stateDir}/media";
    PELLUCID_HOST = "127.0.0.1";
    PELLUCID_PORT = toString cfg.port;
  };

  upstream = "http://127.0.0.1:${toString cfg.port}";

  # The server's command line with the service's settings, as the service's
  # user. It must be run as root, which is what lets it become that user:
  #   sudo pellucid-server-manage create-account NAME
  manage = pkgs.writeShellScriptBin "pellucid-server-manage" ''
    exec ${pkgs.util-linux}/bin/runuser -u ${user} -- \
      ${pkgs.coreutils}/bin/env ${lib.escapeShellArgs (lib.mapAttrsToList (name: value: "${name}=${value}") environment)} \
      ${cfg.package}/bin/pellucid-server "$@"
  '';

  limited = {
    proxyPass = upstream;
    recommendedProxySettings = true;
    extraConfig = ''
      limit_req zone=pellucid_unsigned burst=${toString cfg.rateLimit.burst} nodelay;
      limit_req_status 429;
    '';
  };
in
{
  options.services.pellucid-server = {
    enable = lib.mkEnableOption "pellucid-server, a photo-sharing server that speaks the Pixelfed client API";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      defaultText = lib.literalExpression "pellucid-server.packages.\${system}.default";
      description = "The pellucid-server executable.";
    };

    domain = lib.mkOption {
      type = lib.types.str;
      example = "photos.example.org";
      description = ''
        The name clients reach the server by. Every URL the server writes
        starts with https:// and this name, so it cannot be changed later
        without breaking the links already handed out.
      '';
    };

    title = lib.mkOption {
      type = lib.types.str;
      default = "Pellucid";
      description = "The server's name on its sign-in page.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "The port the server listens on, on 127.0.0.1, for nginx to reach.";
    };

    acme = {
      email = lib.mkOption {
        type = lib.types.str;
        description = "The address Let's Encrypt sends expiry notices to.";
      };
      dnsProvider = lib.mkOption {
        type = lib.types.str;
        example = "cloudflare";
        description = "The DNS provider that answers the DNS-01 challenge, by its lego name.";
      };
      environmentFile = lib.mkOption {
        type = lib.types.path;
        example = "/run/secrets/acme-dns";
        description = ''
          A file of environment variables holding the provider's
          credentials, as lego names them. It is read at run time and is
          never copied into the Nix store.
        '';
      };
    };

    rateLimit = {
      perMinute = lib.mkOption {
        type = lib.types.ints.positive;
        default = 10;
        description = ''
          How many requests a minute one address may make to the sign-in
          page and to client registration.
        '';
      };
      burst = lib.mkOption {
        type = lib.types.ints.unsigned;
        default = 5;
        description = "How many requests over that rate are let through at once before the rest are refused with 429.";
      };
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open ports 80 and 443.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${user} = {
      isSystemUser = true;
      group = user;
      description = "pellucid-server";
    };
    users.groups.${user} = { };

    services.postgresql = {
      enable = true;
      ensureDatabases = [ user ];
      ensureUsers = [
        {
          name = user;
          ensureDBOwnership = true;
        }
      ];
    };

    systemd.services.pellucid-server = {
      description = "pellucid-server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "postgresql.service" ];
      requires = [ "postgresql.service" ];
      inherit environment;

      serviceConfig = {
        ExecStart = "${cfg.package}/bin/pellucid-server serve";
        User = user;
        Group = user;
        StateDirectory = "pellucid-server";
        StateDirectoryMode = "0750";
        WorkingDirectory = stateDir;
        UMask = "0077";
        Restart = "on-failure";
        RestartSec = 5;

        # The service reads its own state directory and PostgreSQL's
        # socket, listens on loopback, and needs nothing else.
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        RemoveIPC = true;
        CapabilityBoundingSet = "";
        AmbientCapabilities = "";
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "@system-service" ];
        IPAddressDeny = "any";
        IPAddressAllow = "localhost";
      };
    };

    security.acme.certs.${cfg.domain} = {
      email = cfg.acme.email;
      dnsProvider = cfg.acme.dnsProvider;
      environmentFile = cfg.acme.environmentFile;
      group = config.services.nginx.group;
    };

    services.nginx = {
      enable = true;

      # One zone for everything a caller without a token can ask that costs
      # the server real work: a password check, or a row in the database.
      appendHttpConfig = ''
        limit_req_zone $binary_remote_addr zone=pellucid_unsigned:10m rate=${toString cfg.rateLimit.perMinute}r/m;
      '';

      virtualHosts.${cfg.domain} = {
        forceSSL = true;
        useACMEHost = cfg.domain;

        # The largest upload the server accepts, with room for the
        # multipart envelope around it. The server enforces its own limit.
        extraConfig = ''
          client_max_body_size 20m;
        '';

        locations."/" = {
          proxyPass = upstream;
          recommendedProxySettings = true;
        };
        locations."= /oauth/authorize" = limited;
        locations."= /api/v1/apps" = limited;
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ 80 443 ];

    environment.systemPackages = [ manage ];
  };
}
