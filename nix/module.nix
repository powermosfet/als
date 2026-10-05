{ config, lib, pkgs, defaultPackage, ... }:
let
  cfg = config.services.als;
  environment = {
    CLIENT_ID = cfg.clientId;
    LIST_ID = cfg.listId;
    TOKEN_FILE = "/var/lib/als/tokens.json";
    RABBITMQ_HOST = cfg.rabbitmq.host;
    RABBITMQ_PORT = toString cfg.rabbitmq.port;
    RABBITMQ_VHOST = cfg.rabbitmq.vhost;
    RABBITMQ_USERNAME = cfg.rabbitmq.username;
    RABBITMQ_QUEUE = cfg.rabbitmq.queue;
    RETRY_DELAY_SECONDS = toString cfg.retryDelaySeconds;
    HTTP_TIMEOUT_SECONDS = toString cfg.httpTimeoutSeconds;
  };
  authArguments = lib.mapAttrsToList (name: value: "--setenv=${name}=${value}") environment
    ++ lib.optional (cfg.environmentFile != null) "--property=EnvironmentFile=${cfg.environmentFile}";
  authCommand = pkgs.writeShellApplication {
    name = "als-auth";
    runtimeInputs = [ pkgs.systemd pkgs.util-linux ];
    text = ''
      als_auth_package=${lib.escapeShellArg (toString cfg.package)}
      als_auth_environment=(${lib.escapeShellArgs authArguments})
      ${builtins.readFile ./als-auth.sh}
    '';
  };
in {
  options.services.als = {
    enable = lib.mkEnableOption "the ALS RabbitMQ to Microsoft To Do worker";
    package = lib.mkOption { type = lib.types.package; default = defaultPackage; description = "ALS executable package."; };
    clientId = lib.mkOption { type = lib.types.str; description = "Microsoft public-client application ID."; };
    listId = lib.mkOption { type = lib.types.str; description = "Microsoft To Do list ID."; };
    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Optional absolute runtime environment file, for example for RABBITMQ_PASSWORD. Keep secrets outside the Nix store.";
    };
    retryDelaySeconds = lib.mkOption { type = lib.types.ints.positive; default = 5; description = "Delay before retrying transient failures."; };
    httpTimeoutSeconds = lib.mkOption { type = lib.types.ints.positive; default = 30; description = "HTTP request timeout."; };
    rabbitmq = {
      host = lib.mkOption { type = lib.types.str; default = "localhost"; description = "RabbitMQ host."; };
      port = lib.mkOption { type = lib.types.port; default = 5672; description = "RabbitMQ port."; };
      vhost = lib.mkOption { type = lib.types.str; default = "/"; description = "Existing RabbitMQ virtual host."; };
      username = lib.mkOption { type = lib.types.str; default = "guest"; description = "RabbitMQ username."; };
      queue = lib.mkOption { type = lib.types.str; default = "shopping-list-items"; description = "Existing RabbitMQ queue."; };
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      { assertion = cfg.clientId != "" && cfg.listId != ""; message = "services.als requires clientId and listId."; }
      { assertion = cfg.environmentFile == null || lib.hasPrefix "/" cfg.environmentFile;
        message = "services.als.environmentFile must be an absolute runtime path."; }
    ];
    users.groups.als = {};
    users.users.als = { isSystemUser = true; group = "als"; };
    environment.systemPackages = [ authCommand ];
    systemd.services.als = {
      description = "ALS RabbitMQ to Microsoft To Do worker (sudo als-auth to sign in)";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      inherit environment;
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/als";
        User = "als";
        Group = "als";
        StateDirectory = "als";
        StateDirectoryMode = "0700";
        UMask = "0077";
        Restart = "on-failure";
        RestartSec = 5;
        RestartPreventExitStatus = [ 78 ];
        TimeoutStopSec = 15;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
      } // lib.optionalAttrs (cfg.environmentFile != null) { EnvironmentFile = cfg.environmentFile; };
    };
  };
}
