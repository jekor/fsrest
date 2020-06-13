{ config, lib, pkgs, ... }:

let

  inherit (lib) mkOption mkIf types optionalAttrs singleton;
  cfg = config.services.fsrest;

in {

  options = {
    services.fsrest = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = "Enable the fsrest web server";
      };

      directory = mkOption {
        type = types.path;
        description = "The directory to serve";
      };

      socket = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "fsrest.sock";
        description = "UNIX domain socket path to listen on (relative to /var/run/fsrest)";
      };

      address = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "localhost";
        description = "The address to listen on (use 0.0.0.0 for all)";
      };

      port = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 80;
        description = "The port to listen on";
      };

      user = mkOption {
        type = types.str;
        default = "wwwrun";
        description = "User account under which fsrest runs.";
      };

      group = mkOption {
        type = types.str;
        default = "wwwrun";
        description = "Group account under which fsrest runs.";
      };

      package = mkOption {
        type = types.package;
        description = "The built fsrest package";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = cfg.socket == null || cfg.address == null;
      message = "Must not use options 'socket' and 'address' simultaneously.";
    } {
      assertion = (cfg.address != null) == (cfg.port != null);
      message = "Must use address and port options together.";
    }];

    systemd.services.fsrest =
      let
        addr = if cfg.socket != null
               then "--socket ${cfg.socket}"
               else "--address ${cfg.address} --port ${toString cfg.port}";
      in {
        description = "fsrest web server";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          RuntimeDirectory = "fsrest";
          WorkingDirectory = "/run/fsrest";
          ExecStart = "${cfg.package}/bin/fsrest ${addr} ${cfg.directory}";
          Restart = "on-failure";
          User = cfg.user;
          Group = cfg.group;
        };
      };

    users.extraUsers = optionalAttrs (cfg.user == "wwwrun") {
      wwwrun = {
        group = cfg.group;
        uid = config.ids.uids.wwwrun;
      };
    };

    users.extraGroups = optionalAttrs (cfg.group == "wwwrun") {
      wwwrun = {
        gid = config.ids.gids.wwwrun;
      };
    };
  };

}
