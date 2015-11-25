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

      address = mkOption {
        type = types.string;
        default = "localhost";
        description = "The address to listen on (use 0.0.0.0 for all)";
      };

      port = mkOption {
        type = types.int;
        default = 80;
      };

      user = mkOption {
        default = "wwwrun";
        description = "User account under which fsrest runs.";
      };

      group = mkOption {
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
    systemd.services.fsrest = {
      description = "fsrest web server";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/fsrest ${cfg.directory} ${cfg.address} ${toString cfg.port}";
        Restart = "on-failure";
        User = cfg.user;
        Group = cfg.group;
      };
    };

    users.extraUsers = optionalAttrs (cfg.user == "wwwrun") (singleton
      { name = "wwwrun";
        group = cfg.group;
        uid = config.ids.uids.wwwrun;
      });

    users.extraGroups = optionalAttrs (cfg.group == "wwwrun") (singleton
      { name = "wwwrun";
        gid = config.ids.gids.wwwrun;
      });
  };

}