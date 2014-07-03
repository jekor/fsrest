{ config, lib, pkgs, ... }:

let

  inherit (lib) mkOption mkIf types;
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
      serviceConfig.ExecStart = "${cfg.package}/bin/fsrest ${cfg.directory} ${cfg.address} ${toString cfg.port}";
      serviceConfig.Restart = "on-failure";
    };
  };

}