{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.bemenu;
in {
  options.programs.bemenu = {
    enable = mkEnableOption "bemenu launcher with rofi-like centered UI";

    package = mkOption {
      type = types.package;
      default = pkgs.bemenu;
      defaultText = literalExpression "pkgs.bemenu";
      description = "The bemenu package to use";
    };

    settings = mkOption {
      type = types.attrs;
      default = {};
      description = "Additional bemenu configuration options";
      example = literalExpression ''
        {
          ignorecase = true;
          list = 20;
        }
      '';
    };

    theme = {
      font = mkOption {
        type = types.str;
        default = "monospace 12";
        description = "Font to use for bemenu";
      };

      width = mkOption {
        type = types.int;
        default = 800;
        description = "Width of the bemenu window in pixels";
      };

      lines = mkOption {
        type = types.int;
        default = 15;
        description = "Number of lines to display";
      };

      prompt = mkOption {
        type = types.str;
        default = "Run:";
        description = "Prompt text";
      };

      # Color scheme (default: Gruvbox dark theme)
      backgroundColor = mkOption {
        type = types.str;
        default = "#282828";
        description = "Background color";
      };

      foregroundColor = mkOption {
        type = types.str;
        default = "#ebdbb2";
        description = "Foreground (text) color";
      };

      selectedBackgroundColor = mkOption {
        type = types.str;
        default = "#458588";
        description = "Selected item background color";
      };

      selectedForegroundColor = mkOption {
        type = types.str;
        default = "#ebdbb2";
        description = "Selected item foreground color";
      };

      titleBackgroundColor = mkOption {
        type = types.str;
        default = "#3c3836";
        description = "Title background color";
      };

      titleForegroundColor = mkOption {
        type = types.str;
        default = "#fabd2f";
        description = "Title foreground color";
      };

      borderColor = mkOption {
        type = types.str;
        default = "#458588";
        description = "Border color";
      };

      borderSize = mkOption {
        type = types.int;
        default = 2;
        description = "Border size in pixels";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.sessionVariables = {
      BEMENU_OPTS = concatStringsSep " " (
        [
          "--fn '${cfg.theme.font}'"
          "--tb '${cfg.theme.titleBackgroundColor}'"
          "--tf '${cfg.theme.titleForegroundColor}'"
          "--fb '${cfg.theme.backgroundColor}'"
          "--ff '${cfg.theme.foregroundColor}'"
          "--nb '${cfg.theme.backgroundColor}'"
          "--nf '${cfg.theme.foregroundColor}'"
          "--hb '${cfg.theme.selectedBackgroundColor}'"
          "--hf '${cfg.theme.selectedForegroundColor}'"
          "--sb '${cfg.theme.selectedBackgroundColor}'"
          "--sf '${cfg.theme.selectedForegroundColor}'"
          "--border '${toString cfg.theme.borderSize}'"
          "--bdr '${cfg.theme.borderColor}'"
          "--width-factor '0.0'"
          "--ch '${toString (cfg.theme.lines + 1)}'"
          "--cw '${toString cfg.theme.width}'"
          "--center"
          "--prompt '${cfg.theme.prompt}'"
          "--list '${toString cfg.theme.lines}'"
        ] ++ 
        (optional (cfg.settings ? ignorecase && cfg.settings.ignorecase) "--ignorecase") ++
        (optional (cfg.settings ? wrap && cfg.settings.wrap) "--wrap")
      );
    };

    # Optional: Create a bemenu-run wrapper script
    home.file.".local/bin/bemenu-run" = {
      text = ''
        #!/bin/sh
        exec ${cfg.package}/bin/bemenu-run "$@"
      '';
      executable = true;
    };
  };
}
