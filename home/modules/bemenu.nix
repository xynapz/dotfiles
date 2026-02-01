{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.bemenu;
in {
  options.programs.bemenu = {
    # Removed conflicting options (enable, package, settings) as they are already defined in Home Manager
    
    theme = {
      font = mkOption {
        type = types.str;
        default = "IBM Plex Mono 14";
        description = "Font to use for bemenu";
      };

      width = mkOption {
        type = types.int;
        default = 40;
        description = "Width of the bemenu window (percent or pixels)";
      };

      lines = mkOption {
        type = types.int;
        default = 15;
        description = "Number of lines to display";
      };

      prompt = mkOption {
        type = types.str;
        default = "❯ ";
        description = "Prompt text";
      };

      # Color scheme (Matches dotfiles/fuzzel theme)
      backgroundColor = mkOption {
        type = types.str;
        default = "#222222";
        description = "Background color";
      };

      foregroundColor = mkOption {
        type = types.str;
        default = "#ffffff";
        description = "Foreground (text) color";
      };

      selectedBackgroundColor = mkOption {
        type = types.str;
        default = "#285577";
        description = "Selected item background color";
      };

      selectedForegroundColor = mkOption {
        type = types.str;
        default = "#ffffff";
        description = "Selected item foreground color";
      };

      titleBackgroundColor = mkOption {
        type = types.str;
        default = "#222222";
        description = "Title background color";
      };

      titleForegroundColor = mkOption {
        type = types.str;
        default = "#3daee9";
        description = "Title foreground color";
      };

      borderColor = mkOption {
        type = types.str;
        default = "#000000";
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
    # We do NOT redefine home.packages = [ cfg.package ] because the main module handles it 
    # (if we set programs.bemenu.enable = true there too due to double import, but safe here)
    # Actually, standard module adds package if enable is true.

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
          "--bdr '${cfg.theme.borderColor}'"
          "--center"
          "-W 0.${toString cfg.theme.width}" # Assuming width is int 40 -> 0.40
          "--prompt '${cfg.theme.prompt}'"
          "-l '${toString cfg.theme.lines}'"
          "--ignorecase"
          "--wrap"
        ]
      );
    };
  };
}
