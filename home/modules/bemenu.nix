# BeMenu Launcher Configuration
{ config, pkgs, lib, ... }:

{
  programs.bemenu = {
    enable = true;
  };

  # Centralized environment variables for BeMenu
  home.sessionVariables = {
    BEMENU_OPTS = lib.concatStringsSep " " [
      "-i"             # Case insensitive
      "-l 15"          # Number of lines
      "--center"       # Center on screen
      "-W 0.40"        # Width (40% of screen)
      "--fn 'IBM Plex Mono 14'"
      "--prompt '❯ '"
      "--wrap"
      
      # Colors (Matches Fuzzel/Waybar theme)
      "--tb '#222222'" # Title bg
      "--tf '#3daee9'" # Title fg
      "--fb '#222222'" # Filter bg
      "--ff '#ffffff'" # Filter fg
      "--nb '#222222'" # Normal bg
      "--nf '#ffffff'" # Normal fg
      "--hb '#285577'" # Highlight bg (Selected item)
      "--hf '#ffffff'" # Highlight fg
      "--sb '#285577'" # Selected bg
      "--sf '#ffffff'" # Selected fg
      "--scb '#222222'" # Scrollbar bg
      "--scf '#3daee9'" # Scrollbar fg
      "--bdr '#000000'" # Border
    ];
  };
}
