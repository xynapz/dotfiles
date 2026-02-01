# BeMenu Launcher Configuration
{ config, pkgs, lib, ... }:

{
  programs.bemenu = {
    enable = true;
    
    settings = {
      line-height = 28;
      prompt = "❯ ";
      font = "IBM Plex Mono 14";
      ignorecase = true;
      wrap = true;
      
      # Colors (Matching system theme)
      # Format: "item" = "fg bg"
      tb = "#222222"; # Title bg (not commonly used alone, but consistent)
      tf = "#3daee9"; # Title fg
      fb = "#222222"; # Filter bg
      ff = "#ffffff"; # Filter fg
      nb = "#222222"; # Normal bg
      nf = "#ffffff"; # Normal fg
      hb = "#285577"; # Highlight bg (Selected item)
      hf = "#ffffff"; # Highlight fg
      sb = "#285577"; # Selected bg
      sf = "#ffffff"; # Selected fg
      scb = "#222222"; # Scrollbar bg
      scf = "#3daee9"; # Scrollbar fg
    };
  };

  # Remove Fuzzel config as we are switching
  # xdg.configFile."fuzzel/fuzzel.ini".source = ...; 
}
