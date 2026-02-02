{ pkgs, ... }:

{
  home.packages = [ pkgs.fuzzel];

  xdg.configFile."fuzzel/fuzzel.ini".source = ../../config/fuzzel/fuzzel.ini;
}
