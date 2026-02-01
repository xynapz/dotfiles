{ pkgs, ... }:

{
  home.packages = [ pkgs.nwg-bar ];

  xdg.configFile."nwg-bar/bar.json".source = ../../config/nwg-bar/bar.json;
  xdg.configFile."nwg-bar/style.css".source = ../../config/nwg-bar/style.css;
}
