{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ 
    eww
    jaq 
    socat 
    pamixer 
    brightnessctl 
    playerctl 
  ];

  xdg.configFile."eww/eww.yuck".source = ./eww.yuck;
  xdg.configFile."eww/eww.scss".source = ./eww.scss;
  xdg.configFile."eww/yuck".source = ./yuck;
  xdg.configFile."eww/scss".source = ./scss;
  xdg.configFile."eww/scripts".source = ./scripts;
}
