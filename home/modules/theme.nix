{ pkgs, lib, ... }:

{
  gtk = {
    enable = true;
    theme = {
      name = "WhiteSur-Dark";
      package = pkgs.whitesur-gtk-theme;
    };
    iconTheme = {
      name = "WhiteSur-dark";
      package = pkgs.whitesur-icon-theme;
    };
    cursorTheme = {
      name = "WhiteSur-cursors-dark";
      package = pkgs.whitesur-cursors;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "qtct";
  };

  xdg.configFile."qt5ct/qt5ct.conf".source = ../../config/qt5ct/qt5ct.conf;
  xdg.configFile."qt6ct/qt6ct.conf".source = ../../config/qt6ct/qt6ct.conf;
  xdg.configFile."Kvantum/kvantum.kvconfig".source = ../../config/Kvantum/kvantum.kvconfig;
}
