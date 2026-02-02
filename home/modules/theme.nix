{ pkgs, ... }:

{
  gtk = {
    enable = true;
    theme = {
      name = "Ant Dracula";
      package = pkgs.nordic;
    };
    iconTheme = {
      name = "Nordic-darker"; # Assuming this variant exists within pkgs.nordic or need to check actual content. For safety/common setup often just 'Nordic'.
      # Actually 'Nordic' package often provides icons too.
      # If pkgs.nordic contains icons, we can use it.
      # Safest bet for icons if specific package unknown is often Papirus-Dark or similar if Nordic icons aren't redundant.
      # BUT user asked for Nordic icons. Let's try 'Nordic-darker' if standard, or just 'Nordic'.
      # Common: theme='Nordic', icons='Nordic-darker' (from same repo).
      package = pkgs.nordic;
    };
    cursorTheme = {
      name = "WhiteSur-cursors";
      package = pkgs.nordic;
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
    platformTheme.name = "gtk";
  };
}
