-- ~/.config/wezterm/wezterm.lua
-- WezTerm config - xz-nord theme integrated with Sway/Emacs workflow
local wezterm = require("wezterm")
local config = wezterm.config_builder()
local act = wezterm.action

-- XZ-NORD COLORSCHEME (matches xz-nord-theme.el)
local xz_nord = {
  -- Base colors from xz-nord-theme.el
  background = "#1b1d1e",
  foreground = "#D8DEE9",
  cursor_bg = "#88C0D0",
  cursor_fg = "#1b1d1e",
  cursor_border = "#88C0D0",
  selection_bg = "#434C5E",
  selection_fg = "#D8DEE9",
  scrollbar_thumb = "#434C5E",

  -- Tab bar
  tab_bar_bg = "#1b1d1e",
  active_tab_bg = "#434C5E",
  active_tab_fg = "#88C0D0",
  inactive_tab_bg = "#262829",
  inactive_tab_fg = "#616e88",

  -- ANSI colors (Nord palette)
  ansi = {
    "#3B4252", -- black (nord1)
    "#BF616A", -- red
    "#A3BE8C", -- green
    "#EBCB8B", -- yellow
    "#81A1C1", -- blue (nord9)
    "#B48EAD", -- magenta
    "#88C0D0", -- cyan (nord8)
    "#E5E9F0", -- white
  },
  brights = {
    "#4C566A", -- bright black (nord3)
    "#BF616A", -- bright red
    "#A3BE8C", -- bright green
    "#EBCB8B", -- bright yellow
    "#81A1C1", -- bright blue
    "#B48EAD", -- bright magenta
    "#8FBCBB", -- bright cyan (nord7)
    "#ECEFF4", -- bright white
  },
}

config.colors = {
  foreground = xz_nord.foreground,
  background = xz_nord.background,
  cursor_bg = xz_nord.cursor_bg,
  cursor_fg = xz_nord.cursor_fg,
  cursor_border = xz_nord.cursor_border,
  selection_bg = xz_nord.selection_bg,
  selection_fg = xz_nord.selection_fg,
  scrollbar_thumb = xz_nord.scrollbar_thumb,
  ansi = xz_nord.ansi,
  brights = xz_nord.brights,

  tab_bar = {
    background = xz_nord.tab_bar_bg,
    active_tab = {
      bg_color = xz_nord.active_tab_bg,
      fg_color = xz_nord.active_tab_fg,
      intensity = "Bold",
    },
    inactive_tab = {
      bg_color = xz_nord.inactive_tab_bg,
      fg_color = xz_nord.inactive_tab_fg,
    },
    inactive_tab_hover = {
      bg_color = xz_nord.active_tab_bg,
      fg_color = xz_nord.foreground,
    },
    new_tab = {
      bg_color = xz_nord.inactive_tab_bg,
      fg_color = xz_nord.inactive_tab_fg,
    },
    new_tab_hover = {
      bg_color = xz_nord.active_tab_bg,
      fg_color = xz_nord.active_tab_fg,
    },
  },
}

-- FONT (Iosevka Term - matching Emacs)
config.font = wezterm.font("Iosevka", { weight = "Regular" })
config.font_size = 16.0
config.line_height = 1.0
config.cell_width = 1.0

-- Font rendering
config.freetype_load_target = "Light"
config.freetype_render_target = "HorizontalLcd"

-- WINDOW APPEARANCE
config.window_background_opacity = 0.998
config.window_decorations = "RESIZE"
config.window_padding = {
  left = 8,
  right = 8,
  top = 8,
  bottom = 8,
}

-- Tab bar (minimal, bottom, auto-hide)
config.enable_tab_bar = true
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = true
config.tab_max_width = 32

-- No audible bell
config.audible_bell = "Disabled"
config.visual_bell = {
  fade_in_function = "EaseIn",
  fade_in_duration_ms = 50,
  fade_out_function = "EaseOut",
  fade_out_duration_ms = 50,
}

-- WAYLAND OPTIMIZATIONS
config.enable_wayland = true
config.front_end = "WebGpu"
config.webgpu_power_preference = "HighPerformance"

-- SCROLLING & PERFORMANCE
config.scrollback_lines = 10000
config.enable_scroll_bar = false

-- KEYBINDINGS (Vim-style, matching Sway hjkl navigation)
config.keys = {
  -- Pane splits (Ctrl+Shift)
  { key = "Enter", mods = "CTRL|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
  { key = "\\", mods = "CTRL|SHIFT", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

  -- Pane navigation (Ctrl+Shift+hjkl)
  { key = "h", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Left") },
  { key = "j", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Down") },
  { key = "k", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Up") },
  { key = "l", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Right") },

  -- Pane resize (Alt+hjkl)
  { key = "h", mods = "ALT", action = act.AdjustPaneSize({ "Left", 3 }) },
  { key = "j", mods = "ALT", action = act.AdjustPaneSize({ "Down", 3 }) },
  { key = "k", mods = "ALT", action = act.AdjustPaneSize({ "Up", 3 }) },
  { key = "l", mods = "ALT", action = act.AdjustPaneSize({ "Right", 3 }) },

  -- Close pane
  { key = "w", mods = "CTRL|SHIFT", action = act.CloseCurrentPane({ confirm = true }) },

  -- Tab management
  { key = "t", mods = "CTRL|SHIFT", action = act.SpawnTab("CurrentPaneDomain") },
  { key = "[", mods = "CTRL|SHIFT", action = act.ActivateTabRelative(-1) },
  { key = "]", mods = "CTRL|SHIFT", action = act.ActivateTabRelative(1) },

  -- Copy/Paste
  { key = "c", mods = "CTRL|SHIFT", action = act.CopyTo("Clipboard") },
  { key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },

  -- Scroll
  { key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-1) },
  { key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(1) },

  -- Font size
  { key = "=", mods = "CTRL", action = act.IncreaseFontSize },
  { key = "-", mods = "CTRL", action = act.DecreaseFontSize },
  { key = "0", mods = "CTRL", action = act.ResetFontSize },

  -- Quick select mode (URLs, hashes, etc.)
  { key = "Space", mods = "CTRL|SHIFT", action = act.QuickSelect },

  -- Search
  { key = "f", mods = "CTRL|SHIFT", action = act.Search({ CaseInSensitiveString = "" }) },

  -- Toggle fullscreen
  { key = "F11", mods = "", action = act.ToggleFullScreen },
}

-- Tab number shortcuts (Ctrl+1-9)
for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = "CTRL",
    action = act.ActivateTab(i - 1),
  })
end

-- HYPERLINKS (Clickable URLs)
config.hyperlink_rules = wezterm.default_hyperlink_rules()

-- Add custom rules (file paths, etc.)
table.insert(config.hyperlink_rules, {
  regex = [[file://\S+]],
  format = "$0",
})

-- MISC
config.default_cursor_style = "SteadyBlock"
config.cursor_blink_rate = 0 -- No blinking
config.default_workspace = "main"
config.check_for_updates = false

return config
