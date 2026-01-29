-- ~/.config/wezterm/wezterm.lua
-- WezTerm config - xz-nord theme integrated with Sway/Emacs workflow
local wezterm = require("wezterm")
local config = wezterm.config_builder()
local act = wezterm.action

-- COLORSCHEME (built-in themes)
-- Try: "Tokyo Night", "Catppuccin Mocha", "Dracula+", "Kanagawa (Gogh)", "Kanagawa Dragon (Gogh)", "rose-pine", "Nord", "Kolorit"
config.color_scheme = "Kanagawa Dragon (Gogh)"

-- FONT (Iosevka - ligatures forcefully disabled)
config.font = wezterm.font({
  family = "Iosevka",
  weight = "Light",  -- Thinner weight to match Foot
  harfbuzz_features = {
    "calt=0",  -- Contextual alternates
    "clig=0",  -- Contextual ligatures
    "liga=0",  -- Standard ligatures
    "dlig=0",  -- Discretionary ligatures
  },
})
config.font_size = 18.0
config.line_height = 1.0
config.cell_width = 1.0

-- Font rendering (Normal = thinner, crisper glyphs)
config.freetype_load_target = "Normal"
config.freetype_render_target = "Normal"

-- WINDOW APPEARANCE
config.window_background_opacity = 0.998
config.window_decorations = "NONE"  -- No title bar, no buttons
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
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
  { key = "|", mods = "CTRL|SHIFT", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

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
