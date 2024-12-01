local wezterm = require("wezterm")
local mux = wezterm.mux

wezterm.on("gui-startup", function()
  local _, _, window = mux.spawn_window {}
  window:gui_window():maximize()
end)

local default_prog
local font_size
if wezterm.target_triple == "x86_64-pc-windows-msvc" then
  default_prog = { "pwsh.exe", "-NoLogo" }
  font_size = 12.0
elseif wezterm.hostname() == "debian" then
  default_prog = { "nu" }
  font_size = 11.0
else
  default_prog = { "zsh" }
  font_size = 11.0
end

return {
  -- color_scheme = "Monokai Soda",
  -- color_scheme = "Dracula",
  color_scheme = "Gruvbox dark, hard (base16)",
  default_prog = default_prog,
  font_size = font_size,
  font = wezterm.font_with_fallback {
    { family = "FiraCode Nerd Font Mono", weight = "Regular", italic = false },
    { family = "Hack NFM", weight = "Regular", italic = false },
    { family = "Hasklug Nerd Font Mono", weight = "Regular", italic = false },
    { family = "CaskaydiaCove Nerd Font", weight = "Regular", italic = false },
  },
  launch_menu = {
    {
      args = { 'btop'},
    },
    {
      label = 'nushell',
      args = {'nu', '-l'}
    }
  },
  keys = {
    {
      key = "f",
      mods = "SHIFT|CTRL",
      action = wezterm.action.ToggleFullScreen,
    },
  },
  colors = {
    tab_bar = {
      -- The color of the inactive tab bar edge/divider
      inactive_tab_edge = "#575757",
    },
  },
  -- default_cursor_style = "BlinkingBar",
  -- cursor_blink_rate = 500,
  enable_kitty_graphics = true,
  window_decorations = "RESIZE",
  force_reverse_video_cursor = true,
  use_fancy_tab_bar = false,
  hide_tab_bar_if_only_one_tab = true,
  window_padding = {
    left = 1,
    right = 1,
    top = 1,
    bottom = 1,
  },
  enable_scroll_bar = false,
  native_macos_fullscreen_mode = false,
}
