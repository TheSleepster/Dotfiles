local wezterm = require("wezterm")
local config  = wezterm.config_builder()
local act = wezterm.action

config.font = wezterm.font("LiterationMono Nerd Font Propo")
config.font_size = 11

config.enable_tab_bar = false
config.window_decorations = "RESIZE"

config.window_background_opacity = 0.9

config.front_end = "OpenGL"
config.max_fps = 240
config.animation_fps = 1

config.term = "xterm-256color"
config.cell_width = 0.9

config.webgpu_power_preference= "HighPerformance"
config.webgpu_force_fallback_adapter = true

config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}

config.keys = {
    {
        key = "h",
        mods = "CTRL|SHIFT|ALT",
        action = wezterm.action.SplitPane({
            direction = "Right",
            size = {Percent = 50},
        })
    },
    {
        key = "v",
        mods = "CTRL|SHIFT|ALT",
        action = wezterm.action.SplitPane({
            direction = "Down",
            size = {Percent = 50},
        })
    },
    {
        key = "U",
        mods = "CTRL|SHIFT",
        action = act.AdjustPaneSize({"Left", 5}),
    },
    {
        key = "I",
        mods = "CTRL|SHIFT",
        action = act.AdjustPaneSize({"Down", 5}),
    },
    {
        key = "O",
        mods = "CTRL|SHIFT",
        action = act.AdjustPaneSize({"Up", 5}),
    },
    {
        key = "P",
        mods = "CTRL|SHIFT",
        action = act.AdjustPaneSize({"Right", 5}),
    },

    {key = "w", mods = "CTRL|SHIFT", action = act.PaneSelect},
    {key = "q", mods = "CTRL", action = act.CloseCurrentPane {confirm = true}},
}

return config
