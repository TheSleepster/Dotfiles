 -- Lua
require('onedark').setup  {
    -- Main options --
    style = 'warm', -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
    transparent = false,  -- Show/hide background
    term_colors = true, -- Change terminal color as per the selected theme style
    ending_tildes = false, -- Show the end-of-buffer tildes. By default they are hidden
    cmp_itemkind_reverse = false, -- reverse item kind highlights in cmp menu

    -- toggle theme style ---
    toggle_style_key = nil, -- keybind to toggle theme style. Leave it nil to disable it, or set it to a string, for example "<leader>ts"
    toggle_style_list = {'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light'}, -- List of styles to toggle between

    -- Change code style ---
    -- Options are italic, bold, underline, none
    -- You can configure multiple style with comma separated, For e.g., keywords = 'italic,bold'
    code_style = {
        comments = 'none',
        keywords = 'none',
        functions = 'none',
        strings = 'none',
        variables = 'none'
    },

    -- Lualine options --
    lualine = {
        transparent = true, -- lualine center bar transparency
    },

    -- Custom Highlights --
    colors = {
        yellow = '#CD950C',
        purple = '#CD950C',
        cyan = '#dab98f',
        blue = '#ff5900',
        orange = '#CD950C',
        red = "#dab98f",
        pink = "#dab98f",
        light_blue = "#ffa006",
        weird_green = "#431eb6",
        fg =        "#dab98f",
        bg0 =       "#161616",
        grey =      "#5c6370",
        light_grey= "#dab98f",
        dark_cyan = "#63a3ff",
        dark_red =  "#63a3ff",
        dark_yellow="#63a3ff",
        dark_purple="#63a3ff",
    }, -- Override default colors
    highlights = {
        ["@keyword"] = {fg = '#CD950C'},
        ["@string"] = {fg = '#f4C430'},
        ["@string.escape"] = {fg = "#6b8e23"},
        ["@function"] = {fg = '#ff5900'},
        ["@function.builtin"] = {fg = '#ff5900'},
        ["@function.macro"] = {fg = '#dab98f'},
        ["@function.method"] = {fg = '#dab98f'},

        ["@keyword.operator"] = {fg = '#CD950C'},
        ["@constant.macro"] = {fg = '#CD950C'},
        ["@preproc"] = {fg = '#ff7a7b'},
        ["@include"] = {fg = '#ff7a7b'},
        ["@keyword.import"] = {fg = '#ff7a7b'},

        ["@lsp.type.namespace"] =  {fg = '#63A3FF'},

        ["@variable"] = {fg = '#dab98f'},
        ["@variable.parameter"] = {fg = '#dab98f'},
        ["@variable.member"] = {fg = '#dab98f'},
        ["@variable.builtin"] = {fg = '#cd950c'},

        ["@lsp.type.enumMember"] = {fg = '#1BFC06'},
        ["@lsp.type.number"] = {fg = '#63A3FF'},
        ["@lsp.type.interface"] = {fg = '#dab98f'},
        ["@lsp.type.keyword"] = {fg = '#cd950c'},
        ["@lsp.typemod.variable.static"] = {fg = '#cd950c'},
        ["@lsp.type.macro"] =  {fg = '#63A3FF'},


        Structure = {fg = '#CD950C'},
        TSConstant =  {fg = '#8806ce'},
        Typedef = {fg = '#8806ce'},
        Include = {fg = '#ff7a7b'},
        Define = {fg = '#ff7a7b'},

        CursorLine  = {bg = "#191970"},
        Cursor      = {bg = "#00fc5f"},
        Special     = {fg = "#f4c430"},
        String      = {fg = "#f4c430"},
        SpecialChar = {fg = "#f4c430"},

        ["@module"] = {fg = '#CD950C'},
        ["@property"] = {fg = '#dab98f'},
        ["@attribute"] = {fg = '#dab98f'},
        ["@type"] = {fg = '#CD950C'},
        ["@tag.attribute"] = {fg = '#CD950C'},
        ["@operator"] = {fg = "#FF0000"},
        ["@constant"] = {fg = "#00FF00"},
        ["@constructor"] = {fg = "#ff5900", bold = false},

        ["@lsp.type.struct"]     = {fg = "#CD950C"},   -- struct keyword
        ["@lsp.type.variable"]   = {fg = "#dab98f"},  -- variable (like Apples when used as type)
        ["@lsp.type.class"]      = {fg = "#CD950C"},  -- class/struct type
        ["@lsp.mod.declaration"] = {fg = "#dab98f"},  -- class/struct type
    }, -- Override highlight groups

    -- Plugins Config --
    diagnostics = {
        darker = false, -- darker colors for diagnostic
        undercurl = false,   -- use undercurl instead of underline for diagnostics
        background = false,    -- use background color for virtual text

        error = "#fc0505",
        hint  = "#41c101",
        info  = "#06dbcd",
        note  = "#41c101",
        todo  = "#06dbcd",
        warn  = "#dbc900",
    },
}
