-- Handmade Neovim theme (based on Casey Muratori's Handmade Hero Emacs theme)
local colors = {
    handmade_beige = "#cdaa7d",          -- burlywood3
    handmade_dark_blue = "#191970",      -- midnight blue
    handmade_dark_gray = "#161616",      -- dark gray
    handmade_dark_green = "#006400",     -- DarkGreen
    handmade_gold = "#b8860b",           -- DarkGoldenrod3
    handmade_light_beige = "#dab98f",    -- light beige
    handmade_light_gray = "#808080",     -- gray50
    handmade_light_green = "#40ff40",    -- light green
    handmade_olive = "#6b8e23",          -- olive drab
    handmade_red = "#ff0000",            -- Red
    handmade_yellow = "#ffff00"          -- Yellow
}

vim.api.nvim_command("highlight clear")
vim.api.nvim_command("syntax reset")
vim.g.colors_name = "handmade_theme"

-- SYNTAX
vim.api.nvim_set_hl(0, "Normal",              {fg = colors.handmade_beige, bg = "#0D0D0D"})
vim.api.nvim_set_hl(0, "NormalFloat",         {fg = colors.handmade_beige, bg = "#0D0D0D"})
vim.api.nvim_set_hl(0, "Comment",             {fg = colors.handmade_light_gray, italic = false})
vim.api.nvim_set_hl(0, "Constant",            {fg = colors.handmade_olive})
vim.api.nvim_set_hl(0, "Function",            {fg = colors.handmade_beige})
vim.api.nvim_set_hl(0, "Keyword",             {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "String",              {fg = colors.handmade_olive})
vim.api.nvim_set_hl(0, "Special",             {fg = colors.handmade_olive})
vim.api.nvim_set_hl(0, "Type",                {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "Variable",            {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "Const",               {fg = colors.handmade_gold, bold = true})
vim.api.nvim_set_hl(0, "Typedef",             {fg = colors.handmade_gold, bold = true})
vim.api.nvim_set_hl(0, "Statement",           {fg = colors.handmade_gold, bold = true})
vim.api.nvim_set_hl(0, "Identifier",          {fg = colors.handmade_gold, bold = true})
vim.api.nvim_set_hl(0, "Label",               {fg = colors.handmade_beige, bold = true})
vim.api.nvim_set_hl(0, "StorageClass",        {fg = colors.handmade_beige, bold = true})
vim.api.nvim_set_hl(0, "Macro",               {fg = colors.handmade_gold, bold = true})
vim.api.nvim_set_hl(0, "Structure",           {fg = colors.handmade_gold});
vim.api.nvim_set_hl(0, "Delimiter",           {fg = colors.handmade_light_beige});

-- ADDITIONAL SYNTAX GROUPS
vim.api.nvim_set_hl(0, "Identifier",          {fg = colors.handmade_beige})
vim.api.nvim_set_hl(0, "PreProc",             {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "Error",               {fg = colors.handmade_red, bold = true})
vim.api.nvim_set_hl(0, "Todo",                {fg = colors.handmade_red, bold = true})
vim.api.nvim_set_hl(0, "Warning",             {fg = colors.handmade_yellow, bold = true})

vim.api.nvim_set_hl(0, "DiagnosticError",     {fg = colors.handmade_red, bold = true})
vim.api.nvim_set_hl(0, "DiagnosticWarn",      {fg = colors.handmade_yellow, bold = true})
vim.api.nvim_set_hl(0, "DiagnosticInfo",      {fg = colors.handmade_light_green})
vim.api.nvim_set_hl(0, "DiagnosticHint",      {fg = colors.handmade_light_green})
vim.api.nvim_set_hl(0, "SignColumn",          {bg = colors.handmade_dark_gray})
vim.api.nvim_set_hl(0, "FloatTitle",          {fg = colors.handmade_beige, bg = colors.handmade_dark_gray})
vim.api.nvim_set_hl(0, "Pmenu",               {fg = colors.handmade_beige, bg = colors.handmade_dark_gray})
vim.api.nvim_set_hl(0, "PmenuSel",            {fg = colors.handmade_beige, bg = colors.handmade_dark_gray})
vim.api.nvim_set_hl(0, "StatusLine",          {fg = colors.handmade_beige, bg = colors.handmade_dark_gray})
vim.api.nvim_set_hl(0, "StatusLineNC",        {fg = colors.handmade_dark_gray, bg = colors.handmade_dark_gray})

-- CURSOR
vim.api.nvim_set_hl(0, "Cursor",              {bg = handmade_light_green})
vim.cmd("highlight Cursor guibg=#00FF00")

vim.api.nvim_set_hl(0, "CursorLine",          {bg = colors.handmade_dark_blue});
vim.api.nvim_set_hl(0, "Visual",              {bg = "#303030"})

vim.api.nvim_set_hl(0, "@lsp.type.struct",    {fg = colors.handmade_gold})   -- struct keyword
vim.api.nvim_set_hl(0, "@lsp.type.variable",  {fg = colors.handmade_beige})  -- variable (like Apples when used as type)
vim.api.nvim_set_hl(0, "@lsp.type.class",     {fg = colors.handmade_gold})   -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.mod.declaration",{fg = colors.handmade_beige})  -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.typemod.enumMember.readonly.cpp", {fg = colors.handmade_light_green}) 

-- Detailed highlighting for types and structs
vim.api.nvim_set_hl(0, "@type",               {fg = colors.handmade_beige})        -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@type.builtin.cpp",   {fg = colors.handmade_beige})        -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@type.definition",    {fg = colors.handmade_beige})        -- Type definitions (struct, class, etc.)
vim.api.nvim_set_hl(0, "@type.declaration",   {fg = colors.handmade_beige})        -- Type declarations (struct, class, etc.)

-- For fields and variables
vim.api.nvim_set_hl(0, "@field",              {fg = colors.handmade_beige})        -- Fields of a struct or class
vim.api.nvim_set_hl(0, "@variable",           {fg = colors.handmade_beige})        -- Normal variables
vim.api.nvim_set_hl(0, "@variable.builtin",   {fg = colors.handmade_blue})         -- Built-in variables like `self`, `this`

-- Function and method-specific highlights
vim.api.nvim_set_hl(0, "@function",           {fg = colors.handmade_beige})        -- Function definitions
vim.api.nvim_set_hl(0, "@method",             {fg = colors.handmade_beige})        -- Method definitions
vim.api.nvim_set_hl(0, "@function.call",      {fg = colors.handmade_gold})         -- Function or method calls

-- Parameters and operators
vim.api.nvim_set_hl(0, "@parameter",          {fg = colors.handmade_light_blue})   -- Function parameters
vim.api.nvim_set_hl(0, "@operator",           {fg = colors.handmade_beige})        -- Operators

-- Enums and constants
vim.api.nvim_set_hl(0, "@constant",           {fg = colors.handmade_light_green})  -- Constants or enum members
vim.api.nvim_set_hl(0, "@constant.builtin",   {fg = colors.handmade_light_green})  -- Built-in constants
vim.api.nvim_set_hl(0, "@constructor.cpp",    {fg = colors.handmade_gold})         -- Built-in constants

vim.api.nvim_set_hl(0, "cOperator",           {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "cStorageClass",       {fg = colors.handmade_gold, bold = false})
vim.api.nvim_set_hl(0, "cppOperator",         {fg = colors.handmade_gold})
vim.api.nvim_set_hl(0, "cCppOutIf",           {fg = handmage_beige})
vim.api.nvim_set_hl(0, "cCppOutIf2",          {fg = handmage_beige})
