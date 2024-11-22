-- Naysayer Neovim theme (Based on Johnathon Blow's Emacs Theme)
local colors = {
    naysayer_builtin   = "#FFFFFF",
    naysayer_bg        = "#161616",
    naysayer_text      = "#D1B897",
    naysayer_select    = "#0000FF",
    naysayer_comment   = "#44b340",
    naysayer_punc      = "#8CDE94",
    naysayer_keyword   = "#FFFFFF",
    naysayer_variable  = "#C1D1E3",
    naysayer_function  = "#FFFFFF",
    naysayer_methods   = "#C1D1E3",
    naysayer_strings   = "#2EC09C",
    naysayer_constants = "#7AD0C6",
    naysayer_macros    = "#8CDE94",
    naysayer_numbers   = "#7AD0C6",

    naysayer_red       = "#FF0000",
    naysayer_yellow    = "#FFFF00",
    naysayer_blue      = "#A1EFE4",
    naysayer_green     = "#A6E22E",
    naysayer_white     = "#FFFFFF",
}

vim.api.nvim_command("highlight clear")
vim.api.nvim_command("syntax reset")
vim.g.colors_name = "naysayer_theme"

-- SYNTAX
vim.api.nvim_set_hl(0, "Normal",              {fg = colors.naysayer_text, bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "NormalFloat",         {fg = colors.naysayer_text, bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "Comment",             {fg = colors.naysayer_comment, italic = true})
vim.api.nvim_set_hl(0, "Constant",            {fg = colors.naysayer_constants})
vim.api.nvim_set_hl(0, "Function",            {fg = colors.naysayer_function})
vim.api.nvim_set_hl(0, "Keyword",             {fg = colors.naysayer_keyword})
vim.api.nvim_set_hl(0, "String",              {fg = colors.naysayer_strings})
vim.api.nvim_set_hl(0, "Special",             {fg = colors.naysayer_strings})
vim.api.nvim_set_hl(0, "Type",                {fg = colors.naysayer_punc})
vim.api.nvim_set_hl(0, "Variable",            {fg = colors.naysayer_text})
vim.api.nvim_set_hl(0, "Const",               {fg = colors.naysayer_constants, bold = true})
vim.api.nvim_set_hl(0, "Typedef",             {fg = colors.naysayer_constants, bold = true})
vim.api.nvim_set_hl(0, "Statement",           {fg = colors.naysayer_builtin, bold = true})
vim.api.nvim_set_hl(0, "Identifier",          {fg = colors.naysayer_text, bold = true})
vim.api.nvim_set_hl(0, "Label",               {fg = colors.naysayer_text, bold = true})
vim.api.nvim_set_hl(0, "StorageClass",        {fg = colors.naysayer_builtin, bold = true})
vim.api.nvim_set_hl(0, "Macro",               {fg = colors.naysayer_macros, bold = true})
vim.api.nvim_set_hl(0, "Structure",           {fg = colors.naysayer_builtin})
vim.api.nvim_set_hl(0, "Delimiter",           {fg = colors.naysayer_numbers})

-- ADDITIONAL SYNTAX GROUPS
vim.api.nvim_set_hl(0, "Identifier",          {fg = colors.naysayer_text})
vim.api.nvim_set_hl(0, "PreProc",             {fg = colors.naysayer_marcos})
vim.api.nvim_set_hl(0, "Error",               {fg = colors.naysayer_red, bold = true})
vim.api.nvim_set_hl(0, "Warning",             {fg = colors.naysayer_yellow, bold = true})
vim.api.nvim_set_hl(0, "Todo",                {fg = colors.naysayer_red, bold = true})

vim.api.nvim_set_hl(0, "DiagnosticError",     {fg = colors.naysayer_red,    bold = true})
vim.api.nvim_set_hl(0, "DiagnosticWarn",      {fg = colors.naysayer_yellow, bold = true})
vim.api.nvim_set_hl(0, "DiagnosticInfo",      {fg = colors.naysayer_green})
vim.api.nvim_set_hl(0, "DiagnosticHint",      {fg = colors.naysayer_green})
vim.api.nvim_set_hl(0, "SignColumn",          {bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "FloatTitle",          {fg = colors.naysayer_text,   bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "Pmenu",               {fg = colors.naysayer_text,   bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "PmenuSel",            {fg = colors.naysayer_text,   bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "StatusLine",          {fg = colors.naysayer_text,   bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "StatusLineNC",        {fg = colors.naysayer_bg,     bg = colors.naysayer_bg})

-- CURSOR
vim.api.nvim_set_hl(0, "Cursor",              {bg = colors.naysayer_white})
vim.cmd("highlight Cursor guibg=colors.naysayer_white")

vim.api.nvim_set_hl(0, "CursorLine",          {bg = colors.naysayer_bg})
vim.api.nvim_set_hl(0, "Visual",              {bg = "#303030"})

vim.api.nvim_set_hl(0, "@lsp.type.struct",    {fg = colors.naysayer_macros})     -- struct keyword
vim.api.nvim_set_hl(0, "@lsp.type.variable",  {fg = colors.naysayer_text})       -- variable (like Apples when used as type)
vim.api.nvim_set_hl(0, "@lsp.type.class",     {fg = colors.naysayer_macros})     -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.mod.declaration",                 {fg = colors.naysayer_builtin})     -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.mod.definition",                  {fg = colors.naysayer_macros})      -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.mod.classScope.cpp",              {fg = colors.naysayer_builtin})     -- class/struct type
vim.api.nvim_set_hl(0, "@lsp.typemod.enumMember.readonly.cpp", {fg = colors.naysayer_constants}) 
vim.api.nvim_set_hl(0, "@lsp.typemod.enum.declaration.cpp",    {fg = colors.naysayer_macros})    
vim.api.nvim_set_hl(0, "@lsp.typemod.class.declaration.cpp",   {fg = colors.naysayer_macros})    
vim.api.nvim_set_hl(0, "@lsp.typemod.macro.globalScope.cpp",   {fg = colors.naysayer_text})      
vim.api.nvim_set_hl(0, "@lsp.typemod.macro.declaration.cpp",   {fg = colors.naysayer_builtin})      

vim.api.nvim_set_hl(0, "cPreCondit",           {fg = colors.naysayer_macros,    bold = true})
vim.api.nvim_set_hl(0, "cInclude",             {fg = colors.naysayer_macros,    bold = true})
vim.api.nvim_set_hl(0, "cParen",               {fg = colors.naysayer_constants, bold = true})
vim.api.nvim_set_hl(0, "cBracket",             {fg = colors.naysayer_constants, bold = true})
vim.api.nvim_set_hl(0, "cBlock",               {fg = colors.naysayer_constants, bold = true})
vim.api.nvim_set_hl(0, "cppModifier",          {fg = colors.naysayer_builtin,   bold = true})

-- Detailed highlighting for types and structs

vim.api.nvim_set_hl(0, "@type",               {fg = colors.naysayer_punc})           -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@type.builtin.cpp",   {fg = colors.naysayer_macros})        -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@type.definition",    {fg = colors.naysayer_builtin})        -- Type definitions (struct, class, etc.)
vim.api.nvim_set_hl(0, "@type.declaration",   {fg = colors.naysayer_builtin})        -- Type declarations (struct, class, etc.)

-- For fields and variables
vim.api.nvim_set_hl(0, "@field",              {fg = colors.naysayer_builtin})        -- Fields of a struct or class
vim.api.nvim_set_hl(0, "@variable",           {fg = colors.naysayer_builtin})        -- Normal variables
vim.api.nvim_set_hl(0, "@variable.builtin",   {fg = colors.naysayer_builtin})        -- Built-in variables like `self`, `this`
vim.api.nvim_set_hl(0, "@variable.member.cpp",{fg = colors.naysayer_builtin})        -- Built-in constants

-- Function and method-specific highlights
vim.api.nvim_set_hl(0, "@function",           {fg = colors.naysayer_text})           -- Function definitions
vim.api.nvim_set_hl(0, "@method",             {fg = colors.naysayer_text})           -- Method definitions
vim.api.nvim_set_hl(0, "@function.call",      {fg = colors.naysayer_text})           -- Function or method calls

-- Parameters and operators
vim.api.nvim_set_hl(0, "@parameter",          {fg = colors.naysayer_punc})           -- Function parameters
vim.api.nvim_set_hl(0, "@property.cpp",       {fg = colors.naysayer_builtin})        -- Built-in constants
vim.api.nvim_set_hl(0, "@operator",           {fg = colors.naysayer_text})           -- Operators

-- Enums and constants
vim.api.nvim_set_hl(0, "@constant",           {fg = colors.naysayer_macros})         -- Constants or enum members
vim.api.nvim_set_hl(0, "@constant.builtin",   {fg = colors.naysayer_macros})         -- Built-in constants
vim.api.nvim_set_hl(0, "@constructor.cpp",    {fg = colors.naysayer_text})           -- Built-in constants

vim.api.nvim_set_hl(0, "@keyword.directive.define.cpp", {fg = colors.naysayer_macros})           -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@keyword.directive.cpp",        {fg = colors.naysayer_macros})           -- Types (struct, class, enum)
vim.api.nvim_set_hl(0, "@keyword.import.cpp",           {fg = colors.naysayer_macros})           -- Types (struct, class, enum)


