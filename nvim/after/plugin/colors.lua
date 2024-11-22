function ColorMyPencils(color)
    --color = color or "handmade_theme"
    color = color or "onedark"
    vim.cmd.colorscheme(color)

    vim.api.nvim_set_hl(0, "Error",               {fg = "#FF0000", bold = true})
    vim.api.nvim_set_hl(0, "Todo",                {fg = "#FF0000", bold = true})
    vim.api.nvim_set_hl(0, "Warning",             {fg = "#FFFF00", bold = true})
    vim.api.nvim_set_hl(0, "DiagnosticError",     {fg = "#FF0000", bold = true})
    vim.api.nvim_set_hl(0, "DiagnosticWarn",      {fg = "#FFFF00", bold = true})
    vim.api.nvim_set_hl(0, "DiagnosticInfo",      {fg = "#40FF40"})
    vim.api.nvim_set_hl(0, "DiagnosticHint",      {fg = "#40FF40"})

    vim.api.nvim_set_hl(0, "CursorLine", {bg = "#000cff"})
    vim.cmd("highlight Cursor guibg=#00FF00")

--    vim.api.nvim_set_hl(0, "Normal", {bg = "#161616", fg = "#dab98f"})
--    vim.api.nvim_set_hl(0, "NormalFloat", {bg = "#161616"})
--    vim.api.nvim_set_hl(0, "Normal", {fg = "#dab98f"})
--    vim.api.nvim_set_hl(0, "NormalFloat", {bg = ""})

--    color = color or "github_dark_high_contrast"
end

local previous_bg_colors = {}

local function get_bg_color(group)
    local hl = vim.api.nvim_exec('hi ' .. group, true)
    return hl:match('guibg=(#%x+)') or 'NONE'  -- Return 'NONE' if no guibg is found
end

local function set_bg_color(group, color)
    vim.cmd('highlight ' .. group .. ' guibg=' .. color)
end

function ToggleTransparency()
    local groups = { "Normal", "NormalNC", "StatusLine", "VertSplit", "SignColumn", "EndOfBuffer" }

    if previous_bg_colors["Normal"] then
        for _, group in ipairs(groups) do
            set_bg_color(group, previous_bg_colors[group] or "#1e1e1e")  -- Fall back to default if no color is stored
        end
        previous_bg_colors = {}

        if vim.g.neovide then
            vim.g.neovide_transparency = 1
        end
    else
        for _, group in ipairs(groups) do
            previous_bg_colors[group] = get_bg_color(group)
            set_bg_color(group, "NONE")
        end

        if vim.g.neovide then
            vim.g.neovide_transparency = 0.8
        end
    end
end

vim.api.nvim_set_keymap('n', '<leader>tb', ':lua ToggleTransparency()<CR>', { noremap = true, silent = true })
ColorMyPencils()
