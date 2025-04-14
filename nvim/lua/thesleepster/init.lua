require("thesleepster.remap")
require("thesleepster.set")
require("thesleepster.lazy")

vim.g.netrw_browse_split = 0
vim.g.netrw_winsize = 25

-- function HighlightCompilerErrors()
--   local error_pattern = "([%w_\\/.]+)%((%d+)%)%):%s*(error|warning)%s*([%w_]+)%s*[:-]%s*(.*)"
  
--   vim.cmd("highlight ErrorMsg guifg=red ctermfg=red")
--   vim.cmd("highlight WarningMsg guifg=yellow ctermfg=yellow")
  
--   local bufnr = vim.api.nvim_get_current_buf()
--   local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

--   for _, line in ipairs(lines) do
--     local filename, lnum, level, error_code, msg = string.match(line, error_pattern)

--     if filename and lnum then
--       if level == "error" then
--         local match_id = vim.fn.matchadd('ErrorMsg', '\\%' .. _ .. 'l')
--       elseif level == "warning" then
--         local match_id = vim.fn.matchadd('WarningMsg', '\\%' .. _ .. 'l')
--       end
--     end
--   end
-- end
-- autocmd TermOpen * setlocal syntax=cpp
-- autocmd TermOpen * lua HighlightCompilerErrors()

vim.cmd[[

    syntax match TodoKeyword "\s*\zs\(TODO\|NOTE\|IMPORTANT\|HACK\|WARN\|WARNING\|FIX\)" contained
    highlight link TodoKeyword DiagnosticWarn

    syntax match AuthorKeyword "Sleepster"
    highlight link AuthorKeyword DiagnosticWarn
    highlight Cursor guifg=#00FF00 guibg=NONE

    augroup cursorline
        autocmd!
        autocmd WinEnter,BufEnter * lua vim.wo.cursorline = true
        autocmd WinLeave,BufLeave * lua vim.wo.cursorline = false
    augroup END
]]

-- GLSL STUFF
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
    pattern = {"*.vs", "*.fs", "*.vert", "*.frag", "*.glh"},
    callback = function()
        vim.bo.filetype = "glsl"
    end,
})

vim.api.nvim_create_user_command('SourceInit', function()
    vim.cmd(':so C:\\users\\ibjal\\AppData\\local\\nvim\\lua\\thesleepster\\init.lua')
end, {})

if vim.g.neovide then
    vim.o.guifont = "LiterationMono Nerd Font Propo:h11" -- text below applies for VimScript
    vim.g.neovide_refresh_rate = 144
    vim.g.neovide_scroll_animation_length = 0.0
    vim.g.neovide_cusor_animation_length = 0.0
    vim.g.neovide_cursor_short_animation_length = 0.0
    vim.g.neovide_cursor_trail_size = 0.0
    vim.g.neovide_cursor_trail_length = 0.0
    vim.g.neovide_cursor_antialiasing = true
    vim.g.neovide_fullscreen = true
    vim.g.neovide_cursor_animate_in_insert_mode = false
    vim.g.neovide_cursor_animate_command_line = false
    vim.g.neovide_position_animation_length  = 0;
    vim.g.neovide_scroll_animation_far_lines = 0;

    vim.api.nvim_set_keymap('v', '<sc-c>', '"+y', {noremap = true})
    vim.api.nvim_set_keymap('n', '<sc-v>', 'l"+P', {noremap = true})
    vim.api.nvim_set_keymap('v', '<sc-v>', '"+P', {noremap = true})
    vim.api.nvim_set_keymap('c', '<sc-v>', '<C-o>l<C-o>"+<C-o>P<C-o>l', {noremap = true})
    vim.api.nvim_set_keymap('i', '<sc-v>', '<ESC>l"+Pli', {noremap = true})
    vim.api.nvim_set_keymap('t', '<sc-v>', '<C-\\><C-n>"+Pi', {noremap = true})
end

ColorMyPencils()

