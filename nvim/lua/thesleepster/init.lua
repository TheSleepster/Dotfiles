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
    vim.g.neovide_scroll_animation_length = 0.0
    vim.g.neovide_cusor_animation_length = 0.0
    vim.g.neovide_cursor_trail_size = 0.0
    vim.g.neovide_cursor_trail_length = 0.0
    vim.g.neovide_cursor_antialiasing = true
end

-- INDENT STYLING
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- Indentation settings
vim.opt.smartindent = true  -- Enable smart indentation
vim.opt.autoindent = true   -- Copy indentation from the current line

-- Custom C/C++ style
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "c", "cpp" },
  callback = function()
    -- Comment formatting
    vim.bo.commentstring = "// %s"  -- Single-line comment style
    vim.bo.formatoptions = vim.bo.formatoptions
      :gsub("c", "") -- Disable comment auto-wrapping
      :gsub("o", "") -- Disable comment continuation on newlines

    -- Indentation rules
    vim.bo.cinoptions = table.concat({
      "L0",  -- No extra indentation for labels
      "g0",  -- Align braces with the opening statement
      "N-s", -- No indentation for `case` labels
      "p0",  -- Align braces at block level
      "t0",  -- No extra indentation for `case` label continuation
      "+4",  -- Indent blocks/statements by 4 spaces
      "w0",  -- No extra indentation for continued lines in `for` or `if`
      "c0",  -- No extra indentation for continuation lines
      "s0",  -- No extra indentation for switch blocks
    }, ",")

    -- Adjust `cinkeys` to avoid unwanted indentations
    vim.bo.cinkeys = vim.bo.cinkeys
      :gsub("{", "") -- Disable auto-indent after '{'
      :gsub(":", "") -- Disable auto-indent after ':'
      :gsub("=", "") -- Disable auto-indent after '='
  end,
})

ColorMyPencils()

