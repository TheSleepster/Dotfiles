vim.opt.guicursor = "a:block-Cursor"
vim.opt.guicursor = blinkon0

vim.opt.nu = false
vim.opt.relativenumber = false

vim.opt.wrap = false
vim.opt.fileformat=dos

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = "~/.undodir/"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "no"
vim.opt.isfname:append("@-@")

--vim.opt.colorcolumn = "80"

vim.opt.cindent = true          -- Enable C-style indentation
vim.opt.shiftwidth = 4          -- One indent level is 4 spaces
vim.opt.tabstop = 4             -- Tabs display as 4 spaces
vim.opt.expandtab = true        -- Use spaces instead of tabs
vim.opt.cinoptions = "(4"       -- Continuation lines indent 4 spaces from the line with '('

--shows a menu while using tab completion
vim.cmd("set wildmenu")

