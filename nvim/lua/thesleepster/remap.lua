vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("n", "<leader>vwm", function()
    require("vim-with-me").StartVimWithMe()
end)
vim.keymap.set("n", "<leader>svwm", function()
    require("vim-with-me").StopVimWithMe()
end)

-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])

-- This is going to get me cancelled
vim.keymap.set("i", "<C-c>", "<Esc>")

-- Tmux
vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

-- My weird helper bindings
vim.keymap.set("n", "<leader>n", "i// NOTE(Sleepster): ")
vim.keymap.set("n", "<leader>t", "i// TODO(Sleepster): ")
vim.keymap.set("n", "<leader>f", "i// FIX(Sleepster): ")
vim.keymap.set("n", "<leader>i", "i// IMPORTANT(Sleepster): ")

vim.keymap.set("i", "<C-z>", " = {};")
vim.keymap.set("i", "<C-b>",  "{}<Left><Enter><Enter><Up><Tab>")

vim.keymap.set("i", "<C-s>", "{};<Left><Left><Enter><Enter><Up><Tab>")
vim.keymap.set("i", "<C-x>", "{}break;<Left><Left><Left><Left><Left><Left><Left><Enter><Enter><Up><Tab>")

-- Indent current line or selection with Shift+Tab
vim.keymap.set("n", "<S-Tab>", '==', { noremap = true, silent = true })
vim.keymap.set("v", "<S-Tab>", '=', { noremap = true, silent = true })

-- Shift + backspace emacs binding, sue me.
vim.keymap.set("i", "<S-BS>", "<C-w>", { noremap = true, silent = true })

-- LINE DUPLICATION
vim.keymap.set("n", "<C-d>", "yyp")

-- CUSTOM FUNCTION BINDS
vim.keymap.set("n", '<A-m>',     ':Build<CR>',         {noremap = true, silent = true})
vim.keymap.set("n", '<F12>',     ':DisplayOB<CR>',     {noremap = true, silent = true})
vim.keymap.set("n", '<S-F12>',   ':DisplaySB<CR>',     {noremap = true, silent = true})
vim.keymap.set("n", '<C-r>',     ':Run<CR>',           {noremap = true, silent = true})
vim.keymap.set("n", '<A-e>',     ':cnext<CR>',         {noremap = true, silent = true})
vim.keymap.set("n", '<A-l>',     ':cprev<CR>',         {noremap = true, silent = true})
vim.keymap.set("n", '<A-;>',     ':cfirst<CR>',        {noremap = true, silent = true})
vim.keymap.set("n", '<C-f>',      function() vim.api.nvim_feedkeys(":find ", "n", false) end, { noremap = true, silent = true })

vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)
