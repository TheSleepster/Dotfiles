local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
    'navarasu/onedark.nvim',
    'vague2k/vague.nvim',
    { "rose-pine/neovim", name = "rose-pine" },
    { "projekt0n/github-nvim-theme"},

    'nvim-telescope/telescope.nvim', tag = '0.1.5',
    'mbbill/undotree',
    'nvim-tree/nvim-web-devicons',

    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig',
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-path',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-buffer',

    'onsails/lspkind.nvim',
    'ThePrimeagen/vim-be-good',
    'tpope/vim-commentary',
    'chaoren/vim-wordmotion',
    'rluba/jai.vim',
    -- LSP LINES
    -- {"https://git.sr.ht/~whynothugo/lsp_lines.nvim", config = function() require("lsp_lines").setup() end,},

    -- HARPOON
    {"ThePrimeagen/harpoon", branch = "harpoon2", dependencies = { "nvim-lua/plenary.nvim" },},

    -- LUALINE
    {'nvim-lualine/lualine.nvim', dependencies = { 'nvim-tree/nvim-web-devicons' }},

    -- I don't want this because it sucks but oh well
    'nvim-treesitter/nvim-treesitter',

    -- FLASH NVIM
    --{
    --  "folke/flash.nvim",
    --  event = "VeryLazy",
    --  ---@type Flash.Config
    --  opts = {},
    --  -- stylua: ignore
    --  keys = {
    --    { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
    --    { "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
    --    { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
    --    { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
    --    { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    --  },
    --},

    -- BLANK LINE
   --{
   --    "lukas-reineke/indent-blankline.nvim",
   --    main = "ibl",
   --    ---@module "ibl"
   --    ---@type ibl.config
   --    opts = {},
   --},

    {
        'MeanderingProgrammer/render-markdown.nvim',
        dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
        ---@module 'render-markdown'
        ---@type render.md.UserConfig
        opts = {},
    },
}

local opts = {}

require("lazy").setup(plugins, opt) 
