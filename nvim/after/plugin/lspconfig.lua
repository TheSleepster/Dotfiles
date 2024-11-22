require('mason').setup({})
require('mason-lspconfig').setup({
    ensure_installed = {
        "ols",
        "glsl_analyzer",
        "omnisharp",
        "gopls",
        "zls",
        "rust_analyzer",
        "lua_ls",
    },
    handlers = {
        function(server_name)
            require("lspconfig")[server_name].setup {}
        end,
        ["clangd"] = function() end,
    }
})

local opts = {noremap=true}

local on_attach = function(client, bufnr)
    -- Key mappings
    local opts = { buffer = bufnr }
    vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
    vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
    vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
    vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
    vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
    vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
    vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
    vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
    vim.keymap.set("i", "<C-K>", function() vim.lsp.buf.hover() end, opts)
    vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)

    vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
        vim.lsp.handlers['signature_help'], {
            border = 'single',
            close_events = {"BufHidden", "CursorMoved"},
        })

    if client.name == "clangd" then
        vim.diagnostic.config({ virtual_lines = false })
        vim.diagnostic.config({underline=false})
        client.server_capabilities.semanticTokensProvider = nil
    elseif client.name == "lua_ls" then
        vim.diagnostic.config({ virtual_lines = false })
        vim.diagnostic.config({underline=false})
        client.server_capabilities.semanticTokensProvider = nil
    end
end

-- Disable virtual_text since it's redundant due to lsp_lines.
vim.diagnostic.config({
  virtual_text = false,
})

require("lspconfig").clangd.setup {
    on_attach = on_attach,
    cmd = {
        "C:/Program Files/LLVM/bin/clangd",
        "--background-index",
        "--pch-storage=memory",
        "--all-scopes-completion",
        "--header-insertion=never",
        "-j=8",
        "--inlay-hints",
        "--header-insertion-decorators=0",
        "--function-arg-placeholders",
        "--completion-style=detailed",
    },
}

require("lspconfig").lua_ls.setup {
    on_attach = on_attach,
    settings = {
        Lua = {
            diagnostics = {
                globals = { "vim" },  -- Recognize 'vim' as a global variable
            },
            workspace = {
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,   
                    [vim.fn.stdpath("config") .. "/lua"] = true, 
                },
                checkThirdParty = false,  
            },
            telemetry = {
                enable = false,
            },
        },
    },
}

require("lspconfig").ols.setup {
    on_attach = on_attach
}

require("lspconfig").glsl_analyzer.setup {
    on_attach = on_attach,
    filetypes = {"glsl", "vert", "frag", "vs", "fs", "glh"},
}

require("lspconfig").lua_ls.setup {
    on_attach = on_attach,
}

require("lspconfig").omnisharp.setup {
    on_attach = on_attach
}

require("lspconfig").gopls.setup {
    on_attach = on_attach
}

require("lspconfig").zls.setup {
    on_attach = on_attach
}

require("lspconfig").rust_analyzer.setup {
    on_attach = on_attach
}

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local gdscript_config = {
    capabilities = capabilities,
    settings = {},
    cmd = {'ncat', 'localhost', os.getenv 'GDScript_Port' or '6005'},
}

vim.diagnostic.config({ virtual_lines = true })

require("lspconfig").gdscript.setup(gdscript_config)

-- define kind_icons array like above
local cmp_kinds = {
  Text = "",
  Method = "󰆧",
  Function = "",
  Constructor = "",
  Field = "󰇽",
  Variable = "󰂡",
  Class = "󰠱",
  Interface = "",
  Module = "",
  Property = "󰜢",
  Unit = "",
  Value = "󰎠",
  Enum = "",
  Keyword = "󰌋",
  Snippet = "",
  Color = "󰏘",
  File = "󰈙",
  Reference = "",
  Folder = "󰉋",
  EnumMember = "",
  Constant = "󰏿",
  Struct = "",
  Event = "",
  Operator = "󰆕",
  TypeParameter = "󰅲",
}

local cmp = require('cmp')
  cmp.setup({
    autocomplete = false,
    sources = {
        {name = 'nvim_lsp', keyword_length = 1, max_item_count = 10},
        {name = 'buffer', keyword_length = 5, max_tiem_count = 10},
        {name = 'path'},
    },

    snippet = {
        expand = function(args)
        vim.snippet.expand(args.body)
      end,
    },

    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
        ['<TAB>'] = cmp.mapping.select_next_item(select_opts),

        ['<C-u>'] = cmp.mapping.scroll_docs(-4),
        ['<C-d>'] = cmp.mapping.scroll_docs(4),

        ['<C-e>'] = cmp.mapping.abort(),
        ['<C-y>'] = cmp.mapping.confirm({select = true}),
        ['<CR>'] = cmp.mapping.confirm({select = false}),
        
        ['<Tab>'] = cmp.mapping(function(fallback)
          local col = vim.fn.col('.') - 1

          if cmp.visible() then
            cmp.select_next_item(select_opts)
          elseif col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
            fallback()
          else
            cmp.complete()
          end
        end, {'i', 's'}),

        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item(select_opts)
          else
            fallback()
          end
        end, {'i', 's'}),
    },

    window = {
        completion = cmp.config.window.bordered({
            border = 'rounded',
            winhighlight = "Normal:Normal,FloatBorder:Normal,Search:None",
            col_offset = -2,
            side_padding = 1,
        }),
        
        documentation = cmp.config.window.bordered({
            border = 'rounded',
            winhighlight = "Normal:Normal,FloatBorder:Normal,Search:None",
            col_offset = -2,
            side_padding = 1,
        }),
    },

    formatting = {
        format = function(entry, vim_item)
        local kind = vim_item.kind
        vim_item.kind = cmp_kinds[vim_item.kind] or "?"

        vim_item.menu = ({
            buffer = "[Buffer]",
            nvim_lsp = "[LSP]",
            nvim_lua = "[Lua]",
            latex_symbols = "[LaTeX]",
        })[entry.source.name]

        return vim_item
        end,
    },

    view = {
        -- custom
        entries = {name = 'wildmenu', seperator = '|'},
    },

    experimental = {
        ghost_text = false,
    },
})


