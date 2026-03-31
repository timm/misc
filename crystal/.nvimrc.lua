-- ~/.config/nvim/init.lua
-- first load, needs :Lazy sync
--
-- ---- basics --------------------------------------------------------------
vim.g.mapleader = " "                  -- space as leader, set FIRST

vim.opt.number         = true
vim.opt.relativenumber = true
vim.opt.cursorline     = true
vim.opt.termguicolors  = true
vim.cmd("colorscheme lunaperche")
vim.cmd("syntax on")

-- ---- sanity --------------------------------------------------------------
vim.opt.ignorecase  = true             -- search case-insensitive...
vim.opt.smartcase   = true             -- ...unless you use capitals
vim.opt.clipboard   = "unnamedplus"    -- system clipboard
vim.opt.undofile    = true             -- persistent undo across sessions
vim.opt.scrolloff   = 8               -- keep 8 lines above/below cursor
vim.opt.updatetime  = 200              -- faster diagnostics

-- ---- keymaps -------------------------------------------------------------
local k = vim.keymap.set
k("i", "jk", "<Esc>")                 -- jk to escape insert
k("n", "<Esc>", ":noh<CR>")           -- clear search highlight
k("n", "<C-h>", "<C-w>h")            -- split nav: ctrl+hjkl
k("n", "<C-j>", "<C-w>j")
k("n", "<C-k>", "<C-w>k")
k("n", "<C-l>", "<C-w>l")
k("n", "<leader>w", ":w<CR>")         -- space-w to save
k("n", "<leader>q", ":q<CR>")
k("n", "<leader>e", ":Ex<CR>")        -- netrw explorer
k("v", "J", ":m '>+1<CR>gv=gv")      -- move selected lines down
k("v", "K", ":m '<-2<CR>gv=gv")      -- move selected lines up
k("n", "J",  "mzJ`z")                -- J keeps cursor in place

-- ---- crystal filetype ----------------------------------------------------
vim.api.nvim_create_autocmd({"BufRead","BufNewFile"}, {
  pattern  = "*.cr",
  callback = function() vim.bo.filetype = "crystal" end
})

-- ---- lazy bootstrap ------------------------------------------------------
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- ---- plugins -------------------------------------------------------------
require("lazy").setup({
  { "vim-crystal/vim-crystal" },

  -- fuzzy find everything (the single most-recommended nvim plugin)
  { "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local t = require("telescope.builtin")
      k("n", "<leader>f", t.find_files)   -- space-f: find files
      k("n", "<leader>g", t.live_grep)    -- space-g: grep
      k("n", "<leader>b", t.buffers)      -- space-b: open buffers
    end
  },

  { "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  config = function()
    local ok, ts = pcall(require, "nvim-treesitter.configs")
    if not ok then return end
    ts.setup({
      ensure_installed = { "lua", "python", "bash", "crystal" },
      highlight = { enable = true },
    })
  end
  },

  -- lsp: language smarts
  { "neovim/nvim-lspconfig",
    config = function()
      local lsp = require("lspconfig")
      -- add servers here, e.g.: lsp.pyright.setup({})
      k("n", "gd",  vim.lsp.buf.definition)
      k("n", "K",   vim.lsp.buf.hover)
      k("n", "<leader>r", vim.lsp.buf.rename)
      k("n", "<leader>a", vim.lsp.buf.code_action)
    end
  },

  -- auto close brackets/quotes
  { "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true
  },

  { "nyoom-engineering/oxocarbon.nvim",
  lazy = false, priority = 1000,
  config = function()
    vim.cmd("colorscheme oxocarbon")
  end
},


  { "folke/tokyonight.nvim",        -- clean blue, very popular
"catppuccin/nvim",               -- warm pastels, name = "catppuccin"
"rebelot/kanagawa.nvim",         -- muted japanese-ink palette
"EdenEast/nightfox.nvim",        -- several variants incl. dayfox
"rose-pine/neovim",              -- name = "rose-pine", soft pink/grey
"savq/melange-nvim",          -- warm dark, low saturation
    lazy = false, priority = 1000,   -- load first, before other plugins
    config = function()
      vim.cmd("colorscheme tokyonight")
    end
  },

})
