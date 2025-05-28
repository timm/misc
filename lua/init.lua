-- Essential Neovim settings for a clean and efficient workflow
vim.opt.termguicolors = true       -- Enable true color support
vim.opt.tabstop = 2                -- Number of spaces a <Tab> counts for
vim.opt.shiftwidth = 2             -- Number of spaces to use for each step of (auto)indent
vim.opt.expandtab = true           -- Use spaces instead of tabs
vim.opt.softtabstop = 2            -- Number of spaces <BS> and <Tab> use in insert mode
vim.opt.number = true              -- Show current line number
vim.opt.relativenumber = true      -- Show relative line numbers
vim.opt.mouse = "a"                -- Enable mouse support in all modes
vim.opt.undofile = true            -- Enable persistent undo
vim.opt.undodir = os.getenv("HOME") .. "/.nvim/undodir" -- Directory for undo files (create this directory: mkdir -p ~/.nvim/undodir)
vim.opt.ignorecase = true          -- Ignore case in search patterns
vim.opt.smartcase = true           -- Override 'ignorecase' if search pattern contains uppercase
vim.opt.incsearch = true           -- Highlight matches as you type
vim.opt.hlsearch = true            -- Highlight all matches for the current search pattern
vim.opt.cursorline = true          -- Highlight the current line
vim.opt.swapfile = false           -- Do not create swap files
vim.opt.backup = false             -- Do not create backup files
vim.opt.errorbells = false         -- No sound effects for errors
vim.opt.visualbell = true          -- Use visual bell instead of sound
vim.opt.wildmenu = true            -- Enhanced command-line completion menu
vim.opt.wildmode = "list:longest,full" -- How wildmenu behaves
vim.opt.smartindent = true         -- Smart auto-indenting
vim.opt.wrap = false               -- Do not wrap lines
vim.opt.title = true               -- Set terminal title
vim.opt.clipboard = "unnamedplus"  -- Share clipboard with system (copy/paste to/from other applications)

-- Set leader key to spacebar (common and ergonomic choice)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Bootstrap lazy.nvim plugin manager
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

-- Load plugins using lazy.nvim
require("lazy").setup({
  -- Catppuccin colorscheme
  { "catppuccin/nvim", name = "catppuccin", priority = 1000 },

  -- Dependency for many UI plugins, including Noice.nvim
  { 'MunifTanjim/nui.nvim' },

  -- File explorer
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = false,
    config = function()
      require("nvim-tree").setup {}
    end,
  },

  -- Fuzzy finder
  {
    "nvim-telescope/telescope.nvim",
    branch = "master",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").setup {}
    end,
  },

  -- Git integration (shows diffs in sign column)
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup {}
    end,
  },

  -- Status line
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-tree.lua", "nvim-tree/nvim-web-devicons" }, -- Add web-devicons for icons
    config = function()
      require("lualine").setup {
        options = {
          theme = "auto", -- Automatically pick a theme, or set 'catppuccin'
        },
        sections = {
          lualine_x = { "filetype", "branch", "diff" }, -- Example: show filetype, git branch, and diffs
        },
        -- Configure the tabline to show buffers
        tabline = {
          lualine_a = { "buffers" }, -- Show buffers in the left section of the tabline
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = { "tabs" }, -- Show tabs in the right section (if you use Neovim tabs)
        },
      }
    end,
  },

  -- Commenting plugin
  {
    "preservim/nerdcommenter",
    lazy = false, -- Load this eagerly as it's a utility
  },

  -- UI for messages, cmdline, and popups (highly recommended for modern Neovim)
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      -- You can customize Noice here. For example, to use a popup for cmdline:
      -- cmdline = { view = "popup" },
      -- messages = { view = "mini" },
    },
    dependencies = {
      "MunifTanjim/nui.nvim", -- Crucial for Noice to function
      "rcarriga/nvim-notify", -- Required for Noice notifications
      "nvim-tree/nvim-web-devicons", -- Recommended for icons within Noice
    },
  },

  -- Nvim-Treesitter for advanced syntax highlighting and parsing
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "c", "cpp", "lua", "vim", "vimdoc", "query", "javascript", "typescript", "html", "css", "json", "yaml", "markdown" }, -- Add languages you use
        sync_install = false, -- Install parsers asynchronously
        auto_install = true,  -- Automatically install missing parsers
        highlight = {
          enable = true, -- Enable syntax highlighting
          additional_vim_regex_highlighting = false, -- Disable legacy regex highlighting
        },
        indent = {
          enable = true, -- Enable tree-sitter based indentation
        },
      })
    end,
  },

  -- Formatter
  {
    "stevearc/conform.nvim",
    lazy = false, -- Load conform eagerly so format-on-save works immediately
    opts = {},    -- Placeholder for conform options
  },
})

-- Load and setup Catppuccin colorscheme (ensure this is called after lazy.nvim setup)
require("catppuccin").setup({
  flavour = "mocha", -- Set your desired Catppuccin style
})
vim.cmd.colorscheme "catppuccin"

-- Keybindings for plugins (optional, but recommended for usability)
vim.keymap.set("n", "<C-n>", ":NvimTreeToggle<CR>", { desc = "Toggle NvimTree" })
vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
vim.keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { desc = "Live Grep" })
vim.keymap.set("n", "<leader>c", ":NERDCommenterComment<CR>", { desc = "Comment/Uncomment line(s)" }) -- For NERDCommenter

-- Minimal Neovim Tricks:

-- 1. Window Navigation with Ctrl + H/J/K/L
-- This allows you to quickly move between split windows using the standard HJKL keys
-- while holding down the Ctrl key, which is very efficient.
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move to lower window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move to upper window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right window" })

-- 2. Highlight on Yank
-- Briefly highlights the text that was just yanked (copied), providing visual feedback.
-- This uses an autocommand to trigger a highlight group for a short duration.
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch", -- Use the 'IncSearch' highlight group for visibility
      timeout = 200,         -- Highlight for 200 milliseconds
    })
  end,
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }), -- Create a dedicated augroup
})

-- 3. Buffer Navigation
-- Quick keybindings to move between open buffers.
vim.keymap.set("n", "[b", ":bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set("n", "]b", ":bnext<CR>", { desc = "Next buffer" })

-- 4. Clear search highlights
-- A simple keymap to clear search highlights with <leader>nh (no highlight).
vim.keymap.set("n", "<leader>nh", ":nohlsearch<CR>", { desc = "Clear search highlights" })

-- 5. Toggle relative number (useful for specific tasks)
-- Toggle between absolute and relative line numbers.
vim.keymap.set("n", "<leader>tn", ":set relativenumber!<CR>:set number!<CR>", { desc = "Toggle relative/absolute line numbers" })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  callback = function()
    vim.bo.tabstop = 2
    vim.bo.shiftwidth = 2
    vim.bo.expandtab = true
    vim.bo.softtabstop = 2
  end,
})


vim.api.nvim_create_autocmd("TermOpen", {
  pattern = "*",
  group = vim.api.nvim_create_augroup("UserTermNoModifiable", { clear = true }), -- Optional: Good practice to group autocommands
  callback = function()
    vim.opt_local.modifiable = false -- or vim.bo.modifiable = false
  end,
})
