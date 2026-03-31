-- ~/.config/nvim/init.lua
-- first load, needs :Lazy sync
--
-- ---- basics --------------------------------------------------------------
vim.opt.number         = true
vim.opt.relativenumber = true
vim.opt.cursorline     = true
vim.opt.termguicolors  = true
vim.cmd("colorscheme lunaperche")
vim.cmd("syntax on")

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
})
