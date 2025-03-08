-- NeoVim Config
-------------------------------------------------------------------------------
require "config.options"                        -- Neovim editor settings
require "config.lazy"                           -- Lazy.nvim and Plugins
vim.cmd[[colorscheme kanagawa]]                 -- Set the colorscheme
require "config.keymaps"                        -- Set the keymaps

