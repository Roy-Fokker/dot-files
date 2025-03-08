-- Options Config
--------------------------------------------------------------------------------

local o = vim.opt
local g = vim.g

g.mapleader = " "
g.maplocalleader = " "

-- Show line number, relatively, and highlight 
o.number = true
o.relativenumber = true
o.cursorline = true

-- Add gutter line to left
o.signcolumn = "yes"

-- Tab settings, use tabs for indent, at 4 spaces
o.expandtab = false
o.shiftwidth = 4
o.tabstop = 4
o.smarttab = true
o.smartindent = true
o.autoindent = true

-- Mouse is allowed
o.mouse = 'a'

-- Show current mode in mode line
o.showmode = true

-- Use system clipboard
o.clipboard = 'unnamedplus'

-- Ignore case when searching, use smart case
o.ignorecase = true
o.smartcase = true

-- Show tab characters and trailing spaces, and non-breaking-spaces
o.list = true
o.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

-- Keep minimum 10 lines below current cursor location
o.scrolloff = 10

-- Highlight when yanking (copying) text
-- See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
	desc = 'Highlight when yanking (copying) text',
	group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

-- Add extra filetypes for C++
vim.filetype.add({
	extension = {
		ixx = 'cpp',
	},
})
