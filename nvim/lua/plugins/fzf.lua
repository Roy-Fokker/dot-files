-- fzf file search and grep
--------------------------------------------------------------------------------
return {
	"ibhagwan/fzf-lua",
	-- optional for icon support
	-- dependencies = { "nvim-tree/nvim-web-devicons" },
	-- or if using mini.icons/mini.nvim
	dependencies = { "echasnovski/mini.icons" },
	opts = {},
	keys = {
		{ "<leader>ff", function() require('fzf-lua').files() end, desc = "Find files in current directory" },
		{ "<leader>fg", function() require('fzf-lua').live_grep() end, desc = "Find by grepping current directory" },
		{ "<leader>fc", function() require('fzf-lua').files({ cwd = vim.fn.stdpath("config") }) end, desc = "Find in NVIM config directory" },
		{ "<leader>fz", function() require('fzf-lua').builtin() end, desc = "Find from nvim builtin"},
		{ "<leader>f?", function() require('fzf-lua').helptags() end, desc = "Find from nvim help"},
		{ "<leader>fk", function() require('fzf-lua').keymaps() end, desc = "Find in Keymaps"},
		{ "<leader>fr", function() require('fzf-lua').resume() end, desc = "Resume Find"},
		{ "<leader>fb", function() require('fzf-lua').buffers() end, desc = "Find open buffer"},
	},
}
