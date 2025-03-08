return {
	'echasnovski/mini.files',
	version = false,
	opts = {
		-- Close Explorer after opening file with 'l'
		mappings = {
			go_in = 'L',
			go_in_plus = 'l',
		},
	},
	keys = {
		-- Make Space + f open file browser 
		{ "<leader>fo", function() require("mini.files").open() end, desc = "Open file browser" },
	},
	dependencies = {
		{ 'echasnovski/mini.icons', version = false },
	},
	lazy = false,
}
