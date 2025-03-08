-- Kanagawa Theme
-------------------------------------------------------------------------------
return {
	"rebelot/kanagawa.nvim",
	opts = {
		compile = true,
		theme = "dragon",
	},
	build = function()
		vim.cmd("KanagawaCompile")
		vim.cmd("colorscheme kanagawa")
	end
}
