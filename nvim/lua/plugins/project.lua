-- Project folder detector
--------------------------------------------------------------------------------
return {
	"ahmedkhalf/project.nvim",
	init = function()
		require("project_nvim").setup {}
	end,
}
