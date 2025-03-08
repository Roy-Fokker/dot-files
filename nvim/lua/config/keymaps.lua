-- Keymap config
--------------------------------------------------------------------------------
local key = vim.keymap.set

-- define files group
key("n", "<leader>f", "", {desc = "Files"})

-- Tab/Shift+Tab to indent/dedent
key("v", "<Tab>", ">gv", {desc = "Indent selection"})
key("v", "<S-Tab>", "<gv", {desc = "Dedent selection"})
key("n", "<Tab>", "v><C-N>", {desc = "Indent selection"})
key("n", "<S-Tab>", "v<<C-N>", {desc = "Dedent selection"})

