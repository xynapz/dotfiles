local vim_map = vim.keymap.set

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Move lines
vim.api.nvim_set_keymap("n", "<A-j>", "<cmd>m .+1<CR>==", { desc = "Move line down" })
vim.api.nvim_set_keymap("n", "<A-k>", "<cmd>m .-2<CR>==", { desc = "Move line up" })
vim.api.nvim_set_keymap("i", "<A-j>", "<Esc><cmd>m .+1<CR>==gi", { desc = "Move line down" })
vim.api.nvim_set_keymap("i", "<A-k>", "<Esc><cmd>m .-2<CR>==gi", { desc = "Move line up" })
vim.api.nvim_set_keymap("v", "<A-j>", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.api.nvim_set_keymap("v", "<A-k>", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

vim_map("n", "j", "gj", { noremap = true, silent = true })
vim_map("n", "k", "gk", { noremap = true, silent = true })
vim_map("v", "j", "gj", { noremap = true, silent = true })
vim_map("v", "k", "gk", { noremap = true, silent = true })


local function disable_key(mode, key)
  vim.api.nvim_set_keymap(mode, key, "<Nop>", { noremap = true, silent = true })
end

-- Disabled keys
for _, mode in ipairs({ "n", "i", "v" }) do
  disable_key(mode, "<Up>")
  disable_key(mode, "<Down>")
  disable_key(mode, "<Left>")
  disable_key(mode, "<Right>")
  -- disable_key(mode, "<Esc>") -- disable esc too
end

local keymap = vim.keymap.set

-- Window navigation
keymap('n', '<C-h>', '<C-w>h')
keymap('n', '<C-j>', '<C-w>j')
keymap('n', '<C-k>', '<C-w>k')
keymap('n', '<C-l>', '<C-w>l')

-- Window resizing
keymap('n', '<C-Up>', '<cmd>resize +2<CR>')
keymap('n', '<C-Down>', '<cmd>resize -2<CR>')
keymap('n', '<C-Left>', '<cmd>vertical resize -2<CR>')
keymap('n', '<C-Right>', '<cmd>vertical resize +2<CR>')

-- Split management
keymap('n', '<leader>sv', '<cmd>vsplit<CR>')
keymap('n', '<leader>sh', '<cmd>split<CR>')
keymap('n', '<leader>se', '<C-w>=')
keymap('n', '<leader>sx', '<cmd>close<CR>')
