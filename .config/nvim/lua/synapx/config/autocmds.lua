-- Autocommands
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Highlight on yank
augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
	group = "YankHighlight",
	callback = function()
		vim.highlight.on_yank({ higroup = "IncSearch", timeout = 150 })
	end,
})

-- Don't auto comment new lines
autocmd("FileType", {
	pattern = "*",
	command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o",
})

-- Remove whitespace on save
autocmd("BufWritePre", {
	pattern = "*",
	command = [[%s/\s\+$//e]],
})

-- only highlight when searching
autocmd("CmdlineEnter", {
	callback = function()
		local cmd = vim.v.event.cmdtype
		if cmd == "/" or cmd == "?" then
			vim.opt.hlsearch = true
		end
	end,
})

autocmd("CmdlineLeave", {
	callback = function()
		local cmd = vim.v.event.cmdtype
		if cmd == "/" or cmd == "?" then
			vim.opt.hlsearch = false
		end
	end,
})

-- Go to last location when opening a buffer
autocmd("BufReadPost", {
	callback = function()
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		if mark[1] > 0 and mark[1] <= vim.api.nvim_buf_line_count(0) then
			pcall(vim.api.nvim_win_set_cursor, 0, mark)
		end
	end,
})

-- run go file
autocmd("BufEnter", {
	pattern = { "*.go" },
	callback = function()
		vim.keymap.set("n", "<Leader>R", ":terminal go run %<CR>", { silent = true, desc = "Run Code" })
	end,
})

-- run py file
autocmd("BufEnter", {
	pattern = { "*.py" },
	callback = function()
		vim.keymap.set("n", "<Leader>R", ":terminal python3 %<CR>", { silent = true, desc = "Run Code" })
	end,
})

-- turn on spell check for markdown and text file
autocmd("BufEnter", {
	pattern = { "*.md" },
	callback = function()
		vim.opt_local.spell = true
	end,
})
