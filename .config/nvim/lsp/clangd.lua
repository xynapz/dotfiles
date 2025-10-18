return {
	cmd = { "clangd", "--compile-commands-dir=build" },
	filetypes = { "cpp", "h", "hpp" },
	root_markers = { "CMakeLists.txt", ".git" },
	-- capabilities = vim.tbl_deep_extend(
	--     "force",
	--     {},
	--     vim.lsp.protocol.make_client_capabilities(),
	--     blink.get_lsp_capabilities()
	-- ),
}
