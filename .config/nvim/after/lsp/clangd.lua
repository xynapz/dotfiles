return {
  cmd = { "clangd", "--compile-commands-dir=build" },
  filetypes = { "c", "cpp", "cc" },
  root_markers = { "CMakeLists.txt", ".git" },
}
