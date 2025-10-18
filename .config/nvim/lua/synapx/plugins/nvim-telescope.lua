return {
  {
    "nvim-telescope/telescope.nvim",
    enable = false,
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      { "nvim-telescope/telescope-ui-select.nvim" },
      { "nvim-telescope/telescope-project.nvim" },
    },
    event = "VeryLazy", -- Load earlier to ensure availability for extensions
    cmd = "Telescope",
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")

      -- Telescope setup (unchanged)
      telescope.setup({
        defaults = {
          prompt_prefix = "   ",
          selection_caret = " ",
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = { prompt_position = "top", preview_width = 0.55 },
            vertical = { mirror = false },
            width = 0.87,
            height = 0.80,
          },
          mappings = {
            i = {
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
              ["<esc>"] = actions.close,
            },
          },
          file_ignore_patterns = {
            "node_modules/",
            "%.git/",
            "%.cache/",
            "__pycache__/",
            "%.class",
            ".marks.md",
          },
        },
        pickers = {
          find_files = {
            hidden = true,
            follow = true,
          },
        },
        extensions = {
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
          ["ui-select"] = {
            require("telescope.themes").get_dropdown({}),
          },
          project = {
            base_dirs = {
              { path = "/home/angel/angel-dx/", max_depth = 3, hidden_files = true },
              { path = "/home/angel/sinopx/",   max_depth = 3, hidden_files = true },
              { path = "/home/angel/.config",   max_depth = 2 },
            },
            hidden_files = true,
          },
        },
      })

      -- Load Telescope extensions
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
      telescope.load_extension("project")
      pcall(telescope.load_extension, "trouble")

      -- Register keymaps with which-key
      local wk = require("which-key")
      wk.add({
        { "<leader>f",  group = "Find",               mode = "n" },
        {
          "<leader>ff",
          "<cmd>Telescope find_files<CR>",
          desc = "Find Files",
          mode = "n",
        },
        {
          "<leader>fg",
          "<cmd>Telescope live_grep<CR>",
          desc = "Live Grep",
          mode = "n",
        },
        {
          "<leader>fb",
          "<cmd>Telescope buffers<CR>",
          desc = "Buffers",
          mode = "n",
        },
        {
          "<leader>fh",
          "<cmd>Telescope help_tags<CR>",
          desc = "Help Tags",
          mode = "n",
        },
        {
          "<leader>fr",
          "<cmd>Telescope oldfiles<CR>",
          desc = "Recent Files",
          mode = "n",
        },
        {
          "<leader>fp",
          "<cmd>Telescope project<CR>",
          desc = "Projects",
          mode = "n",
        },
        { "<leader>fd", "<cmd>Telescope trouble<CR>", desc = "Find Diagnostics" },
        -- Include todo-comments keymaps under <leader>f for consistency
        {
          "<leader>ft",
          "<cmd>TodoTelescope<cr>",
          desc = "Find Todos",
          mode = "n",
        },
        {
          "<leader>fT",
          "<cmd>TodoTelescope keywords=TODO,FIX,FIXME<cr>",
          desc = "Find Todo/Fix/Fixme",
          mode = "n",
        },
      })
    end,
  },
}
