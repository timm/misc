-- init.lua (or require it from lua/plugins/prolog.lua)

-- Make sure Neovim recognizes Prolog
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.pl", "*.prolog"},
  callback = function()
    vim.bo.filetype = "prolog"
  end,
})

-- Replace default syntax with your custom rules
vim.api.nvim_create_autocmd("FileType", {
  pattern = "prolog",
  callback = function()
    -- Clear old syntax
    vim.cmd("syntax clear")

    -- Case sensitive
    vim.cmd("syntax case match")
    vim.cmd("syntax sync minlines=1 maxlines=0")

    ----------------------------------------------------------------------
    -- Numbers
    vim.cmd([[syntax match prologNumber '[-+]\=\<\d\+\>' contained]])
    vim.cmd([[syntax match prologNumber '[-+]\=\<\d\+\.\d\+\>' contained]])
    vim.cmd([[highlight link prologNumber Number]])

    -- Variables
    vim.cmd([[syntax match prologVariable '\<\(\u\|_\)\(\w\)*\>' contained]])
    vim.cmd([[highlight link prologVariable Identifier]])

    -- Atoms, strings, char codes
    vim.cmd([[syntax region prologAtom start=+'+ skip=+\\\\\|\\'+ end=+'+]])
    vim.cmd([[syntax region prologString start=+"+ skip=+\\\\\|\\"+ end=+"+]])
    vim.cmd([[syntax region prologCharCodes start=+`+ skip=+\\\\\|\\`+ end=+`+]])
    vim.cmd([[highlight link prologAtom Constant]])
    vim.cmd([[highlight link prologString String]])
    vim.cmd([[highlight link prologCharCodes String]])

    -- Comments
    vim.cmd([[syntax match prologLineComment "%.*" contains=@Spell]])
    vim.cmd([[syntax region prologCComment start="/\*" end="\*/" contains=@Spell]])
    vim.cmd([[highlight link prologLineComment Comment]])
    vim.cmd([[highlight link prologCComment Comment]])

    -- Operators
    vim.cmd([[syntax match prologOperator ":-" contained]])
    vim.cmd([[syntax match prologOperator "=\.\." contained]])
    vim.cmd([[syntax match prologOperator "=>" contained]])
    vim.cmd([[highlight link prologOperator Operator]])

    -- Special characters
    vim.cmd([[syntax match prologSpecialCharacter '[;!@^|]' contained]])
    vim.cmd([[highlight link prologSpecialCharacter Special]])

    -- Builtins (example subset, you can add more)
    vim.cmd([[syntax keyword prologBuiltin member append length sort]])
    vim.cmd([[highlight link prologBuiltin Keyword]])

    ----------------------------------------------------------------------
    -- Top-level rule head
    vim.cmd([[syntax match prologHead '^\zs[a-z][A-Za-z0-9_]*\ze\s*(' skipwhite]])
    vim.cmd([[highlight link prologHead Constant]])

    -- Rule ending
    vim.cmd([[syntax match prologEndingRule '\.\ze\s*\_%' ]])
    vim.cmd([[highlight link prologEndingRule Special]])

    ----------------------------------------------------------------------
    -- Mark syntax loaded
    vim.b.current_syntax = "prolog"
  end,
})

