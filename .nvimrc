let g:ale_linters ={
      \   'haskell': ['hlint', 'hie'],
      \}

nnoremap ,day :-1read .snippets/day.hs<CR>8j3ela

let NERDTreeIgnore=['\.hi$', '\.o$']

" let g:gutentags_enabled = 0
let g:gutentags_project_info = []

call add(g:gutentags_project_info, { 'type': 'haskell', 'glob': '*.cabal' })

let g:gutentags_ctags_executable_haskell = './bin/tags'

au FileType haskell command! MakeTags execute "!echo ':ctags' | stack ghci"
