" dein scripts
if &compatible
  set nocompatible " Be iMproved
endif

" reset augroup
augroup MyAutoCmd
  autocmd!
augroup END

" automatic function to update $PATH
" add $PATH if specified path doesn't exist in $PATH
function! IncludePath(path)
  " define delimiter depends on platform
  if has('win16') || has('win32') || has('win64')
    let delimiter = ";"
  else
    let delimiter = ":"
  endif
  let pathlist = split($PATH, delimiter)
  if isdirectory(a:path) && index(pathlist, a:path) == -1
    let $PATH=a:path.delimiter.$PATH
  endif
endfunction

" add ~/.pyenv/shims into $PATH
" because python cannot be loaded correctly without this configurations
call IncludePath(expand("~/.anyenv/envs/pyenv/shims"))

" Required:
let s:cache_home = empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME
let s:dein_dir = s:cache_home . '/dein'
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
let s:iceberg_dir = s:dein_dir . '/repos/github.com/cocopon/iceberg.vim'
if !isdirectory(s:dein_repo_dir)
  call system('git clone https://github.com/Shougo/dein.vim ' . shellescape(s:dein_repo_dir))
endif
let &runtimepath = s:dein_repo_dir .",". &runtimepath .",". s:iceberg_dir

let s:toml_file = '~/.dein.toml'
let s:lazy_toml_file = '~/.dein_lazy.toml'

" Required:
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  call dein#load_toml(s:toml_file)
  call dein#load_toml(s:lazy_toml_file, {'lazy': 1})

  " Required:
  call dein#source('iceberg')
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable
au BufRead, BufNewFile *.md set filetype=markdown

" If you want to install not installed plugins on startup.
if has('vim_starting') && dein#check_install()
  call dein#install()
endif

" configure colorscheme
set background=dark
colorscheme iceberg


" file encoding
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis

" configurations
" indent
set autoindent
" insert 2 spaces when press tab key
set expandtab
" tab size = 2
set tabstop=2
" tab size when press << or >>
set shiftwidth=2
" enable backspace key
set backspace=indent,eol,start
" display line number
set number
" display current line number
set ruler
" incremental search
set incsearch
" highlighting search keyword
set hlsearch
" return top after searching till bottom
set wrapscan
" ingore Upper case and Lower case
set ignorecase
set smartcase
" emphasize brackets
set showmatch
" enable to open other file when not saving
set hidden
" some servers have issues with backup file (coc.nvim)
set nobackup
set nowritebackup

"----------------------------------------------------------
" NERDTree
"----------------------------------------------------------
" run NERDTree by Ctrl + e
nnoremap <silent><C-e> :NERDTreeToggle<CR>

"----------------------------------------------------------
" airline
"----------------------------------------------------------
let g:airline_theme = 'iceberg'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

"---------------------------------------------------------
" Dart Vim Plugins
"---------------------------------------------------------
let dart_html_in_string=v:true
let g:dart_style_guide=2
let g:dart_format_on_save=1
"let g:dartfmt_options

"---------------------------------------------------------
" coc nvim
"---------------------------------------------------------
" mark when error or warning happens
set cmdheight=2
set updatetime=300
" always show signcolumn, otherwise it would shift the text each time
if has("patch-8.1.1564")
  "recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif
highlight CocErrorSign ctermfg=15 ctermbg=196
highlight CocWarningSign ctermfg=0 ctermbg=172
nmap <silent> <space><space> :<C-u>CocList<CR>
nmap <silent> <space>h :<C-u>call CocAction('doHover')<CR>
nmap <silent> <space>df <Plug>(coc-definition)
nmap <silent> <space>rf <Plug>(coc-references)
nmap <silent> <space>rn <Plug>(coc-rename)
nmap <silent> <space>fmt <Plug>(coc-format)

" OCaml (merlin)
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
set rtp^="/Users/makoto.ishizaki/.opam/default/share/ocp-indent/vim"
