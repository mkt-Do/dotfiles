execute pathogen#infect()
syntax enable

" colorschemeの設定
set background=dark
colorscheme solarized

" ---必要な機能---
" インデント
set autoindent
" タブ押したらスペースを入れる
set expandtab
" tabの大きさ
set tabstop=2
" <<と>>で入るtabの大きさ
set shiftwidth=2
" backspaceキーを有効に
set backspace=indent,eol,start
" 行数表示
set number
" 今何行目か表示
set ruler
" リアルタイム検索
set incsearch
" 検索のハイライト
set hlsearch
" 検索終わったら一番上に
set wrapscan
" 大文字小文字の区別しない
set ignorecase
" 小文字で検索のとき区別しない
set smartcase
" 括弧の強調
set showmatch
" 別のファイルに替えれる
set hidden

" ---不要な機能---
" 端で改行
"set nowrap

if 0 | endif

if has('vim_starting')
	if &compatible
		set nocompatible
	endif

	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

" ---NeoBundleでインストールするものを以下に記載---

NeoBundle 'scrooloose/nerdtree'
NeoBundle 'jistr/vim-nerdtree-tabs'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'othree/html5.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'posva/vim-vue'
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'leafgarland/typescript-vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'zchee/deoplete-go'
NeoBundle 'neomake/neomake'
NeoBundle 'elmcast/elm-vim'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'udalov/kotlin-vim'

"---ここまで---

call neobundle#end()

filetype plugin indent on

NeoBundleCheck

" ---プラグインの設定---

" NERDTreeをCtrl-eで起動
nnoremap <silent><C-e> :NERDTreeToggle<CR>

" ファイルが指定されていなければNERD treeを有効にする
if argc() == 0
  let g:nerdtree_tabs_open_on_console_startup = 1
end

autocmd BufRead,BufNewFile *.ts set filetype=typescript
