set backupdir-=.
set backupdir^=~/tmp,/tmp
set nocompatible              
"filetype plugin indent on
set modelines=3
set scrolloff=3
set autoindent
set hidden "remember ls
set wildmenu
set wildmode=list:longest
set visualbell
set ttyfast
set backspace=indent,eol,start
set laststatus=2
set splitbelow
set paste
set mouse=a
set title
set number
autocmd BufEnter * cd %:p:h
set showmatch
set matchtime=15
set background=light
set syntax=on
syntax enable
set ignorecase
set incsearch
set smartcase
set showmatch
set hlsearch
set nofoldenable    " disable folding
set ruler
set statusline=%F\ \(%M\)\ %=%l\:%c\ 
set lispwords+=defthing   
set lispwords+=doitems    
set lispwords+=deftest
set lispwords+=defkeep
set lispwords+=labels
set lispwords+=labels
set lispwords+=doread
set lispwords+=while
set lispwords+=until
set path+=../**
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2 
end        
colorscheme default
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
map Z 1z=
set spell spelllang=en_us
set spellsuggest=fast,20 "Don't show too much suggestion for spell check
nn <F7> :setlocal spell! spell?<CR>
let g:vim_markdown_fenced_languages = ['awk=awk']
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/tmp/ish/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'airblade/vim-gitgutter'
Plugin 'itchyny/lightline.vim'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'jnurmine/Zenburn'
Plugin 'altercation/vim-colors-solarized'
Plugin 'seebi/dircolors-solarized'
Plugin 'nequo/vim-allomancer'
Plugin 'nanotech/jellybeans.vim'
Plugin 'vimwiki/vimwiki'
Plugin 'JuliaEditorSupport/julia-vim'
        "  All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
colorscheme jellybeans
map <C-o> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
hi Normal guibg=NONE ctermbg=NONE
hi NonText guibg=NONE ctermbg=NONE
        set fillchars=vert:\|
hi VertSplit cterm=NONE	