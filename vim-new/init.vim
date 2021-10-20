" Miguel's Fresh Neovim Config
" 29th Apr 2021 - 01st July 2021
"
" link from ~/.config/nvim/init.vim
" 1. install vim-plug
" 2. run :PlugUpdate
" 3. run :checkhealth

" TODO
" explore nvim 0.5+ native lsp-client

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-dispatch'

Plug 'junegunn/fzf',                   { 'do': { -> fzf#install() }                }
Plug 'junegunn/fzf.vim'

Plug 'airblade/vim-gitgutter'
Plug 'nelstrom/vim-visual-star-search'
Plug 'majutsushi/tagbar'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Plug 'rafi/awesome-vim-colorschemes'
Plug 'jnurmine/Zenburn'

" haskell related
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-stylishask'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }

Plug 'christoomey/vim-tmux-navigator'

call plug#end()
" }}}

" 256 colorscheme
colorscheme zenburn

" {{{ misc
set nocompatible
" set cursorcolumn
set cursorline
set foldmethod=marker
set nowrap
set showcmd
set number relativenumber
set hidden
set colorcolumn=73
set backspace=2
" }}}


" disable preview window on autocompletion
set completeopt-=preview

set updatetime=300
set signcolumn=yes:3

set mouse=a

syntax on

" detect type and autoload plugin and indent files
filetype plugin indent on

let g:deoplete#enable_at_startup = 1

let g:airline_theme='wombat'
let g:airline_powerline_fonts = 1

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
let g:LanguageClient_serverCommands = { 'haskell': ['haskell-language-server-wrapper', '--lsp'] }
let g:LanguageClient_codeLensDisplay = { "virtualTexthl": "SpellRare" }
let g:LanguageClient_diagnosticsDisplay =
\    {
\        1: {
\            "name": "Error",
\            "texthl": "LanguageClientError",
\            "signText": "x",
\            "signTexthl": "LanguageClientErrorSign",
\            "virtualTexthl": "SpellRare",
\        },
\        2: {
\            "name": "Warning",
\            "texthl": "LanguageClientError",
\            "signText": "!",
\            "signTexthl": "LanguageClientWarningSign",
\            "virtualTexthl": "SpellRare",
\        },
\        3: {
\            "name": "Information",
\            "texthl": "LanguageClientError",
\            "signText": "i",
\            "signTexthl": "LanguageClientInfoSign",
\            "virtualTexthl": "SpellRare",
\        },
\        4: {
\            "name": "Hint",
\            "texthl": "LanguageClientError",
\            "signText": ">",
\            "signTexthl": "LanguageClientInfoSign",
\            "virtualTexthl": "SpellRare",
\        },
\    }


let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" {{{ tabs
set listchars=tab:>.,trail:~
set list
set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
set shiftround
" }}}

" {{{ folding
set foldcolumn=5
set foldlevelstart=0
" }}}

" {{{ search
set hlsearch
set incsearch
set ignorecase
set smartcase
" }}}

" {{{ match brackets
set showmatch
set matchtime=5
" }}}

" {{{ KEY BINDINGS
" mapleader
" use <space> but remap to _ so it is visible for 'showcmd'
nmap <space> _
let mapleader="\_"
let maplocalleader="\_"

" get rid of bad habits. (might break other stuff eg. in cygwin)
" inoremap <esc> <nop>
" inoremap <Up> <nop>
" inoremap <Down> <nop>
" inoremap <Left> <nop>
" inoremap <Right> <nop>

" activate 'very magic' for searches automatically
nnoremap / /\v
nnoremap ? ?\v

" alt esc
inoremap jk <esc>
cnoremap jk <esc>

" easy editing and sourcing of vimrc
" nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :e $MYVIMRC<cr>

" turn off search highlight
nnoremap <leader>h :nohlsearch<cr>

" layout
"nnoremap <leader>ln :NERDTreeToggle<cr>

" silver search
"noremap <leader>a :Ack!<Space>

" show list if multiple ctrl-] matches
nnoremap <C-]> g<C-]>
augroup filetype_haskell
    autocmd!
    autocmd Filetype haskell nnoremap <C-]> :call LanguageClient#textDocument_definition()<CR>
    autocmd Filetype haskell nnoremap <leader>i :call LanguageClient#textDocument_hover()<CR>
    autocmd Filetype haskell nnoremap <leader>x :call LanguageClient#explainErrorAtPoint()<CR>
    autocmd Filetype haskell set softtabstop=2
    autocmd Filetype haskell set shiftwidth=2
augroup END

" ctrl-space auto complete in insert mode
inoremap <C-Space> <C-N>

" FZF settings
nmap <leader>f :Files<cr>|     " fuzzy find files in the working directory (where you launched Vim from)
nmap <leader>/ :BLines<cr>|    " fuzzy find lines in the current file
nmap <leader>b :Buffers<cr>|   " fuzzy find an open buffer
nmap <leader>a :Rg |           " fuzzy find text in the working directory
nmap <leader>c :Commands<cr>|  " fuzzy find Vim commands (like Ctrl-Shift-P in Sublime/Atom/VSC)
nmap <leader>t :Tags<cr>|      " fuzzy find tags

nmap <leader>q :call setqflist(filter(getqflist(),"v:val['type'] == 'E'"))
" }}}

let g:fzf_layout = { 'down': '~40%' }

let g:tagbar_type_haskell = {
    \ 'ctagsbin'    : 'hasktags',
    \ 'ctagsargs'   : '-x -c -o-',
    \ 'kinds'       : [
        \  'm:modules:0:1',
        \  'd:data:0:1',
        \  'd_gadt:data gadt:0:1',
        \  'nt:newtype:0:1',
        \  'c:classes:0:1',
        \  'i:instances:0:1',
        \  'cons:constructors:0:1',
        \  'c_gadt:constructor gadt:0:1',
        \  'c_a:constructor accessors:1:1',
        \  't:type names:0:1',
        \  'pt:pattern types:0:1',
        \  'pi:pattern implementations:0:1',
        \  'ft:function types:0:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'          : '.',
    \ 'kind2scope'   : {
        \ 'm'        : 'module',
        \ 'd'        : 'data',
        \ 'd_gadt'   : 'd_gadt',
        \ 'c_gadt'   : 'c_gadt',
        \ 'nt'       : 'newtype',
        \ 'cons'     : 'cons',
        \ 'c_a'      : 'accessor',
        \ 'c'        : 'class',
        \ 'i'        : 'instance'
    \ },
    \ 'scope2kind'   : {
        \ 'module'   : 'm',
        \ 'data'     : 'd',
        \ 'newtype'  : 'nt',
        \ 'cons'     : 'c_a',
        \ 'd_gadt'   : 'c_gadt',
        \ 'class'    : 'ft',
        \ 'instance' : 'ft'
    \ }
\ }
