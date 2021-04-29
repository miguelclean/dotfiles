" Miguel's Fresh Vimrc "
" Started from scratch on 29th Apr 2021

" {{{ misc
colorscheme desert
set nocompatible
set foldmethod=marker
set nowrap
set showcmd
set number
set hidden
set colorcolumn=73
set backspace=2
" }}}

" {{{ tabs
set listchars=tab:>.
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

" alt esc in insert mode
inoremap jk <esc>

" toggle tabs visibility
noremap <leader>t :set invlist<CR>

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

" ctrl-space auto complete in insert mode
inoremap <C-Space> <C-N>
" }}}
