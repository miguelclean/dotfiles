" ============================
" Miguel's Fresh Neovim Config
" ============================
" 29th Apr 2021 - 2nd May 2023
"
" {{{ INSTALLATION
"
" 1. $ stow nvim
"       ./init.vim <- ~/.config/nvim/init.vim
"       ./plugin/  <- ~/.config/nvim/plugin/
"
" 2. install vim-plug (https://github.com/junegunn/vim-plug)
" 3. :PlugUpdate
" 4. :checkhealth (locale problem??)
"
" }}}

" {{{ TODO
"
" watch ThePrimeagen & GregHurrell
" split into mutiple files 
" :help treesitter
" consider/compare telescope.nvim or fzf-lua 
"
" }}}

" {{{ VIM-PLUG
call plug#begin('~/.vim/plugged')

" COLOR
Plug 'jnurmine/Zenburn'

" STATUSLINE
Plug 'nvim-lualine/lualine.nvim'

" TPOPE
Plug 'tpope/vim-commentary' " comment/uncomment
Plug 'tpope/vim-dadbod'     " database interface
Plug 'tpope/vim-dispatch'   " dispatch async
Plug 'tpope/vim-eunuch'     " common shell commands 
Plug 'tpope/vim-fugitive'   " git wrapper
Plug 'tpope/vim-sensible'   " reasonable defaults
Plug 'tpope/vim-surround'   " xml tags
Plug 'tpope/vim-unimpaired' " brackets
Plug 'tpope/vim-vinegar'    " enhance netrw

" auto-align
Plug 'godlygeek/tabular'   

" fuzzy finder
" consider telescope & fzf-lua instead below
Plug 'junegunn/fzf',                   { 'do': { -> fzf#install() }                }
Plug 'junegunn/fzf.vim'

" git diff symbols
Plug 'airblade/vim-gitgutter'

" undotree visualizer
Plug 'mbbill/undotree'

" browse tags
Plug 'majutsushi/tagbar'

" ??? need that?
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" navigate between splits (integrates with tmux)
Plug 'christoomey/vim-tmux-navigator' 

" select in visual mode and search with `*`
Plug 'nelstrom/vim-visual-star-search'

" LANGUAGE SERVER PROTOCOL
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/lsp-status.nvim'

" LANGUAGE SPECIFIC 
"
" Haskell
"check: https://wiki.haskell.org/Vim
"try: haskell-tools.nvim ?
Plug 'neovimhaskell/haskell-vim' " highlight and indent (hls instead?)
Plug 'alx741/vim-stylishask' " maybe let hls run this instead?
"Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }

" Agda 
Plug 'derekelkins/agda-vim' "this requires python2 / pip2 :(

call plug#end()
" }}}

" {{{ COLORS & FILETYPE
colorscheme zenburn

syntax on

hi DiffAdd    ctermfg=none ctermbg=23
hi DiffDelete ctermfg=none ctermbg=52
hi DiffChange ctermfg=none ctermbg=236
hi DiffText   ctermfg=none ctermbg=94

" detect type and autoload plugin and indent files
filetype plugin indent on
" }}}

" {{{ OPTIONS
set nocompatible
set cursorcolumn
set cursorline
set foldmethod=marker
set nowrap
set showcmd
set number
set relativenumber
set hidden
set colorcolumn=80
set backspace=2
set noswapfile
set nobackup
set undofile
set undodir=~/.vim/undodir
set noerrorbells
set updatetime=1000
set signcolumn=yes
set mouse=a
set scrolloff=8
set completeopt-=preview

" tabs
set listchars=tab:>.,trail:~
set list
set tabstop=8
set softtabstop=4
set expandtab
set shiftwidth=4
set shiftround

" folding
set foldcolumn=5
set foldlevelstart=0

" search
set hlsearch
set incsearch
set ignorecase
set smartcase

" match brackets
set showmatch
set matchtime=5

" }}}

" {{{ KEY BINDINGS
" mapleader
" use <space> but remap to _ so it is visible for 'showcmd'
nmap <space> _
let mapleader="\_"
let maplocalleader="\_"

" write current file as superuser
cmap w!! SudoWrite

" activate 'very magic' for searches automatically
nnoremap / /\v
nnoremap ? ?\v

" alt esc
inoremap jk <esc>
cnoremap jk <esc>

" esc in term
tnoremap <Esc> <C-\><C-n>

" easy editing and sourcing of vimrc
nnoremap <leader>ev :e $MYVIMRC<cr>
" nnoremap <leader>sv :source $MYVIMRC<cr>

" turn off search highlight
nnoremap <leader>h :nohlsearch<cr>

" layout
"nnoremap <leader>ln :NERDTreeToggle<cr>

" show list on multiple matches
nnoremap <C-]> g<C-]>

" misc
nmap <leader>q :call setqflist(filter(getqflist(),"v:val['type'] == 'E'"))<CR>

" ctrl-space auto complete in insert mode
inoremap <C-Space> <C-X><C-O>

" FZF settings
nmap <leader>f :Files<cr>|     " fuzzy find files in the working directory (where you launched Vim from)
nmap <leader>/ :BLines<cr>|    " fuzzy find lines in the current file
nmap <leader>b :Buffers<cr>|   " fuzzy find an open buffer
nmap <leader>a :Rg |           " fuzzy find text in the working directory
nmap <leader>c :Commands<cr>|  " fuzzy find Vim commands (like Ctrl-Shift-P in Sublime/Atom/VSC)
nmap <leader>t :Tags<cr>|      " fuzzy find tags

" haskell specific
augroup filetype_haskell
    autocmd!
    autocmd Filetype haskell nnoremap <buffer> <C-]>     :lua vim.lsp.buf.definition()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>i :lua vim.lsp.buf.hover()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>q :lua vim.lsp.buf.code_action()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>r :lua vim.lsp.buf.references()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>s :lua vim.lsp.buf.document_symbol()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>S :lua vim.lsp.buf.workspace_symbol()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>m :lua vim.diagnostic.open_float()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>e :lua vim.diagnostic.setloclist({open = true})<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>E :lua vim.diagnostic.setqflist({open = true})<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>l :lua vim.lsp.codelens.run()<CR>
    autocmd Filetype haskell nnoremap <buffer> <leader>L :lua vim.lsp.codelens.refresh()<CR>
    autocmd Filetype haskell autocmd InsertLeave <buffer> lua vim.lsp.codelens.refresh()
    " vim.lsp.buf.formatting()
    " vim.lsp.buf.rename() ?
    " print(vim.lsp.buf.server_ready())  
    " autocmd Filetype haskell nnoremap <buffer> <leader>x :call LanguageClient#explainErrorAtPoint()<CR>
    autocmd Filetype haskell setlocal softtabstop=2
    autocmd Filetype haskell setlocal shiftwidth=2
    autocmd Filetype haskell setlocal omnifunc=v:lua.vim.lsp.omnifunc
augroup END
" }}}

" {{{ PLUGINS CONFIG

let g:deoplete#enable_at_startup = 1

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

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

" {{{ lua
lua << END
local lsp_status = require('lsp-status')
lsp_status.register_progress()

require('lualine').setup
  { options = { icons_enabled = true
              , theme = 'auto'
              , component_separators = { left = '', right = ''}
              , section_separators = { left = '', right = ''}
              , disabled_filetypes = { statusline = {}, winbar = {}, }
              , ignore_focus = {}
              , always_divide_middle = true
              , globalstatus = false
              , refresh = { statusline = 1000, tabline = 1000, winbar = 1000, }
              }
  , sections = { lualine_a = {'mode'}
               , lualine_b = {'branch', 'diff', 'diagnostics'}
               , lualine_c = { "filename", 'data', "require'lsp-status'.status()" }
               , lualine_x = {'encoding', 'fileformat', 'filetype'}
               , lualine_y = {'progress'}
               , lualine_z = {'location'} }
  , inactive_sections = { lualine_a = {}
                        , lualine_b = {}
                        , lualine_c = {'filename'}
                        , lualine_x = {'location'}
                        , lualine_y = {}
                        , lualine_z = {} }
  , tabline = {}
  , winbar = {}
  , inactive_winbar = {}
  , extensions = {}
  }

require('lspconfig').hls.setup
  { autostart = true
    , cmd = { "haskell-language-server-wrapper", "--lsp" } 
    , settings = { haskell = { checkProject = true
                             }
                 }
  }
END
" }}}
" }}}
