" Best mappings since sliced bread
nmap <up> <nop>
nmap <down> <nop>
nmap <left> <nop>
nmap <right> <nop>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Use display lines with j and k
nmap j gj
nmap k gk

" Pathogen and plugin config
call pathogen#infect()
syntax on
filetype indent plugin on

" Basic configuration
set nocompatible
set isk+=-
set scrolloff=5
set autoread
set showcmd
set showmode
set ruler
set autowrite
set title
set hidden
set wildmenu
set wildmode=longest,list
set completeopt=menuone,longest,preview
set wildignore=*$py.class,*.pyc
set history=2000
set directory=/tmp/
set vb t_vb=
set tags=tags
colorscheme solarized
let mapleader = ","

set hlsearch incsearch showmatch
set ignorecase smartcase
set gdefault

set formatoptions=crqn
set textwidth=78
set colorcolumn=79

" Highlight EOL whitespace and too long lines
match SpellBad /\s\+$/

" Remove EOL whitepsace when saving
autocmd BufWritePre * :%s/\s\+$//e
autocmd FocusLost * :wa

" Keyboard mappings
nmap Y y$

nmap <F2> :set invspell<CR>
nmap <F12> :!ctags -R .<cr>
nmap <leader><space> :noh<CR>
nmap <leader>, :buffers<CR>:buffer<Space>
nmap <leader>d :buffers<CR>:bdelete<Space>

" Ctrl-Left, Ctrl-Right for jumping to and from tags
map <silent><C-Left> <C-T>
map <silent><C-Right> <C-]>

" Left. Alt to switch buffers
map <silent><Right> :bnext<CR>
map <silent><Left> :bprevious<CR>

function! ConfigureTab(tabwidth)
    set expandtab
    let &tabstop=a:tabwidth
    let &shiftwidth=a:tabwidth
    let &softtabstop=a:tabwidth
endfunction

" Usually, expanding and replacing tab with 4 spaces is fine
:call ConfigureTab(4)

" Autoindent and completion function for Python files.
autocmd BufRead,BufNewFile,BufEnter *.py call ConfigureTab(4)
autocmd BufRead,BufNewFile,BufEnter *.py set ai
    \ cinwords=if,elif,else,for,while,try,except,finally,def,class
    \ omnifunc=pythoncomplete#Complete

" Tab Separated Values kind of do not like tabs to be expanded
autocmd BufRead,BufNewFile,BufEnter *.tsv set noexpandtab

" In different markup languages, tabwidth should be 2
autocmd BufRead,BufNewFile,BufEnter *.*ml call ConfigureTab(2)

" Ruby style, also for buildfile
autocmd BufRead,BufNewFile,BufEnter *.rb call ConfigureTab(2)
autocmd BufRead,BufNewFile,BufEnter,BufWinEnter buildfile call ConfigureTab(2)
autocmd BufRead,BufNewFile,BufEnter buildfile :set syntax=ruby

" Github insists on having .rest extension for .rst files!
autocmd BufRead,BufNewFile,BufEnter *.rest :set filetype=rst

let g:vimclojure#ParenRainbow = 1
"
" Haskell mode config
autocmd BufRead,BufNewFile,BufEnter *.*hs call ConfigureTab(4)
autocmd BufRead,BufNewFile,BufEnter *.hs compiler ghc
let g:haddock_browser = "firefox"
let g:haddock_indexfiledir = "~/.haddock"
let g:haddock_docdir = "~/.haddock"

let g:syntastic_check_on_open=1

