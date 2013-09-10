set nocompatible               " Be iMproved

if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'banyan/recognize_charcode.vim'

NeoBundle 'nathanaelkane/vim-indent-guides'
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=239
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=239
nnoremap <silent> [toggle]i  :IndentGuidesToggle<CR>

NeoBundle 'scrooloose/nerdtree'
nmap <Leader>n :NERDTreeToggle<CR>
let NERDTreeShowHidden = 1
let NERDTreeAutoDeleteBuffer = 1
autocmd VimEnter * NERDTree ./
autocmd VimEnter * wincmd l

set tabstop=4
set smarttab
set smartindent
set cindent
set autoindent
set nosmartcase
set shiftwidth=4
set incsearch
set showmatch
set showcmd
set hlsearch
set number
set wildmenu
set noswapfile
set laststatus=2
set statusline=[%Y][%{&fileencoding}:%{&ff}]\%F\%=[\ \%l:\ \%c]--%p%%--
set ruler
syntax on

"let g:solarized_termcolors=256
set background=dark
colorscheme hybrid

filetype plugin indent on
set fileencoding=utf-8
set fileencodings=iso-2022-jp,utf-8,euc-jp

" 改行コードの自動認識
set fileformats=unix,dos,mac
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
	set ambiwidth=double
endif
function! InsertTabWrapper()
	let col = col('.') - 1
	if !col || getline('.')[col - 1] !~ '\k'
		return "\<TAB>"
	else
		if pumvisible()
			return "\<C-N>"
		else
			return "\<C-N>\<C-P>"
		end
	endif
endfunction
" Remap the tab key to select action with InsertTabWrapper
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" }}} Autocompletion using the TAB key

let g:neocomplcacheenableatstartup = 1
let g:echodoc_enable_at_startup = 1

imap <C-e> <END> 
imap <C-a> <HOME>

""
"" neocomplcache & neosnippet
""
let g:neocomplcache_enable_at_startup = 1
""imap <C-k>     <Plug>(neosnippet_expand_or_jump)
""smap <C-k>     <Plug>(neosnippet_expand_or_jump)
" neocomplcache
let g:neocomplcache_enable_at_startup = 1 " 起動時に有効化
"inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" neocomplcache
let g:neocomplcache_enable_at_startup = 1 " 起動時に有効化

""let g:filetype_m = 'objc'

set mouse=a
set ttymouse=xterm2

if has('gui_running')
	set mousemodel=popup
	set nomousefocus
	set mousehide
endif
" Emacs-like keybind
cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <C-h> <Backspace>
cnoremap <C-k> <C-\>e
inoremap <C-a> <Home>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-h> <Backspace>
inoremap <C-k> <C-o>
inoremap <C-n> <Down>
inoremap <C-p> <Up>

NeoBundleCheck
