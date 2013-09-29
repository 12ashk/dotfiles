if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'banyan/recognize_charcode.vim'

NeoBundle 'scrooloose/nerdtree'
nmap <Leader>n :NERDTreeToggle<CR>
let NERDTreeShowHidden = 1
let NERDTreeAutoDeleteBuffer = 1
autocmd VimEnter * NERDTree ./
autocmd VimEnter * wincmd l

set nocompatible
set tabstop=4
set smarttab
set smartindent
set cindent
set autoindent
set smartcase
set ignorecase
set wrapscan
set shiftwidth=4
set incsearch
set showmatch
set showcmd
set hlsearch
nmap <Esc><Esc> :nohlsearch<CR><Esc>
set number
set wildmenu
set noswapfile
"set visualbell t_vb=
set laststatus=2
set statusline=[%Y][%{&fileencoding}:%{&ff}]\%F\%=[\ \%l:\ \%c]--%p%%--
set ruler
set clipboard=unnamed
set mouse=a
set guioptions+=a
set ttymouse=xterm2
set backspace=2
set shellslash
set grepprg=grep\ -nH\ $*et grepprg=grep\ -nH\ $*
syntax on
set mouse=a
set ttymouse=xterm2

let g:solarized_termcolors=256
set background=dark
colorschem desert

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

let g:echodoc_enable_at_startup = 1

imap <C-e> <END> 
imap <C-a> <HOME>

""let g:filetype_m = 'objc'

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

if has('lua') && ( (v:version == 703 && has('patch885')) || v:version == 704 )
  NeoBundleLazy 'Shougo/neocomplete.vim', {
        \ 'autoload': {
        \   'insert': 1,
        \ }}
  let s:hooks = neobundle#get_hooks('neocomplete.vim')
  function! s:hooks.on_source(bundle)
    " Disable AutoComplPop.
    let g:acp_enableAtStartup = 0
    " Use neocomplete.
    let g:neocomplete#enable_at_startup = 1
    " Use smartcase.
    let g:neocomplete#enable_smart_case = 1
    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
  endfunction
else
  NeoBundleLazy 'Shougo/neocomplcache.vim', {
        \ 'autoload': {
        \   'insert': 1,
        \ }}
  let s:hooks = neobundle#get_hooks('neocomplcache.vim')
  function! s:hooks.on_source(bundle)
    " Disable AutoComplPop.
    let g:acp_enableAtStartup = 0
    " Use neocomplcache.
    let g:neocomplcache_enable_at_startup = 1
    " Use smartcase.
    let g:neocomplcache_enable_smart_case = 1
    " Set minimum syntax keyword length.
    let g:neocomplcache_min_syntax_length = 3
    let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
  endfunction
endif

""" Vim-LaTeX 
"NeoBundle 'git://git.code.sf.net/p/vim-latex/vim-latex'
"let g:Tex_AutoFolding = 0
"let g:tex_flavor='latex'
"let g:Imap_UsePlaceHolders = 1
"let g:Imap_DeleteEmptyPlaceHolders = 1
"let g:Imap_StickyPlaceHolders = 0
"let g:Tex_DefaultTargetFormat = 'dvi'
""let g:Tex_FormatDependency_pdf = 'pdf'
""let g:Tex_FormatDependency_ps = 'dvi,ps'
"let g:Tex_CompileRule_pdf = '/usr/texbin/dvipdfmx $*.dvi'
""let g:Tex_CompileRule_ps = '/usr/texbin/dvips -Ppdf -o $*.ps $*.dvi'
"let g:Tex_CompileRule_dvi = '/usr/texbin/platex -synctex=1 -interaction=nonstopmode -file-line-error-style $*'
""let g:Tex_BibtexFlavor = '/usr/texbin/pbibtex'
""let g:Tex_MakeIndexFlavor = '/usr/texbin/mendex $*.idx'
"let g:Tex_UseEditorSettingInDVIViewer = 1
"let g:Tex_ViewRule_pdf = '/usr/bin/open -a Preview.app'
""let g:Tex_ViewRule_ps = '/usr/bin/open'
"let g:Tex_ViewRule_dvi = '/usr/bin/open'

NeoBundleCheck
