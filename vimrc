if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'banyan/recognize_charcode.vim'
NeoBundle 'kana/vim-smartword'
let g:echodoc_enable_at_startup = 1
NeoBundle 'tpope/vim-surround'
autocmd BufNewFile,BufRead *.cs set filetype=python
NeoBundle 'scrooloose/syntastic.git'
NeoBundle 'Shougo/vimproc'

" Vim-indent-guides
NeoBundle "nathanaelkane/vim-indent-guides"
let s:hooks = neobundle#get_hooks("vim-indent-guides")
function! s:hooks.on_source(bundle)
	let g:indent_guides_guide_size = 1
	IndentGuidesEnable
endfunction

" NERD_commenter.vim
NeoBundle 'scrooloose/nerdcommenter.git'
let g:NERDCreateDefaultMappings = 0
let NERDSpaceDelims = 1
nmap <Leader>/ <Plug>NERDCommenterToggle
vmap <Leader>/ <Plug>NERDCommenterToggle
nmap <Leader>/a <Plug>NERDCommenterAppend
nmap <leader>/9 <Plug>NERDCommenterToEOL
vmap <Leader>/s <Plug>NERDCommenterSexy
vmap <Leader>/b <Plug>NERDCommenterMinimal

"setting for vimfiler
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler.vim'
"vimデフォルトのエクスプローラをvimfilerで置き換える
let g:vimfiler_as_default_explorer = 1
""セーフモードを無効にした状態で起動する
let g:vimfiler_safe_mode_by_default = 0
"現在開いているバッファのディレクトリを開く
nnoremap <silent> <Leader>fe :<C-u>VimFilerBufferDir -quit<CR>



set nocompatible
set smarttab
set cindent
set smartcase
set ignorecase
set wrapscan
set tabstop=4
set shiftwidth=4
set incsearch
set hlsearch
set showmatch
set matchtime=1
set showcmd
set noexpandtab
set softtabstop=0
nmap <Esc><Esc> :nohlsearch<CR><Esc>
set number
set wildmenu
set backup
set backupdir=~/.vim_backup
set noswapfile
set laststatus=2
set statusline=[%Y][%{&fileencoding}:%{&ff}]\%F\%=[\ \%l:\ \%c]--%p%%--
set ruler
set clipboard=unnamed,autoselect
set mouse=a
set ttymouse=xterm2
set backspace=2
syntax on
""let g:filetype_m = 'objc'

let g:solarized_termcolors=256
set background=dark
colorschem desert

set guioptions+=a
if has('gui_running')
	set mousemodel=popup
	set nomousefocus
	set mousehide
	set background=dark
	colorschem solarized
endif

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

""" keybind for neocomplcache
" <TAB>: completion
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" new line with Enter key
noremap <CR><CR> o<ESC>
" Emacs-like keybind
cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <C-h> <Backspace>
cnoremap <C-k> <C-\>e
inoremap <C-a> <Home>
inoremap <C-p> <Up>
inoremap <C-n> <Down>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-h> <Backspace>

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
NeoBundle 'git://git.code.sf.net/p/vim-latex/vim-latex'
let g:Tex_AutoFolding = 0
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 1
let g:Imap_DeleteEmptyPlaceHolders = 1
let g:Imap_StickyPlaceHolders = 0
let g:Tex_DefaultTargetFormat = 'dvi'
"let g:Tex_FormatDependency_pdf = 'pdf'
"let g:Tex_FormatDependency_ps = 'dvi,ps'
let g:Tex_CompileRule_pdf = '/usr/texbin/dvipdfmx $*.dvi'
"let g:Tex_CompileRule_ps = '/usr/texbin/dvips -Ppdf -o $*.ps $*.dvi'
"let g:Tex_CompileRule_dvi = '/usr/texbin/platex -synctex=1 -interaction=nonstopmode -file-line-error-style $*'
let g:Tex_CompileRule_dvi = '/usr/texbin/platex -interaction=nonstopmode -file-line-error-style $*'
"let g:Tex_BibtexFlavor = '/usr/texbin/pbibtex'
"let g:Tex_MakeIndexFlavor = '/usr/texbin/mendex $*.idx'
let g:Tex_UseEditorSettingInDVIViewer = 1
let g:Tex_ViewRule_pdf = '/usr/bin/open -a Preview.app'
"let g:Tex_ViewRule_ps = '/usr/bin/open'
let g:Tex_ViewRule_dvi = '/usr/bin/open'

"lightline
NeoBundle 'itchyny/lightline.vim'
let g:lightline = {
			\ 'colorscheme': 'wombat',
			\ 'mode_map': {'c': 'NORMAL'},
			\ 'active': {
			\   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
			\ },
			\ 'component_function': {
			\   'modified': 'MyModified',
			\   'readonly': 'MyReadonly',
			\   'fugitive': 'MyFugitive',
			\   'filename': 'MyFilename',
			\   'fileformat': 'MyFileformat',
			\   'filetype': 'MyFiletype',
			\   'fileencoding': 'MyFileencoding',
			\   'mode': 'MyMode'
			\ }
			\ }

function! MyModified()
	return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
	return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
endfunction

function! MyFilename()
	return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
				\ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
				\  &ft == 'unite' ? unite#get_status_string() :
				\  &ft == 'vimshell' ? vimshell#get_status_string() :
				\ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
				\ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
	try
		if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
			return fugitive#head()
		endif
	catch
	endtry
	return ''
endfunction

function! MyFileformat()
	return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! MyFiletype()
	return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
	return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
	return winwidth(0) > 60 ? lightline#mode() : ''
endfunction
"end lightline setting

NeoBundleCheck
