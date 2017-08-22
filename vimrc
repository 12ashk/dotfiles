"let $PATH = "~/.pyenv/shims:".$PATH

syntax on
set nocompatible
inoremap <silent> jj <ESC>
set smarttab
set cindent
set smartcase
set ignorecase
set wrapscan
set tabstop=4
set shiftwidth=4
set expandtab
set incsearch
set hlsearch
set showmatch
set matchtime=1
set showcmd
set expandtab
set autoindent
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
" 改行コードの自動認識
set fileformats=unix,dos,mac
set ambiwidth=double
let g:echodoc_enable_at_startup = 1

" プラグインが実際にインストールされるディレクトリ
let s:dein_dir = expand('~/.cache/dein')
" dein.vim 本体
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
" dein.vim がなければ github から落としてくる
if &runtimepath !~# '/dein.vim'
    if !isdirectory(s:dein_repo_dir)
        execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
    endif
    execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')

    " 設定開始
    if dein#load_state(s:dein_dir)
        call dein#begin(s:dein_dir)

        " プラグインリストを収めた TOML ファイル
        " 予め TOML ファイル（後述）を用意しておく
        let g:rc_dir    = expand('~/.vim/rc')
        let s:toml      = g:rc_dir . '/dein.toml'
        let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

        " TOML を読み込み、キャッシュしておく
        call dein#load_toml(s:toml,      {'lazy': 0})
        call dein#load_toml(s:lazy_toml, {'lazy': 1})

        " 設定終了
        call dein#end()
        call dein#save_state()
    endif

    " もし、未インストールものものがあったらインストール
    if dein#check_install()
        call dein#install()
    endif


    let g:unite_split_rule = 'botright'
    noremap ,u <ESC>:Unite -vertical -winwidth=30 outline<Return>

    " .md as markdown
    au BufRead,BufNewFile *.md set filetype=markdown

    " NERD_commenter.vim
    let g:NERDCreateDefaultMappings = 0
    let NERDSpaceDelims = 1
    nmap <Leader>/ <Plug>NERDCommenterToggle
    vmap <Leader>/ <Plug>NERDCommenterToggle
    nmap <Leader>/a <Plug>NERDCommenterAppend
    nmap <leader>/9 <Plug>NERDCommenterToEOL
    vmap <Leader>/s <Plug>NERDCommenterSexy
    vmap <Leader>/b <Plug>NERDCommenterMinimal


    "setting for vimfiler
    "vimデフォルトのエクスプローラをvimfilerで置き換える
    let g:vimfiler_as_default_explorer = 1
    ""セーフモードを無効にした状態で起動する
    let g:vimfiler_safe_mode_by_default = 0
    "現在開いているバッファのディレクトリを開く
    nnoremap <silent> <Leader>fe :<C-u>VimFilerBufferDir -quit<CR>


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
    "
    "    """ keybind for neocomplcache
    "    " <TAB>: completion
    "    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    "    " Plugin key-mappings.
    "    inoremap <expr><C-g>     neocomplete#undo_completion()
    "inoremap <expr><C-l>     neocomplete#complete_common_string()

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

    """ Vim-LaTeX 
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
    "
endif
