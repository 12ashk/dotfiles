" Author:       konitter <web.kony@gmail.com>
" URL:          http://re-dzine.net/
" Last Change:  June 18, 2012

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "night_watch"

" General
hi Cursor                        guibg=#656565 gui=none
hi CursorLine                    guibg=#000000
hi CursorColumn                  guibg=#000000
hi Folded          guifg=#a0a8b0 guibg=#384048 gui=none
hi LineNr          guifg=#857b6f guibg=#000000 gui=none
hi MatchParen      guifg=#f6f3e8 guibg=#857b6f gui=bold
hi Normal          guifg=#f6f3e8 guibg=#242424 gui=none
hi NonText         guifg=#808080 guibg=#303030 gui=none
hi Pmenu           guifg=#f6f3e8 guibg=#444444
hi PmenuSel        guifg=#000000 guibg=#e6db74
hi StatusLine      guifg=#f6f3e8 guibg=#444444 gui=italic
hi StatusLineNC    guifg=#857b6f guibg=#444444 gui=none
hi SpecialKey      guifg=#393e40
hi Title           guifg=fg
hi VertSplit       guifg=#444444 guibg=#444444 gui=none
hi Visual          guifg=#f6f3e8 guibg=#444444 gui=none
hi WarningMsg      guifg=#ff6666

" Syntax
hi Comment         guifg=#99968b               gui=italic
hi Constant        guifg=#e5786d               gui=none
hi Function        guifg=#e6db74               gui=none
hi Identifier      guifg=#e6db74               gui=none
hi Keyword         guifg=#8ac6f2               gui=none
hi Number          guifg=#e5786d               gui=none
hi PreProc         guifg=#e5786d               gui=none
hi Special         guifg=#e7f6da               gui=none
hi Statement       guifg=#8ac6f2               gui=none
hi String          guifg=#95e454               gui=italic
hi Todo            guifg=#ffff00 guibg=NONE    gui=underline
hi Type            guifg=#e6db74               gui=none
hi Underlined      guifg=#99968b

" HTML
hi htmlEndTag      guifg=fg
hi htmlTag         guifg=fg

" CSS
hi cssBraces       guifg=fg
hi cssSelectorOp   guifg=fg
hi link cssImportant Statement
hi link cssmedia     Statement
