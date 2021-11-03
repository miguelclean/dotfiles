"
" Miguel's Vim Colors
"
" So far this scheme is mostly optimized for the gui version of vim.
" adaption for cterm / xterm might follow one day
"
" useful vim help-screens: group-name, highlight-groups, cterm-colors
" rgb: /usr/share/vim/vim73/rgb.txt
"

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "miguels"

"first objective is to set: guifg guibg gui and maybe guisp
"afterwards we will port this to xterm
""""""""""""""""""""""""""""""""""""""""""""""""""""

"Normal		normal text, we can also use "font=Monospace 10" or similar 
hi Normal	guifg=White guibg=Black gui=none ctermfg=White  ctermbg=Black

"""""""""""""""""""""""""""""""""""""""""""""""""""

"Comment	/* */, //, ...
hi Comment	guifg=#5a7 gui=none ctermfg=DarkGreen cterm=none

"Constant	true, false, 0.123, "hello", <iostream> ...
hi Constant	guifg=#8f6 gui=bold ctermfg=Cyan cterm=none

"Identifier	mysql_escape_string, $dupa, ...
hi Identifier	guifg=orange gui=bold ctermfg=Brown cterm=none

"Statement	if, for, return, ...
hi Statement	guifg=#f76 gui=bold ctermfg=Red cterm=none

"PreProc	#include, ...
hi PreProc	guifg=#fe5 gui=none ctermfg=Yellow cterm=none

"Type		int, float, bool, struct ...
hi Type		guifg=#89f gui=bold ctermfg=Blue cterm=bold

"Special	<?php, '\n', ...
hi Special	guibg=gray20 guifg=#afc gui=bold ctermbg=DarkGray ctermfg=Green cterm=bold
    

"
hi Underlined	gui=underline,bold guibg=#521 guifg=white ctermbg=black ctermfg=white cterm=underline
hi Ignore	guifg=grey30 ctermfg=DarkGray

"hi Error	guifg=white guibg=#a00 gui=bold cterm=bold ctermfg=white ctermbg=red
hi Error	guibg=#a00
hi Todo		guifg=black guibg=#dd7 gui=bold ctermbg=yellow ctermfg=black

""""""""""""""""""""""""""""""""""""""""""""""""  

"ColorColumn	used for the columns set with 'colorcolumn'
hi ColorColumn  guibg=#200 ctermbg=DarkRed ctermfg=White

"Conceal	placeholder characters substituted for concealed text (see 'conceallevel')
hi Conceal	guibg=gray20

"Cursor		the character under the cursor
hi Cursor	guifg=black guibg=#afe gui=bold 

"CursorIM	like Cursor, but used when in IME mode |CursorIM|
"		I do not use this by now (set to strange colors to notice them easily)
hi CursorIM	gui=underline guifg=yellow guibg=red

"CursorColumn	the screen column that the cursor is in when 'cursorcolumn' is set
hi CursorColumn	guibg=#052010

"CursorLine	the screen line that the cursor is in when 'cursorline' is set
hi CursorLine	guibg=#052010

"Directory	directory names (and other special names in listings)
hi Directory	guifg=green

"DiffAdd	diff mode: Added line |diff.txt|
hi DiffAdd	guibg=#326

"DiffChange	diff mode: Changed line |diff.txt|
hi DiffChange	gui=reverse

"DiffDelete	diff mode: Deleted line |diff.txt|
hi DiffDelete	guibg=#a43 guifg=white

"DiffText	diff mode: Changed text within a changed line |diff.txt|
hi DiffText	guibg=DarkRed

"ErrorMsg	error messages on the command line
hi ErrorMsg	guifg=white guibg=#a00 gui=bold

"VertSplit	the column separating vertically split windows
hi VertSplit	guibg=gray20 guifg=gray60 gui=none

"Folded		line used for closed folds
hi Folded	guibg=gray20 guifg=#f7f gui=bold

"FoldColumn	'foldcolumn'
hi FoldColumn	guibg=gray20 guifg=#f7f gui=bold

"SignColumn	column where |signs| are displayed
hi SignColumn	guibg=gray10

"IncSearch	'incsearch' highlighting; also used for the text replaced with ":s///c"
hi IncSearch	guifg=yellow guibg=black

"LineNr		Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
hi LineNr	guifg=gray80 guibg=gray10 gui=none ctermbg=Black ctermfg=Gray

"CursorLineNr	Like LineNr when 'cursorline' is set for the cursor line.
hi CursorLineNr	guifg=gray10 guibg=gray60 gui=bold

"MatchParen	The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
hi MatchParen	guifg=black guibg=white gui=bold

"ModeMsg	'showmode' message (e.g., "-- INSERT --")
hi ModeMsg	guifg=#9fa gui=bold guibg=gray10

"MoreMsg	|more-prompt|
hi MoreMsg	guifg=yellow guibg=gray10 gui=bold 

"NonText	'~' and '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line).
hi NonText	guibg=gray10 guifg=#f0f gui=bold

"Pmenu		Popup menu: normal item.
hi Pmenu	guibg=orange guifg=black

"PmenuSel	Popup menu: selected item.
hi PmenuSel	guibg=red guifg=black gui=bold

"PmenuSbar	Popup menu: scrollbar.
hi PmenuSbar	guibg=red

"PmenuThumb	Popup menu: Thumb of the scrollbar.
hi PmenuThumb	guibg=black

"Question	|hit-enter| prompt and yes/no questions
hi Question	guifg=yellow guibg=gray10 gui=bold

"Search		Last search pattern highlighting (see 'hlsearch'). Also used for highlighting the current line in the quickfix window and similar items that need to stand out. 
hi Search	guibg=orange guifg=black

"SpecialKey	Meta and special keys listed with ":map", also for text used to show unprintable characters in the text, 'listchars'.  Generally: text that is displayed differently from what it really is.
hi SpecialKey	guibg=gray20 guifg=#f95 gui=bold

"SpellBad	Word Warsaw that is not recoggized by the spellchecker. |spell| This will be combined with the highlighting used otherwise.
hi SpellBad	gui=undercurl guisp=red

"SpellCap	Word that should start with a capital. |spell| This will be combined with the highlighting used otherwise.
hi SpellCap	gui=undercurl guisp=yellow

"SpellLocal	Word that is recognized by the spellchecker as one that is used in another region. |spell| This will be combined with the highlighting used otherwise.
hi SpellLocal	gui=undercurl guisp=LightBlue

"SpellRare	Word that is recognized by the spellchecker as one that is hardly ever used. |spell| This will be combined with the highlighting used otherwise.
hi SpellRare	gui=undercurl guisp=green

"StatusLine	status line of current window
hi StatusLine	guibg=gray20 guifg=white gui=none

"StatusLineNC	status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
hi StatusLineNC	guibg=gray10 guifg=grey50 gui=none

"Title		titles for output from ":set all", ":autocmd" etc.
hi Title	guifg=red guibg=gray10 gui=bold

"Visual		Visual mode selection
hi Visual	gui=none guifg=white guibg=blue ctermfg=white ctermbg=blue cterm=none term=reverse

"VisualNOS	Visual mode selection when vim is "Not Owning the Selection".  Only X11 Gui's |gui-x11| and |xterm-clipboard| supports this.
"		I do not use this by now (set to strange colors to notice them easily)
hi VisualNOS	gui=underline guifg=yellow guibg=red

"WarningMsg	warning messages
hi WarningMsg	guifg=black guibg=#fa0 gui=bold

"WildMenu	current match in 'wildmenu' completion
hi WildMenu	guibg=yellow guifg=black gui=bold

"
"(The following only applies to the terminal version)
"

"TabLine	tab pages line, not active tab page label
hi TabLine	ctermbg=darkgray ctermfg=gray

"TabLineFill	tab pages line, where there are no labels
hi TabLineFill	ctermbg=black cterm=none

"TabLineSel	tab pages line, active tab page label
hi TabLineSel	ctermbg=gray ctermfg=white


