set background=dark
hi clear
if exists("syntax_on")
syntax reset
endif
let g:colors_name = "olgas"

" cterm colors
hi Normal cterm=NONE ctermfg=15 ctermbg=0
hi Comment cterm=NONE ctermfg=32 ctermbg=NONE
hi Constant cterm=NONE ctermfg=40 ctermbg=NONE
hi Identifier cterm=bold ctermfg=11 ctermbg=NONE
hi Statement cterm=bold ctermfg=196 ctermbg=NONE
hi PreProc cterm=NONE ctermfg=136 ctermbg=NONE
hi Type cterm=bold ctermfg=135 ctermbg=NONE
hi Special cterm=bold ctermfg=51 ctermbg=NONE
hi Underlined cterm=underline ctermfg=NONE ctermbg=NONE
hi Ignore cterm=NONE ctermfg=240 ctermbg=NONE
hi Error cterm=bold ctermfg=15 ctermbg=196
hi Todo cterm=bold ctermfg=16 ctermbg=11
hi ColorColumn cterm=NONE ctermfg=NONE ctermbg=17
hi Conceal cterm=NONE ctermfg=248 ctermbg=NONE
hi CursorColumn cterm=NONE ctermfg=NONE ctermbg=17
hi CursorLine cterm=underline ctermfg=NONE ctermbg=17
hi Directory cterm=NONE ctermfg=11 ctermbg=NONE
hi DiffAdd cterm=NONE ctermfg=0 ctermbg=14
hi DiffChange cterm=NONE ctermfg=0 ctermbg=14
hi DiffText cterm=NONE ctermfg=0 ctermbg=14
hi ErrorMsg cterm=bold ctermfg=15 ctermbg=196
hi VertSplit cterm=NONE ctermfg=248 ctermbg=17
hi Folded cterm=bold ctermfg=15 ctermbg=17
hi FoldColumn cterm=bold ctermfg=11 ctermbg=17
hi SignColumn cterm=bold ctermfg=14 ctermbg=17
hi IncSearch cterm=bold ctermfg=16 ctermbg=208
hi LineNr cterm=NONE ctermfg=239 ctermbg=NONE
hi CursorLineNr cterm=bold ctermfg=16 ctermbg=7
hi MatchParen cterm=bold ctermfg=196 ctermbg=NONE
hi ModeMsg cterm=bold ctermfg=NONE ctermbg=4
hi MoreMsg cterm=bold ctermfg=NONE ctermbg=4
hi NonText cterm=bold ctermfg=4 ctermbg=NONE
hi PmenuSel cterm=NONE ctermfg=0 ctermbg=14
hi PmenuSbar cterm=NONE ctermfg=0 ctermbg=14
hi PmenuThumb cterm=NONE ctermfg=0 ctermbg=14
hi Question cterm=bold ctermfg=15 ctermbg=2
hi Search cterm=bold ctermfg=16 ctermbg=11
hi SpecialKey cterm=bold ctermfg=11 ctermbg=17
hi SpellBad cterm=underline ctermfg=203 ctermbg=NONE
hi SpellCap cterm=underline ctermfg=214 ctermbg=NONE
hi SpellLocal cterm=underline ctermfg=105 ctermbg=NONE
hi SpellRare cterm=underline ctermfg=112 ctermbg=NONE
hi StatusLine cterm=NONE ctermfg=NONE ctermbg=NONE
hi StatusLineNC cterm=NONE ctermfg=241 ctermbg=NONE
hi Title cterm=bold ctermfg=14 ctermbg=NONE
hi Visual cterm=bold ctermfg=NONE ctermbg=4
hi VisualNOS cterm=bold,underline ctermfg=11 ctermbg=196
hi WarningMsg cterm=bold ctermfg=15 ctermbg=1
hi WildMenu cterm=bold ctermfg=15 ctermbg=17
hi TabLine cterm=NONE ctermfg=248 ctermbg=17
hi TabLineFill cterm=NONE ctermfg=0 ctermbg=17
hi TabLineSel cterm=bold ctermfg=15 ctermbg=0

"gui colors
hi Normal gui=NONE guifg=white guibg=black guisp=NONE
hi Comment gui=NONE guifg=#7799cc guibg=NONE guisp=NONE
hi Constant gui=NONE guifg=green guibg=NONE guisp=NONE
hi Identifier gui=bold guifg=yellow guibg=NONE guisp=NONE
hi Statement gui=bold guifg=orangered guibg=NONE guisp=NONE
hi PreProc gui=NONE guifg=#ffaa44 guibg=NONE guisp=NONE
hi Type gui=bold guifg=#aa55ff guibg=NONE guisp=NONE
hi Special gui=bold guifg=cyan guibg=NONE guisp=NONE
hi Underlined gui=underline guifg=NONE guibg=NONE guisp=NONE
hi Ignore gui=NONE guifg=grey20 guibg=NONE guisp=NONE
hi Error gui=bold guifg=white guibg=red guisp=NONE
hi Todo gui=bold guifg=black guibg=gold guisp=NONE
hi ColorColumn gui=NONE guifg=NONE guibg=#000036 guisp=NONE
hi Conceal gui=NONE guifg=LightGrey guibg=gray20 guisp=NONE
hi Cursor gui=bold guifg=black guibg=white guisp=NONE
hi CursorIM gui=underline guifg=yellow guibg=red guisp=NONE
hi CursorColumn gui=NONE guifg=NONE guibg=#000036 guisp=NONE
hi CursorLine gui=underline guifg=NONE guibg=#000036 guisp=NONE
hi Directory gui=NONE guifg=yellow guibg=NONE guisp=NONE
hi DiffAdd gui=NONE guifg=black guibg=cyan guisp=NONE
hi DiffChange gui=NONE guifg=black guibg=cyan guisp=NONE
hi DiffText gui=NONE guifg=black guibg=cyan guisp=NONE
hi ErrorMsg gui=bold guifg=white guibg=red guisp=NONE
hi VertSplit gui=NONE guifg=gray20 guibg=#000036 guisp=NONE
hi Folded gui=bold guifg=white guibg=#000056 guisp=NONE
hi FoldColumn gui=bold guifg=yellow guibg=#000026 guisp=NONE
hi SignColumn gui=bold guifg=cyan guibg=#000026 guisp=NONE
hi IncSearch gui=bold guifg=black guibg=orangered guisp=NONE
hi LineNr gui=NONE guifg=gray30 guibg=#000026 guisp=NONE
hi CursorLineNr gui=bold guifg=gray10 guibg=gray60 guisp=NONE
hi MatchParen gui=bold guifg=red guibg=#000036 guisp=NONE
hi ModeMsg gui=bold guifg=white guibg=blue guisp=NONE
hi MoreMsg gui=bold guifg=white guibg=blue guisp=NONE
hi NonText gui=bold guifg=#5577ee guibg=gray5 guisp=NONE
hi PmenuSel gui=NONE guifg=black guibg=cyan guisp=NONE
hi PmenuSbar gui=NONE guifg=black guibg=cyan guisp=NONE
hi PmenuThumb gui=NONE guifg=black guibg=cyan guisp=NONE
hi Question gui=bold guifg=white guibg=darkgreen guisp=NONE
hi Search gui=bold guifg=black guibg=orange guisp=NONE
hi SpecialKey gui=bold guifg=yellow guibg=gray10 guisp=NONE
hi SpellBad gui=undercurl guifg=NONE guibg=NONE guisp=red
hi SpellCap gui=undercurl guifg=NONE guibg=NONE guisp=yellow
hi SpellLocal gui=undercurl guifg=NONE guibg=NONE guisp=LightBlue
hi SpellRare gui=undercurl guifg=NONE guibg=NONE guisp=green
hi StatusLine gui=NONE guifg=NONE guibg=#000026 guisp=NONE
hi StatusLineNC gui=NONE guifg=gray40 guibg=#000026 guisp=NONE
hi Title gui=bold guifg=MediumSpringGreen guibg=NONE guisp=NONE
hi Visual gui=bold guifg=white guibg=blue guisp=NONE
hi VisualNOS gui=underline guifg=yellow guibg=red guisp=NONE
hi WarningMsg gui=bold guifg=white guibg=orangered guisp=NONE
hi WildMenu gui=bold guifg=white guibg=#112266 guisp=NONE
hi TabLine gui=NONE guifg=black guibg=cyan guisp=NONE
hi TabLineFill gui=NONE guifg=black guibg=cyan guisp=NONE
hi TabLineSel gui=NONE guifg=black guibg=cyan guisp=NONE

