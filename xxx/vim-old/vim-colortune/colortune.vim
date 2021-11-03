"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Miguel's ColorTune plugin
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" {{{
"
" TODO LIST:
" {{{
" * restrict movement and values, but how???
" * use R/G/B r/g/b keys to increase/decrease rgb values
" * map some nice short-keys for this buffer only
" * show rgb.txt! to alllow selection of named gui-colors
" * show some dropdowns or something for other stuff
" * let user override some of the values from users vimrc!!
" * enable reset to colortheme
" * clear all groups / clear group completely or just for active mode!
" * document and publish
" * treat NONE and NOT SET AT ALL differently!!
" * add some error detection and treatment! ie. setline etc..
" * just redraw single lines where possible
" * REORDER and CLEANUP
" * create bundle and move most of functioanlity to autoload,
"	except Colortune_start or somehting
" * check if we are not reinventing the wheel at some places:
"	ie: getline()?
" * check if there are already such plugins, and find out why this one is
"	better ;)
" * BUG: regex limited number so start/stop omittted in complete list
"	so it is also not exported via config_dump right now
" * BUG: i do not understand set background. it is always set to dark by now
" * BUG: column names are not nicely aligned
" * BUG: spaces are not supported within key-values
"
" }}}

" Set some basic defaults and initialize
" {{{

" modes are: 0-term, 1-cterm, 2-gui, 3-complete
let s:default_mode=2

let s:colortune_buf_name='__COLORTUNE_PLUGIN__'

let s:temp_reg='a'

" Define the popular highlight groups
let s:colortable=[
    \	'Normal', 'Comment', 'Constant', 'Identifier',
    \	'Statement', 'PreProc', 'Type', 'Special', 'Underlined',
    \	'Ignore', 'Error', 'Todo', 'ColorColumn', 'Conceal', 'Cursor',
    \	'CursorIM', 'CursorColumn', 'CursorLine', 'Directory',
    \	'DiffAdd', 'DiffChange', 'DiffText', 'ErrorMsg', 'VertSplit',
    \	'Folded', 'FoldColumn', 'SignColumn', 'IncSearch', 'LineNr',
    \	'CursorLineNr', 'MatchParen', 'ModeMsg', 'MoreMsg', 'NonText',
    \	'PmenuSel', 'PmenuSbar', 'PmenuThumb', 'Question', 'Search',
    \	'SpecialKey', 'SpellBad', 'SpellCap', 'SpellLocal',
    \	'SpellRare', 'StatusLine', 'StatusLineNC', 'Title', 'Visual',
    \	'VisualNOS', 'WarningMsg', 'WildMenu', 'TabLine',
    \	'TabLineFill', 'TabLineSel' ]


" Define modes and their relevant keys in the highlight groups
let s:modes_labels=['Black & White Terminals','Color Terminals','GUI','Complete']
let s:modes=[	['term','start','stop'],
	    \	['cterm','ctermfg','ctermbg'],
	    \	['gui','guifg','guibg','guisp'],
	    \   ['term','cterm','ctermfg','ctermbg','gui','guifg','guibg','guisp']]

" init
"
let s:current_mode=s:default_mode
" }}}

" Colortune_getline()
" {{{
" Retruns current line of current buffer.
"
function! Colortune_getline()
    return getline(line('.'))
endfunction
" }}}

" Colortune_getcmdoutput(cmd)
" {{{
" Redirect the output of cmd to a temporary register and return result.
"
function! Colortune_getcmdoutput(cmd)

	let reg_val=getreg(s:temp_reg)
	redir @a
	    silent execute a:cmd
	redir END
	let val=getreg(s:temp_reg)
	call setreg(s:temp_reg,reg_val)
	return val

endfunction
" }}}

" Colortune_getattr(group,key)
" {{{
" Get the value of one 'key' of a highlighting group
" returns 'NONE' if the requested key is not specified
"
function! Colortune_getattr(group,key)

	let hil=Colortune_getcmdoutput('hi '.a:group)

	" extract the value for given key (if exists) and return
	if hil !~# a:key.'=.\+'
	    let val='NONE'
	else
	    let val=substitute(hil.' ','^.* '.a:key.'=\(.\{-}\)\s.*$','\1','')
	endif
	return val

endfunction
"}}}

" Colortune_getmax(key)
" {{{
" Returns maximal length of values for requested key.
" key='GROUP_NAME' returns maximal length of highlight group names.
"
function! Colortune_getmax(key)
    let max=0
    for group in s:colortable
	if(a:key==#'GROUP_NAME')
	    let length=len(group)
	else
	    let length=len(Colortune_getattr(group,a:key))
	endif
	let max = max<length ? length : max
    endfor
    return max
endfunction
" }}}

" Colortune_extend(val,fill,len)
" {{{
" Extend string 'val' with characters 'fill' to match length of 'len'
"
function! Colortune_extend(val,fill,len)

    let len=a:len-len(a:val)
    let val=a:val

    while len>0
	let len-=1
	let val.=a:fill
    endwhile
    return val

endfunction
" }}}

" Colortune_togglemode()
" {{{
" Cycles through available modes
"
function! Colortune_togglemode()

    let max=len(s:modes)-1
    let s:current_mode+=1
    if s:current_mode > max
	let s:current_mode=0
    endif
    call Colortune_fill()
endfunction
" }}}

" Colortune_getregexpattern()
" {{{
" Helper function that builds our regex-pattern for setting and clearing
" the highlight values, based on the current buffer-line
"
function! Colortune_getregexpattern()

    let str='^\(.\{-}\)\s.*'

    for column in s:modes[s:current_mode]
	let str.='.*\[\(.*\)\]'
    endfor

    return str

endfunction
" }}}

" Colortune_getregexstring()
" {{{
" Helper function that builds our substitution string for setting
" the highlight values, based on the current buffer-line
"
function! Colortune_getregexstring()
    let str=''
    let idx=2
    for column in s:modes[s:current_mode]
	let str.=' '.column.'=\'.idx
	let idx+=1
    endfor
    return str
endfunction
" }}}

" Colortune_clearbyline()
" {{{
" Clear highlight group in current line
" Set highlight group values by extracting them from the current line
"
function! Colortune_clearbyline()
    let myline=Colortune_getline()
    exec substitute(myline,Colortune_getregexpattern(),'hi clear \1','')
    call Colortune_fill()
endfunction
" }}}

" Colortune_setbyline()
" {{{
" Set highlight group values by extracting them from the current line
"
function! Colortune_setbyline()
    let myline=Colortune_getline()
    exec substitute(myline,Colortune_getregexpattern(),'hi \1 '.Colortune_getregexstring(),'')
    call Colortune_fill()
endfunction
" }}}

" Colortune_fill()
" {{{
" Create or Redraw the ColorTune Window/Buffer
"
function! Colortune_fill()

    " open a new split or switch to it (if already open)
    let potbufnr=bufwinnr(s:colortune_buf_name)
    if potbufnr==#-1
        execute 'vsplit '.s:colortune_buf_name
    else
	execute potbufnr.'wincmd w'
    endif

    " set some options
    setlocal filetype=colortune
    setlocal buftype=nofile
    setlocal nowrap

    " set mappings
    nnoremap <buffer> <cr> :call Colortune_setbyline()<cr>
    inoremap <buffer> <cr> <esc>:call Colortune_setbyline()<cr>

    nnoremap <buffer> X :call Colortune_clearbyline()<cr>
    nnoremap <buffer> M :call Colortune_togglemode()<cr>
    nnoremap <buffer> C :call Colortune_dumpconfig()<cr>

    nnoremap <buffer> Ar :call Colortune_manipulate_rgb(16,0,0)<cr>
    nnoremap <buffer> Sr :call Colortune_manipulate_rgb(-16,0,0)<cr>
    nnoremap <buffer> Ag :call Colortune_manipulate_rgb(0,16,0)<cr>
    nnoremap <buffer> Sg :call Colortune_manipulate_rgb(0,-16,0)<cr>
    nnoremap <buffer> Ab :call Colortune_manipulate_rgb(0,0,16)<cr>
    nnoremap <buffer> Sb :call Colortune_manipulate_rgb(0,0,-16)<cr>

    nnoremap <buffer> ar :call Colortune_manipulate_rgb(1,0,0)<cr>
    nnoremap <buffer> sr :call Colortune_manipulate_rgb(-1,0,0)<cr>
    nnoremap <buffer> ag :call Colortune_manipulate_rgb(0,1,0)<cr>
    nnoremap <buffer> sg :call Colortune_manipulate_rgb(0,-1,0)<cr>
    nnoremap <buffer> ab :call Colortune_manipulate_rgb(0,0,1)<cr>
    nnoremap <buffer> sb :call Colortune_manipulate_rgb(0,0,-1)<cr>
    

    nnoremap <buffer> VG :call Colortune_showguirgb()<cr>
    nnoremap <buffer> VC :call Colortune_showctermrgb()<cr>


    " set syntax highlighting for our group keywords
    for mycolor in s:colortable
	execute 'syn keyword '.mycolor.' '.mycolor
    endfor

    " remember current position and clean the buffer
    let top_line=line('w0')
    let cur_line=line('.')
    let save_cursor = getpos(".")
    normal! ggdG

    " display header with some infos
    call append(line('$')-1,'------------------------------------------')
    call append(line('$')-1,'Colortable Mode: ['.s:modes_labels[s:current_mode].']')
    call append(line('$')-1,'------------------------------------------')
    call append(line('$')-1,'ENTER  - Set from current line -----------')
    call append(line('$')-1,'X      - Unset current line --------------')
    call append(line('$')-1,'M      - Toggle Mode ---------------------')
    call append(line('$')-1,'C      - Dump Config to new buffer -------')
    call append(line('$')-1,'VC     - View palette  (cterm) -----------')
    call append(line('$')-1,'VG     - View palette  (gui) -------------')
    call append(line('$')-1,'a[rgb] - increase [rgb]] by 1 ------------')
    call append(line('$')-1,'A[rgb] - increase [rgb]] by 16 -----------')
    call append(line('$')-1,'s[rgb] - decrease [rgb]] by 1 ------------')
    call append(line('$')-1,'S[rgb] - decrease [rgb]] by 16 -----------')
    call append(line('$')-1,'------------------------------------------')
    call append(line('$')-1,'-')

    " find out the maximal length for each column
    let max_len=[Colortune_getmax('GROUP_NAME')]
    for column in s:modes[s:current_mode]
	let max_len+=[Colortune_getmax(column)]
    endfor
	
    " display column names
    let labels=Colortune_extend('GROUP_NAME'.' ',' ',5+max_len[0])
    let idx=1
    for column in s:modes[s:current_mode]
	let labels.='  '.Colortune_extend(column,' ',max_len[idx]).'  '
	let idx+=1
    endfor
    call append(line('$')-1,labels)
    call append(line('$')-1,'------------------------------------------')

    " paint the color table
    for group in s:colortable

	let colorline=Colortune_extend(group.' ','-',5+max_len[0])
	
	let idx=1
	for column in s:modes[s:current_mode]
	    let value=Colortune_getattr(group,column)
	    let colorline.=' ['.Colortune_extend(value,' ',max_len[idx]).'] '
	    let idx+=1
	endfor

	call append(line('$')-1,colorline)

    endfor

    " delete last (empty) line and restore initial position.
    normal ddgg
    exec 'normal G'.top_line.'G'.cur_line.'G'
    call setpos('.', save_cursor)

endfunction
" }}}

" Colortune_dumpconfig()
" {{{
"
function! Colortune_dumpconfig()

    new
    set ft=vim
    set nowrap

    " add header stuff for our colortheme
    call append(line('$')-1,[
	\   'set background=dark',
	\   'hi clear',
	\   'if exists("syntax_on")', 'syntax reset', 'endif',
	\   'let g:colors_name = "mycolorscheme"'])


    for group in s:colortable
	let config=''

	for column in s:modes[s:current_mode]
	    let value=Colortune_getattr(group,column)
	    let config.=' '.column.'='.value
	endfor
	call append(line('$')-1,'hi '.group.config)
    endfor

    normal gg

endfunction
" }}}

" Colortune_manipulate_rgb()
" {{{
" TODO: restore register!
" TODO: check overflow/underflow
" TODO: check format
"
function! Colortune_manipulate_rgb(r,g,b)

    normal T["adt]
    let val=getreg('a')
    let r='0x'.substitute(val,'^#\(\x\x\)\x\x\x\x','\1','')
    let g='0x'.substitute(val,'^#\x\x\(\x\x\)\x\x','\1','')
    let b='0x'.substitute(val,'^#\x\x\x\x\(\x\x\)','\1','')
    call setreg('a',printf("#%02x%02x%02x",r+a:r,g+a:g,b+a:b))
    normal "aP
    call Colortune_setbyline()

endfunction


" }}}

" Colortune_showguirgb()
"{{{
function! Colortune_showguirgb()

    vnew
    set nowrap
    nnoremap <buffer> <cr> :call Colortune_fillincolor()<cr>

    r $VIMRUNTIME/rgb.txt
    let linenr=1
    let result=[]
    while linenr<line('$')

	let myline=getline(linenr)
	if myline =~# '^\s*\d\+\s\+\d\+\s\+\d\+\s\+\w\+\s*$'
	    exec substitute(myline,'^\s*\(\d\+\)\s\+\(\d\+\)\s\+\(\d\+\)\s\+\(\w\+\)\s*$','let extr=["\4",\1,\2,\3]','')
	    exec 'hi color_'.extr[0].' guibg='.printf("#%02x%02x%02x",extr[1],extr[2],extr[3])
	    exec 'syntax match color_'.extr[0].' "color_'.extr[0].'"'
	    let result+=[extr[0].' ~ color_'.extr[0]]
	else
	endif

	let linenr+=1

    endwhile

    normal ggdG
    call append(0,result)


endfunction
"}}}

" Colortune_showctermrgb()
"{{{
function! Colortune_showctermrgb()

    vnew
    set nowrap
    nnoremap <buffer> <cr> :call Colortune_fillincolor()<cr>

    let colornr=0
    let result=[]
    while colornr<256

	exec 'hi color_'.colornr.' ctermbg='.colornr
	exec 'syntax match color_'.colornr.' "color_'.colornr.'"'
	let result+=[colornr.' ~ color_'.colornr]
	let colornr+=1

    endwhile

    call append(0,result)


endfunction
"}}}

" Colortune_fillincolor()
" {{{
function! Colortune_fillincolor()
    normal 0"ayaw
    let potbufnr=bufwinnr(s:colortune_buf_name)
    execute potbufnr.'wincmd w'
    normal di]"aP
    call Colortune_setbyline()
endfunction
" }}}

" }}}

