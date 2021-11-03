function! potion#running#PotionCompileAndRunFile()
    write
    silent !clear
    execute "!" . g:potion_command . " " . bufname("%")
endfunction

function! potion#running#PotionShowBytecode()
    
    write

    " Get the bytecode.
    let bytecode = system(g:potion_command . " -c -V " . bufname("%") . " 2>&1")

    " Check returned string for errors.
    if bytecode=~#"Syntax error"
	let bytecode = "SYNTAX ERROR!! Check your potion."
    endif

    " Open a new split and set it up or use existing one!
    let potbufnr=bufwinnr('__Potion_Bytecode__')
    if potbufnr==#-1
        vsplit __Potion_Bytecode__
    else
	execute potbufnr.'wincmd w'
    endif
    normal! ggdG
    setlocal filetype=potionbytecode
    setlocal buftype=nofile

    " Insert the bytecode.
    call append(0, split(bytecode, '\v\n'))

endfunction
