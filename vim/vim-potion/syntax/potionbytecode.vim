" minimalistic syntax highlighting for testing purposes

if exists("b:current_syntax")
    finish
endif

syntax keyword potionKeyword code assign


syntax match potionNumber "\v\d+"
syntax region potionString start=/\v"/ skip=/\v\\./ end=/\v"/


highlight link potionString String
highlight link potionKeyword Keyword
highlight link potionNumber Number

let b:current_syntax = "potionbytecode"
