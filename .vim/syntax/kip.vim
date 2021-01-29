" Quit when a syntax file was already loaded.
if exists('b:current_syntax') | finish|  endif

syntax keyword kipKeyword Bir ya olabilir var olamaz değilse yazdır diyelim
syn region   kipEncl transparent matchgroup=kipKeyword start="(" matchgroup=kipKeyword end=")" contains=ALLBUT,@kipContained,kipParenErr
syn region   kipComment start="(\*" end="\*)" contains=@Spell,kipComment,kipTodo

hi def link kipComment	   Comment
hi def link kipKeyword	   Keyword

let b:current_syntax = 'kip'
