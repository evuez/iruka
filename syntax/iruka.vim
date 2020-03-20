if exists('b:current_syntax') | finish | endif

syntax match term /term: / nextgroup=termValue
syntax match termValue /.\+/ contained
syntax match extra /\(tags\|color\|image\|link\): / nextgroup=extraValue
syntax match extraValue /.\+/ contained
syntax match multi /---/

hi def link term Statement
hi def link termValue Underlined
hi def link extra Constant
hi def link extraValue Comment
hi def link multi Statement

let b:current_syntax = 'iruka'
