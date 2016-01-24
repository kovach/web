syntax match comm "^#.*$"
syntax match sep ","
syntax match at "\v\@(\w|-)*"
syntax match sym "\v'(\w|-)+"
highlight link comm Comment
highlight link sep Function
highlight link at Label
highlight link sym TypeDef
