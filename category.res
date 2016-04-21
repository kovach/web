obj x [
  'cat .object x
]

arrow s arrow t [
  arrow.source s, arrow.target t
]

# fg = h (left to right composition convention)
comp f g h [
  comp.m01 f,
  comp.m12 g,
  comp.m02 h
]

#~[obj x, obj y, obj z, arrow x f y, arrow y g z, comp f g h]
~[obj x, obj y, arrow x f y]

#[arrow a f b, arrow a id a, comp id f f]
# make id_l
#~[[obj a] [arrow a id a, [arrow a f b] [comp id f f]]]
# make id_r
#~[[obj a] [arrow a id a, [arrow b f a] [comp f id f]]]


[arrow s f t]
