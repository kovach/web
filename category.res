obj cat x [
  cat .object x
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

# ~[obj x, obj y, arrow x f y]

~[a .foo b, arrow a b c]
~[[a .foo b] [b .bar a]]

# [arrow a b c]
# ~[[arrow u f v, arrow v g w] [arrow u h w, comp f g h]]
