# a file consists of some number of definitions
# followed by some number of commands to be run

arrow s arrow t [
  arrow.source s, arrow.target t
]

# fg = h (left to right composition convention)
comp f g h [
  comp.m01 f,
  comp.m12 g,
  comp.m02 h
]

product a b p-fst p-snd [
  arrow p p-fst a, arrow p p-snd b,
  [arrow x x-fst a, arrow x x-snd b]
  ![arrow x factor p,
    comp factor p-fst x-fst,
    comp factor p-snd x-snd]
]

[arrow p p-fst a, arrow p p-snd b, del a b p]
[product a b pi1 pi2]
[comp 'id 'pi1 'pi1]
[comp 'h 'pi1 x]
[[comp 'h 'pi1 x]]
[arrow x fst 'a, arrow x snd 'b, arrow x factor 'ab]
[arrow s m t, ![arrow a b t]]
[x .target t, ![arrow a b t]]

# [a .s b, ~[a .t c]]
# ~[foo x y z]
# [arrow s f t, ~[arrow s id-s s, arrow t id-t t, comp id-s f f, comp f id-t f]]
# source s x [x.source s]
# target x t [x.target t]
# comp l r o [e.left l, e.right r, e.out o]
# comp s x x
# comp x t x
