-- a file consists of some number of definitions
-- followed by some number of commands to be run

arrow s arrow t [
  arrow .source s, arrow .target t
]

-- fg = h (left to right composition convention)
comp f g h [
  comp .m01 f,
  comp .m12 g,
  comp .m02 h
]

-- TODO make a [forall _] [unique _] clause so this definition is ok
product a b p-fst p-snd [
  arrow p p-fst a, arrow p p-snd b,
  [arrow x x-fst a, arrow x x-snd b]
  ![arrow x factor p,
    comp factor p-fst x-fst,
    comp factor p-snd x-snd]
]

-- [s source t]
-- [arrow s ar1 c, arrow c ar2 a]
-- [arrow s arr t]

[arrow p p-fst a, arrow p p-snd b, del a b p]
[product a b pi1 pi2]
[comp 'id 'pi1 'pi1]
[comp 'h 'pi1 x]
[[comp 'h 'pi1 x]]
[arrow x fst 'a, arrow x snd 'b, arrow x factor 'ab]
[arrow s m t, ![arrow a b t]]
[x .target t, ![arrow a b t]]
