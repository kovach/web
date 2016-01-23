# # is comment character
# a;b means if a result nonempty, return it, otherwise try b
# . means end def
# a <- b means take the external label assigned to b and apply it to a.
#   the label might be some text data, or an image
# 'foo is a pattern referring to a unique entity named by foo
# @ is application of a named relation
# ~ separates a query from an effect, which may reference bindings from the query
# complete whitespace insensitivity. just for token separation

# std-lib contains +, >
# @+ x y z
#   id fst x, id snd y, id sum z.

text-editor elem text
  ; elem type 'text_area, elem text text
  ; elem type 'node_editor, elem text text
.

key_press e key
  e type 'key, e key key.

# shift+enter on text element -> create new element labelled with text
create_node e node
  @key_press e 'enter, e mod 'shift, e elem ed,
  ed type 'node_editor, ed text t, ed pos p
    ~ new node, node <- t, node pos p, del e, del ed.

# lol
contains elem pos
  elem pos epos, elem rect r
  r width w, r height h
  epos x ex, epos y ey
  pos x x, pos y y
  @> x ex, @> y ey,
  @+ ex w ex', @+ ey h ey'
  @> ex' x, @> ey' y .

# possibly crucial syntax additions:
#  1. C [ a.prop ] -> a prop freshname, C [ freshname ]
#     and general chains (a.b.c, a.b.c.d, ...)
#  2. functional nesting of @'s with ( ). last param becomes return value
# so the above becomes
# contains elem pos
#   @> pos.x elem.pos.x, @> pos.y elem.pos.y
#   @> (@+ elem.pos.x elem.rect.width) pos.x
#   @> (@+ elem.pos.y elem.rect.height) pos.y
# .

# TODO handle multiple containing elems?
e type 'mouseDown, 'root mode 'not_dragging, e pos p, @contains elem p
  ~ 'root mode 'dragging, 'root target elem, del e.

e type 'mouseMove, 'root mode 'dragging, e pos p, 'root target elem
  ~ elem pos p, del e.

e type 'mouseUp, 'root mode 'dragging, 'root target elem
  ~ 'root mode 'not_dragging, del e.

