

(dc dc 1 (basic))
 (dc word {id} {attribute-list})
Dc (for "define comment") allows the association of comments with atoms in much
the same way as de or dv associate function definitions and values.  It is very
similar to the CMULisp dc from which it was copied.  The first argument is the
atom with which you want to associate the comment.  The optional id identifies
the comment - you are allowed to give an atom multiple comments.  The attribute
list is supposed to describe the comment in the same way as the help messages
are described.  This could be used by helpfilter to decide whether to print the
comment.  Both of the optional arguments default to nil.  

(dc dc 2 (general))
Dc actually calls a function named by (the value of) dc-switch.  The normal
value of dc-switch is dc-define, which prints a message, reads text from the
terminal and stores a comment property.  Dc-switch is rebound by the help
function to dc-help, a function that decides whether to print the comment
(by asking helpfilter) and then either does or does not.  Dskin rebinds
dc-switch to dc-dskin which is like dc-define without the prompt.  From this
it follows that (1) unlike CMULisp, all comments are stored in core, and
(2) you can program dc yourself just by setting dc-switch to the name of a
function that you supply.

(dc dc 3 (detail))
Comments are stored in a format similar to that of CMULisp.  The comment
property of an atom is a list of comments, each consisting of an id, an
attribute list and text.  However, due to a restriction on the size of
strings or atoms that Franz can read, the text is stored as a list of
lines.  As in CMULisp, a comment can be changed just by redefining it
(i.e. creating another comment for the same atom with the same id).  In
order to  delete comments or change the order in which they are printed
it is necessary to edit the comment property.  Comments are pretty-printed
(by pp-comment) in the reverse of their order in the comment property.
(New comments are added to the front, so this keeps them in the order of
their definition.)
