.(l C
.sz 12
.b
ADDENDUM TO THE \-ME
REFERENCE MANUAL
.sz
.r
.sp
(Applying to documentation dated September 29, 1978)
.sp
Version 1.1, Mod 4
.)l
.sp 3
.ba 4n
.nr so 0-4n
.uh "New Request: .uh"
.pp
The request
.b .uh
.i T
will print unnumbered headings.
This is preferable to
.b ".sh 0"
in a number of ways.
.uh "Parameters changed to .$p"
.pp
Because of the request above,
there are now three parameters
to
.b .$p .
The first is the section title,
the second is the section number,
and the third is the section depth.
.uh "Correction to .sh Section"
.pp
It is
.i not
legal to perform a
.b .af
request on the number registers
.b \en($1
through
.b \en($6 .
