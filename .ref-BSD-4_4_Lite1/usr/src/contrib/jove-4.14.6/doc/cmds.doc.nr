.de bp
..
.de NH
..
.de IQ
"\\$1"
..
.de dc
.sp 1
:entry "\\$1"
.if '\\$2'(variable)' "Variable"
.if !'\\$2'(variable)' "Command"
.br
..
.de ID
.sp 1
.in +5
..
.de DE
.fi
.sp 1
.in -5
..
.de DS
.nf
.sp 1
.in +5
..
.de UX
UNIX\c
..
.ll 7i
.na	\" No Adjust: ragged right looks best on fixed-pitch screen
.nh	\" No Hyphenation: best with ragged
