.\" @(#)ref.ms	1.3 (Berkeley) %G%
.	\" refer macros for citations
.de []
.][ \\$1
..
.de ][
.if \\$1>4 .tm %M undefined (Bell Labs)
.[\\$1
..
.if n .ds [o \&"
.if n .ds [c \&"
.if t .ds [o ``
.if t .ds [c ''
.ds <. .
.ds <, ,
.	\" [0 - other type of citation
.de [0
.FS
.nr [: 0
.if !\\*([F .FP \\*([F
.if !\\*([Q \{\
.nr [: 1
\\*([Q\c
.\}
.if !\\*([A \{\
.nr [: 1
\\*([A\c
.\}
.if !\\*([T \{\
.if \\n([:>0 ,
.nr [: 1
\f2\\*([T\f1\c
.\}
.if !\\*([S , \\*([S\c
.if !\\*([V , \\*([V\c
.if !\\*([P \{\
.ie \\n([P>0 , pp. \\*([P\c
.el , p. \\*([P\c
.\}
.if !\\*([C , \\*([C\c
.if !\\*([D , \\*([D\c
.if \\n([:>0 \&.
.if !\\*([O \\*([O
.FE
..
.	\" [1 - journal article
.de [1
.FS
.if !\\*([F .FP \\*([F
.if !\\*([Q \\*([Q,
.if !\\*([A \\*([A,
.if !\\*([T \\*([o\\*([T,\\*([c
\f2\\*([J\f1\c
.if !\\*([V , vol. \\*([V\c
.if !\\*([N , no. \\*([N\c
.if !\\*([P \{\
.ie \\n([P>0 , pp. \\*([P\c
.el , p. \\*([P\c
.\}
.if !\\*([I , \\*([I\c
.if !\\*([C , \\*([C\c
.if !\\*([D , \\*([D\c
\&.
.if !\\*([O \\*([O
.FE
..
.	\" [2 - book
.de [2
.FS
.if !\\*([F .FP \\*([F
.if !\\*([Q \\*([Q,
.if !\\*([A \\*([A,
.if !\\*([T \f2\\*([T,\f1
.if !\\*([S \\*([S,
.if !\\*([V \\*([V,
.if !\\*([P \{\
.ie \\n([P>0 pp. \\*([P,
.el p. \\*([P,
.\}
\\*([I\c
.if !\\*([C , \\*([C\c
.if !\\*([D , \\*([D\c
\&.
.if !\\*([O \\*([O
.FE
..
.	\" [3 - article in book
.de [3
.FS
.if !\\*([F .FP \\*([F
.if !\\*([Q \\*([Q,
.if !\\*([A \\*([A,
.if !\\*([T \\*([o\\*([T,\\*([c
in \f2\\*([B\f1\c
.if !\\*([E , ed. \\*([E\c
.if !\\*([S , \\*([S\c
.if !\\*([V , vol. \\*([V\c
.if !\\*([P \{\
.ie \\n([P>0 , pp. \\*([P\c
.el , p. \\*([P\c
.\}
.if !\\*([I , \\*([I\c
.if !\\*([C , \\*([C\c
.if !\\*([D , \\*([D\c
\&.
.if !\\*([O \\*([O
.FE
..
.	\" [4 - report
.de [4
.FS
.if !\\*([F .FP \\*([F
.if !\\*([Q \\*([Q,
.if !\\*([A \\*([A,
.if !\\*([T \\*([o\\*([T,\\*([c
.if !\\*([R \\*([R\c
.if !\\*([G \& (\\*([G)\c
.if !\\*([P \{\
.ie \\n([P>0 , pp. \\*([P\c
.el , p. \\*([P\c
.\}
.if !\\*([I , \\*([I\c
.if !\\*([C , \\*([C\c
.if !\\*([D , \\*([D\c
\&.
.if !\\*([O \\*([O
.FE
..
.de ]<
.[<
..
.de [<
.SH
References
.LP
.de FP
.IP \\\\$1.
\\..
.rm FS FE
..
.de [>
.]>
..
.de ]>
.sp
..
.de ]-
.[-
..
.de [-
.rm [Q [A [T [J [B [E [S [V
.rm [N [P [I [C [D [O [R [G
..
.de ]]
.\" circumvent EOF bug in troff
..
