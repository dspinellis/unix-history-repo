. \" Macros for the index
.de Ib	\" blank major entry
.br
.ne 2v
\\$1:
..
.de I>	\" major entry
\\$1, \\$2
..
.de I<  \" minor entry
.br
   \\$2, \\$3
..
.de LB	\" new letter starts here
.di DT	\" start diverted text
.sp
.sz +2
.b
\\$1
.r
.sz -2
.sp
.di	\" end diverted text
.ne \\n(dnu+1v	\" get enough space for it
.DT		\" output it
..
.\" set up various parameters for the right evironment.
.\" Your taste may be different.
.ef 'C Language X Interface''X Version 10'
.of 'C Language X Interface''X Version 10'
.eh ''- % -''
.oh ''- % -''
.\" Set the page number for the index properly.
.pn 61
.++ A
.po 1.0i	\" physical offset
.ta 5iR		\" right alignment tab
.lp		\" initialize -me
.nf
.ce
.sz 18
Index
.sp 1
.sz 10
.2c		\" 2 column mode
.sp 3
