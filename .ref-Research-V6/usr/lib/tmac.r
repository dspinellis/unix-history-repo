.de HD
.if t .tl '\(rn'''
.if t 'sp  \\n(m1-1
.if n 'sp \\n(m1
.ps 10
.ft R
.if e .1e
.if o .1o
.ps
.ft
'sp \\n(m2
.if \\n(:n .nm 1 1 2
.ns
..
.wh 0 HD
.de FT
'sp \\n(m3
.ps 10
.ft R
.if e .2e
.if o .2o
.ps
.ft
'bp
..
.wh -7 FT (1inch)
.de m1
.nr m1 \\$1
..
.de m2
.nr m2 \\$1
..
.de m3
.nr m3 \\$1
.ch FT -\\n(m3-\\n(m4-2
..
.de m4
.nr m4 \\$1
.ch FT -\\n(m3-\\n(m4-2
..
.m1 3
.m2 2
.m3 2
.m4 3
.de he
.de 1e
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
.de 1o
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.de fo
.de 2e
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
.de 2o
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.de eh
.de 1e
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.de oh
.de 1o
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.de ef
.de 2e
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.de of
.de 2o
.tl \\$1 \\$2 \\$3 \\$4 \\$5 \\$6 \\$7 \\$8 \\$9
\\..
..
.he ''''
.fo ''''
.de bl
.rs
.sp \\$1
..
.de n1
.nm \\$1 1 2
.nr :n 0
.if \\n(.$ .nr :n 1
..
.de n2
.nm \\$1 1 2
..
.if n .ll 60
.if n .lt 60
.if t .ll 6i
.if t .lt 6i
