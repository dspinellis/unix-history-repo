.sp 100
.nr PS 9
.nr VS 11
.	\" START OF Figures
.de P1
.nf
.in +.5i
.ta .5i 1i 1.5i 2i 2.5i 3i 3.5i 4i 4.5i 5i
.sp
.ps 9
.vs 11p
..
.de P2
.sp
.fi
.ps \\n(PS
.vs \\n(VS
.in -.5i
..
.SH
Figure 1:  C program with pointer bug
.LP
.P1
struct buf {
	int fildes;
	int nleft;
	char *nextp;
	char buff[512];
	}bb;
struct buf *obuf;

char *charp "this is a sentence.";

main(argc,argv)
int argc;
char **argv;
{
	char	cc;

	if(argc < 2) {
		printf("Input file missing\\n");
		exit(8);
	}

	if((fcreat(argv[1],obuf)) < 0){
		printf("%s : not found\\n", argv[1]);
		exit(8);
	}
	charp = \'T\';
printf("debug 1 %s\\n",charp);
	while(cc=  *charp++)
		putc(cc,obuf);
	fflush(obuf);
}
.P2
.sp 100
.SH
Figure 2:  ADB output for C program of Figure 1
.LP
.P1
.ft B
adb a.out core
$c
.ft R
~main(02,0177762)
.ft B
$C
.ft R
~main(02,0177762)
	argc:	    02
	argv:	    0177762
	cc:	    02124
.ft B
$r
.ft R
ps	0170010
pc	0204	~main+0152
sp	0177740
r5	0177752
r4	01
r3	0
r2	0
r1	0
r0	0124
~main+0152:	mov	_obuf,(sp)
.ft B
$e
.ft R
savr5:	    0
_obuf:	    0
_charp:	    0124
_errno:	    0
_fout:	    0
.ft B
$m
.ft R
text map    \`ex1\'
b1 = 0		     e1	= 02360		  f1 = 020
b2 = 0		     e2	= 02360		  f2 = 020
data map    \`core1\'
b1 = 0		     e1	= 03500		  f1 = 02000
b2 = 0175400	     e2	= 0200000		  f2 = 05500
.ft B
*charp/s
.ft R
0124:		TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTLx	Nh@x&_
~
.ft B
charp/s
.ft R
_charp:		T
 
_charp+02:	this is a sentence.
 
_charp+026:	Input file missing
.ft B
main.argc/d
.ft R
0177756:	2
.ft B
*main.argv/3o
.ft R
0177762:	0177770	0177776	0177777
.ft B
0177770/s
.ft R
0177770:	a.out
.ft B
*main.argv/3o
.ft R
0177762:	0177770	0177776	0177777
.ft B
*"/s
.ft R
0177770:	a.out
.ft B
 .=o
.ft R
		0177770
.ft B
 .\(mi10/d
.ft R
0177756:	2
.ft B
$q
.P2
.sp 100
.SH
Figure 3:  Multiple function C program for stack trace illustration
.LP
.P1
int	fcnt,gcnt,hcnt;
h(x,y)
{
	int hi; register int hr;
	hi = x+1;
	hr = x\(miy+1;
	hcnt++ ;
	hj:
	f(hr,hi);
}

g(p,q)
{
	int gi; register int gr;
	gi = q\(mip;
	gr = q\(mip+1;
	gcnt++ ;
	gj:
	h(gr,gi);
}

f(a,b)
{
	int fi; register int fr;
	fi = a+2*b;
	fr = a+b;
	fcnt++ ;
	fj:
	g(fr,fi);
}

main()
{
	f(1,1);
}
.P2
.sp 100
.SH
Figure 4:  ADB output for C program of Figure 3
.LP
.P1
.ft B
adb
$c
.ft R
~h(04452,04451)
~g(04453,011124)
~f(02,04451)
~h(04450,04447)
~g(04451,011120)
~f(02,04447)
~h(04446,04445)
~g(04447,011114)
~f(02,04445)
~h(04444,04443)
.ft B
HIT DEL KEY
.ft R
adb
.ft B
,5$C
.ft R
~h(04452,04451)
	x:	    04452
	y:	    04451
	hi:	    ?
~g(04453,011124)
	p:	    04453
	q:	    011124
	gi:	    04451
	gr:	    ?
~f(02,04451)
	a:	    02
	b:	    04451
	fi:	    011124
	fr:	    04453
~h(04450,04447)
	x:	    04450
	y:	    04447
	hi:	    04451
	hr:	    02
~g(04451,011120)
	p:	    04451
	q:	    011120
	gi:	    04447
	gr:	    04450
.ft B
fcnt/d
.ft R
_fcnt:		1173
.ft B
gcnt/d
.ft R
_gcnt:		1173
.ft B
hcnt/d
.ft R
_hcnt:		1172
.ft B
h.x/d
.ft R
022004:		2346
.ft B
$q
.P2
.sp 100
.SH
Figure 5:  C program to decode tabs
.LP
.P1
#define MAXLINE	80
#define YES		1
#define NO		0
#define TABSP		8
.sp .5
char	input[] "data";
char	ibuf[518];
int	tabs[MAXLINE];
.sp .5
main()
{
	int col, *ptab;
	char c;
.sp .5
	ptab = tabs;
	settab(ptab);	/*Set initial tab stops */
	col = 1;
	if(fopen(input,ibuf) < 0) {
		printf("%s : not found\\n",input);
		exit(8);
	}
	while((c = getc(ibuf)) != \(mi1) {
		switch(c) {
			case \(fm\\t\(fm:	/* TAB */
				while(tabpos(col) != YES) {
					putchar(\(fm \(fm);	/* put BLANK */
					col++ ;
				}
				break;
			case \(fm\\n\(fm:	/*NEWLINE */
				putchar(\(fm\\n\(fm);
				col = 1;
				break;
			default:
				putchar(c);
				col++ ;
		}
	}
}
.sp .5
/* Tabpos return YES if col is a tab stop */
tabpos(col)
int col;
{
	if(col > MAXLINE)
		return(YES);
	else
		return(tabs[col]);
}
.sp .5
/* Settab - Set initial tab stops */
settab(tabp)
int *tabp;
{
	int i;
.sp .5
	for(i = 0; i<= MAXLINE; i++) 
		(i%TABSP) ? (tabs[i] = NO) : (tabs[i] = YES);
}
.P2
.sp 100
.SH
Figure 6a:  ADB output for C program of Figure 5
.LP
.P1
.ft B
adb a.out \(mi
settab+4:b
fopen+4:b
getc+4:b
tabpos+4:b
$b
.ft R
breakpoints
count	bkpt		command
1	~tabpos+04
1	_getc+04
1	_fopen+04
1	~settab+04
.ft B
settab,5?ia
.ft R
~settab:		jsr	r5,csv
~settab+04:	tst	\(mi(sp)
~settab+06:	clr	0177770(r5)
~settab+012:	cmp	$0120,0177770(r5)
~settab+020:	blt	~settab+076
~settab+022:
.ft B
settab,5?i
.ft R
~settab:		jsr	r5,csv
		tst	\(mi(sp)
		clr	0177770(r5)
		cmp	$0120,0177770(r5)
		blt	~settab+076
.ft B
:r
.ft R
a.out: running
breakpoint	~settab+04:	tst	\(mi(sp)
.ft B
settab+4:d
:c
.ft R
a.out: running
breakpoint	_fopen+04:	mov	04(r5),nulstr+012
.ft B
$C
.ft R
_fopen(02302,02472)
~main(01,0177770)
	col:	    01
	c:	    0
	ptab:	    03500
.ft B
tabs,3/8o
.ft R
03500:		01	0	0	0	0	0	0	0
		01	0	0	0	0	0	0	0
		01	0	0	0	0	0	0	0
.P2
.sp 100
.SH
Figure 6b:  ADB output for C program of Figure 5
.LP
.P1
.ft B
:c
.ft R
a.out: running
breakpoint	_getc+04:	mov	04(r5),r1
.ft B
ibuf+6/20c
.ft R
__cleanu+0202:		This	is	a test	of
.ft B
:c
.ft R
a.out: running
breakpoint	~tabpos+04:	cmp	$0120,04(r5)
.ft B
tabpos+4:d
settab+4:b  settab,5?ia
settab+4:b  settab,5?ia;  0
getc+4,3:b  main.c?C;  0
settab+4:b  settab,5?ia;  ptab/o;  0
$b
.ft R
breakpoints
count	bkpt		command
1	~tabpos+04
3	_getc+04	main.c?C;0
1	_fopen+04
1	~settab+04	settab,5?ia;ptab?o;0
~settab:		jsr	r5,csv
~settab+04:	bpt
~settab+06:	clr	0177770(r5)
~settab+012:	cmp	$0120,0177770(r5)
~settab+020:	blt	~settab+076
~settab+022:
0177766:	0177770
0177744:	@\`
T0177744:	T
h0177744:	h
i0177744:	i
s0177744:	s
.P2
.sp 100
.SH
Figure 7:  ADB output for C program with breakpoints
.LP
.in +.5i
.nf
.ps 8
.vs 9
.ft B
adb ex3 \(mi
h+4:b hcnt/d; h.hi/; h.hr/
g+4:b gcnt/d; g.gi/; g.gr/
f+4:b fcnt/d; f.fi/; f.fr/
:r
.ft R
ex3: running
_fcnt:		0
0177732:	214
symbol not found
.ft B
f+4:b fcnt/d; f.a/; f.b/; f.fi/
g+4:b gcnt/d; g.p/; g.q/; g.gi/
h+4:b hcnt/d; h.x/; h.y/; h.hi/
:c
.ft R
ex3: running
_fcnt:		0
0177746:	1
0177750:	1
0177732:	214
_gcnt:		0
0177726:	2
0177730:	3
0177712:	214
_hcnt:		0
0177706:	2
0177710:	1
0177672:	214
_fcnt:		1
0177666:	2
0177670:	3
0177652:	214
_gcnt:		1
0177646:	5
0177650:	8
0177632:	214
.ft B
HIT DEL
f+4:b fcnt/d; f.a/"a = "d; f.b/"b = "d; f.fi/"fi = "d
g+4:b gcnt/d; g.p/"p = "d; g.q/"q = "d; g.gi/"gi = "d
h+4:b hcnt/d; h.x/"x = "d; h.y/"h = "d; h.hi/"hi = "d
:r
.ft R
ex3: running
_fcnt:		0
0177746:	a = 1
0177750:	b = 1
0177732:	fi = 214
_gcnt:		0
0177726:	p = 2
0177730:	q = 3
0177712:	gi = 214
_hcnt:		0
0177706:	x = 2
0177710:	y = 1
0177672:	hi = 214
_fcnt:		1
0177666:	a = 2
0177670:	b = 3
0177652:	fi = 214
.ft B
HIT DEL
$q
.in -.5i
.sp 100
.SH
Figure 8:  ADB address maps
.LP
.de l1
.tc
.ta 1.20i +1.6i +2.5i
..
.de l3
.tc
.ta 1.6i +2.80i +.2i +1.55i
..
.de l2
.tc
.ti 1.0i
.ta +0.5i +3.0i +1.75i
.tc _
..
.de l5
.tc 
.ti 1.0i
.ta +0.75i +3.0i +1.5i
.tc _
..
.de l6
.tc
.ti 1.0i
.ta +.8i +2.85i +0.4i +1.1i
..
.de l8
.tc
.ti 1.0i
.ta +0.5i +3.0i +1.75i
.tc _
..
.de la
.tc
.ta 1.20i +1.25i +1.7i
..
.de lc
.tc
.ti 1.0i
.ta +.85i +1.6i +.35i +1.1i
..
.de lb
.tc
.ti 1.0i
.ta +0.75i +1.75i +1.5i
.tc _
..
.ul
407 files
.sp
.l1
a.out	hdr	text+data
.l2
|	|	|
.l3
	0	D
.sp
.l1
core	hdr	text+data	stack
.l5
|	|	......|	|
.l6
	0	D	S	E
.sp 2
.ul
410 files (shared text)
.sp
.l1
a.out	hdr	 text	data
.l2
|	|	|	|
.l3
	0	T	B	D
.sp
.la
core	hdr	data	stack
.lb
|	|	......|	|
.lc
	B	D	S	E
.sp 2
.ul
411 files (separated I and D space)
.sp
.l1
a.out	hdr	text	data
.l2
|	|	|	|
.l3
	0	T	0	D
.sp
.la
core	hdr	data	stack
.lb
|	|	......|	|
.lc
	0	D	S	E
.sp 2
The following 
.ul 
adb
variables are set.
.nf
.ta .75i 1.5i 3.5i 4.5i 5.5i
.sp
			407	410	411
.sp
	b	base of data	0	B	0
	d	length of data	D	D\(miB	D
	s	length of stack	S	S	S
	t	length of text	0	T	T
.sp 100
.SH
Figure 9:  ADB output for maps
.LP
.nf
.in +.5i
.ft B
adb map407 core407
$m
.ft R
text map    \`map407\'
b1 = 0		     e1	= 0256		  f1 = 020
b2 = 0		     e2	= 0256		  f2 = 020
data map    \`core407\'
b1 = 0		     e1	= 0300		  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02300
.ft B
$v
.ft R
variables
d = 0300
m = 0407
s = 02400
.ft B
$q
.sp 2
adb map410 core410
$m
.ft R
text map    \`map410\'
b1 = 0		     e1	= 0200		  f1 = 020
b2 = 020000	     e2	= 020116	  f2 = 0220
data map    \`core410\'
b1 = 020000	     e1	= 020200	  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02200
.ft B
$v
.ft R
variables
b = 020000
d = 0200
m = 0410
s = 02400
t = 0200
.ft B
$q
.sp 2
adb map411 core411
$m
.ft R
text map    \`map411\'
b1 = 0		     e1	= 0200		  f1 = 020
b2 = 0		     e2	= 0116		  f2 = 0220
data map    \`core411\'
b1 = 0		     e1	= 0200		  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02200
.ft B
$v
.ft R
variables
d = 0200
m = 0411
s = 02400
t = 0200
.ft B
$q
.in -.5i
.sp 100
.SH
Figure 10:  Simple C program for illustrating formatting and patching
.LP
.P1
char	str1[]	"This is a character string";
int	one	1;
int	number	456;
long	lnum	1234;
float	fpt	1.25;
char	str2[]	"This is the second character string";
main()
{
	one = 2;
}
.P2
.sp 100
.SH
Figure 11:  ADB output illustrating fancy formats
.LP
.nf
.ps 9
.vs 11p
.ft B
adb map410 core410
<b,\(mi1/8ona
.ft R
020000:		0	064124	071551	064440	020163	020141	064143	071141
.sp .5
_str1+016:	061541	062564	020162	072163	064562	063556	0	02
.sp .5
_number:
_number:	0710	0	02322	040240	0	064124	071551	064440
.sp .5
_str2+06:	020163	064164	020145	062563	067543	062156	061440	060550
.sp .5
_str2+026:	060562	072143	071145	071440	071164	067151	0147	0
.sp .5
savr5+02:	0	0	0	0	0	0	0	0
.sp .5
.ft B
<b,20/4o4^8Cn
.ft R
020000:		0	064124	071551	064440	@\`@\`This i
		020163	020141	064143	071141	s a char
		061541	062564	020162	072163	acter st
		064562	063556	0	02	ring@\`@\`@b@\`
.sp .5
_number:	0710	0	02322	040240	H@a@\`@\`R@d @@
		0	064124	071551	064440	@\`@\`This i
		020163	064164	020145	062563	s the se
		067543	062156	061440	060550	cond cha
		060562	072143	071145	071440	racter s
		071164	067151	0147	0	tring@\`@\`@\`
		0	0	0	0	@\`@\`@\`@\`@\`@\`@\`@\`
		0	0	0	0	@\`@\`@\`@\`@\`@\`@\`@\`
data address not found
.ft B
<b,20/4o4^8t8cna
.ft R
020000:		0	064124	071551	064440		This i
_str1+06:	020163	020141	064143	071141		s a char
_str1+016:	061541	062564	020162	072163		acter st
_str1+026:	064562	063556	0	02		ring
_number:
_number:	0710	0	02322	040240		HR
_fpt+02:	0	064124	071551	064440		This i
_str2+06:	020163	064164	020145	062563		s the se
_str2+016:	067543	062156	061440	060550		cond cha
_str2+026:	060562	072143	071145	071440		racter	s
_str2+036:	071164	067151	0147	0		tring
savr5+02:	0	0	0	0
savr5+012:	0	0	0	0
data address not found
.ft B
<b,10/2b8t^2cn
.ft R
020000:		0	0
.sp .5
_str1:		0124	0150		Th
		0151	0163		is
		040	0151		 i
		0163	040		s
		0141	040		a
		0143	0150		ch
		0141	0162		ar
		0141	0143		ac
		0164	0145		te
.ft B
$Q
.sp 100
.SH
Figure 12:  Directory and inode dumps
.LP
.nf
.ft B
adb dir \(mi
=nt"Inode"t"Name"
0,\(mi1?ut14cn
.ft R

		Inode	Name
0:		652	.
		82	..
		5971	cap.c
		5323	cap
		0	pp
.sp 4
.ft B
adb /dev/src \(mi
.ft B
02000>b
?m<b
.ft R
new map	    \`/dev/src\'
b1 = 02000	     e1	= 0100000000	  f1 = 0
b2 = 0		     e2	= 0		  f2 = 0
.ft B
$v
.ft R
variables
b = 02000
.ft B
<b,\(mi1?"flags"8ton"links,uid,gid"8t3bn"size"8tbrdn"addr"8t8un"times"8t2Y2na
.ft R
02000:		flags	073145
		links,uid,gid	0163	0164	0141
		size	0162	10356
		addr	28770	8236	25956	27766	25455	8236	25956	25206
		times	1976 Feb 5 08:34:56	1975 Dec 28 10:55:15

02040:		flags	024555
		links,uid,gid	012	0163	0164
		size	0162	25461
		addr	8308	30050	8294	25130	15216	26890	29806	10784
		times	1976 Aug 17 12:16:51	1976 Aug 17 12:16:51

02100:		flags	05173
		links,uid,gid	011	0162	0145
		size	0147	29545
		addr	25972	8306	28265	8308	25642	15216	2314	25970
		times	1977 Apr 2 08:58:01	1977 Feb 5 10:21:44
.\"
.\"	Start of Summary
.sp 100
.TL
ADB Summary
.LP
.LP
.if t .2C
.nr VS 9
.nr VS 11
.SH
Command Summary
.LP
.ta .7i
a)   formatted printing
.sp .5
.IP "\fB? \fIformat\fR" .7i
print from \fIa.out\fR file according to \fIformat\fR
.IP "\fB/ \fIformat\fR" .7i
print from \fIcore\fR file according to \fIformat\fR
.IP "\fB= \fIformat\fR" .7i
print the value of \fIdot\fR
.sp .5
.IP "\fB?w\fR expr" .7i
write expression into \fIa.out\fR file
.IP "\fB/w\fR expr" .7i
write expression into \fIcore\fR file
.sp .5
.IP "\fB?l\fR expr" .7i
locate expression in \fIa.out\fR file
.LP
.ta .7i
b)   breakpoint and program control
.LP
.ta .7i
.nf
.ta .7i
\fB:b\fR	set breakpoint at \fIdot\fR
\fB:c\fR	continue running program
\fB:d\fR	delete breakpoint
\fB:k\fR	kill the program being debugged
\fB:r\fR	run \fIa.out\fR file under ADB control
\fB:s\fR	single step
.LP
.ta .7i
c)   miscellaneous printing
.LP
.ta .7i
.nf
\fB$b\fR	print current breakpoints
\fB$c\fR	C stack trace
\fB$e\fR	external variables
\fB$f\fR	floating registers
\fB$m\fR	print ADB segment maps
\fB$q\fR	exit from ADB
\fB$r\fR	general registers
\fB$s\fR	set offset for symbol match
\fB$v\fR	print ADB variables
\fB$w\fR	set output line width
.LP
.ta .7i
d)   calling the shell
.LP
.ta .7i
.nf
\fB!\fR	call \fIshell\fP to read rest of line
.LP
.ta .7i
e)   assignment to variables
.LP
.ta .7i
.nf
\fB>\fIname\fR	assign dot to variable or register \fIname\fR
.sp 100
.SH
Format Summary
.LP
.ta .7i
.nf
\fBa	\fRthe value of dot
\fBb	\fRone byte in octal
\fBc	\fRone byte as a character
\fBd	\fRone word in decimal
\fBf	\fRtwo words in floating point
\fBi	\fRPDP 11 instruction
\fBo	\fRone word in octal
\fBn	\fRprint a newline
\fBr	\fRprint a blank space
\fBs	\fRa null terminated character string
\fIn\fBt	\fRmove to next \fIn\fR space tab
\fBu	\fRone word as unsigned integer
\fBx	\fRhexadecimal
\fBY	\fRdate
\fB^	\fRbackup dot
\fB"..."\fR	print string
.LP
.ta .7i
.SH
Expression Summary
.LP
.ta .7i
a) expression components
.LP
.ta .1.1i
.nf
\fBdecimal integer	\fRe.g. 256
\fBoctal integer	\fRe.g. 0277
\fBhexadecimal	\fRe.g. #ff
\fBsymbols	\fRe.g. flag  _main  main.argc
\fBvariables	\fRe.g. <b
\fBregisters	\fRe.g. <pc <r0
\fB(expression)	\fRexpression grouping
.LP
.ta .7i
b) dyadic operators
.LP
.ta .7i
.nf
\fB+\fP	add
\fB\(mi\fP	subtract
\fB*\fP	multiply
\fB%\fP	integer division
\fB&\fP	bitwise and
\fB|\fP	bitwise or
\fB#\fP	round up to the next multiple
.LP
.ta .7i
c) monadic operators
.LP
.ta .7i
.nf
\v'.25m'\s+2\fB~\fP\s0\v'-.25m'	not
\fB*\fR	contents of location
\fB\(mi\fR	integer negate
.fi
