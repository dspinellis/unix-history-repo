
/ c code tables-- compile to register

.globl	_regtab

_regtab=.; .+2
	20.;	cr20
	21.;	cr20
	22.;	cr20
	30.;	cr30
	31.;	cr30
	32.;	cr32
	33.;	cr32
	34.;	cr34
	35.;	cr35
	29.;	cr29
	36.;	cr36
	37.;	cr37
	38.;	cr38
	101.;	cr100
	80.;	cr80
	40.;	cr40
	41.;	cr40	/ - like +
	42.;	cr42
	43.;	cr43
	44.;	cr43
	45.;	cr45
	46.;	cr45
	47.;	cr47
	48.;	cr48
	60.;	cr60
	61.;	cr60
	62.;	cr60
	63.;	cr60
	64.;	cr60
	65.;	cr60
	66.;	cr60
	67.;	cr60
	68.;	cr60
	69.;	cr60
	70.;	cr70
	71.;	cr70
	72.;	cr72
	73.;	cr73
	74.;	cr73
	75.;	cr75
	76.;	cr75
	77.;	cr77
	78.;	cr78
	102.;	cr102
	97.;	cr97
{	84.;	cr84
	85.;	cr84
	86.;	cr86
	87.;	cr87
	88.;	cr88
	89.;	cr87
	94.;	cr84
	95.;	cr84
	96.;	cr87}
	0

/ goto
cr102:
%i,n
	jmp	*A1

%n*,n
	F*
	jmp	*#1(R)

%n,n
	F
	jmp	(R)

/ call
cr100:
%n*,n
	F*
	jsr	pc,*#1(R)

%a,n
	jsr	pc,*A1

%n,n
	F
	jsr	pc,(R)

/ name, constant
cr20:
%z,n
{	M}
	clrB1	R

%aw,n
	mov	A,R

%ab,n
	movb	A,R

{%af,n
	M
	movf	A,R}

/++,-- prefix
cr30:
%ai,n
%abp,n
%ab,n
	IB1	A1
	movB1	A1,R

%a,n
	I'	$2,A1
	mov	A1,R

%nbp*,n
%ni*,n
%nb*,n
	F*
	IB1	#1(R)
	movB1	#1(R),R

%n*,n
	F*
	I'	$2,#1(R)
	mov	#1(R),R

{%af,n
	M
	movfi	$1,R
	I'f	R,A1
	movf	A1,R

%nf*,n
	F*
	M
	movfi	$1,R
	I'f	R,#1(R)
	movf	#1(R),R}

/ ++,-- postfix
cr32:
%ai,n
%abp,n
%ab,n
	movB1	A1,R
	IB1	A1

%a,n
	mov	A1,R
	I'	$2,A1

%nbp*,n
%nb*,n
%ni*,n
	F*
	movB1	#1(R),-(sp)
	IB1	#1(R)
	movB1	(sp)+,R

%n*,n
	F*
	mov	#1(R),-(sp)
	I'	$2,#1(R)
	mov	(sp)+,R

{%af,n
	M
	movf	A1,R
	movf	R,-(sp)
	movif	$1,R
	I'f	R,A1
	movf	(sp)+,R

%nf*,n
	F*
	M
	movf	#1(R),R
	movf	R,-(sp)
	movif	$1,R
	I'f	R,#1(R)
	movf	(sp)+,R}

/ !
cr34:
%n,n
	FC
	beq	1f
	clr	R
	br	2f
1:	mov	$1,R
2:

/ &unary
cr35:
%a,n
%af,n
	mov	$A1,R

/ & unary of auto
cr29:
%e,n
%ef,n
	mov	r5,R
	add	Z,R

/ *unary
cr36:
%abp*,n
	F
	movb	(R),R

%a*,n
	F
	mov	(R),R

%abp,n
	movb	*A1,R

%a,n
	mov	*A1,R

%nbp*,n
	F*
	movb	*#1(R),R

%n*,n
	F*
	mov	*#1(R),R

%nbp,n
	H*
	movb	~(R),R

%n,n
	H*
	mov	~(R),R

/ - unary
cr37:
%n,n
	F
	neg	R

{%nf,n
	F
	negf	R}

/ ~
cr38:
%n,n
	F
	com	R

/ =
cr80:
%a,n
{%af,nf}
	S
	movB1	R,A1

%n*,a
{%nf*,af}
	F*
{	M}
	movB1	A2,#1(R)
	movB1	#1(R),R

%n*,e
{%nf*,ef}
	F*
	S1
	movB1	R1,#1(R)
	movB1	R1,R

%n*,n
{%nf*,nf}
	FS*
	S
	movB1	R,*(sp)+

/ |
cr48:
%n,a
	F
	bisB2	A2,R

%n,e*
	F
	S1*
	bisB2	#2(R1),R

%n,e
	F
	S1
	bis	R1,R

%n,n
	FS
	S
	bis	(sp)+,R

/ &
cr47:
%n,c
	F
	bic	$!C2,R

%n,e
	F
	S1
	com	R1
	bic	R1,R

%n,n
	FS
	S
	com	(sp)
	bic	(sp)+,R

/ relationals
cr60:
%n,n
	HC
	I	2f
	clr	R
	br	1f
2:	mov	$1,R
1:

/ >>, <<
cr45:
%a,aw
	movB1	A1,I'
	I	A2,lsh
	movB1	I',R

%n*,aw
	F*
	movB1	#1(R),I'
	I	A2,lsh
	movB1	I',R

%n,aw
	F
	mov	R,I'
	I	A2,lsh
	mov	I',R

%a,nw*
	S*
	movB1	A1,(r4)
	I	#2(R),lsh
	mov	(r4),R

%a,n
	S
	movB1	A1,I'
	I	R,lsh
	mov	I',R

%n,n
	FS
	S
	mov	(sp)+,I'
	I	R,lsh
	mov	I',R

/ +, -
cr40:
%n,aw
{%nf,af}
	F
	IB1	A2,R

%n,ew*
{%nf,ef*}
	F
	S1*
	IB2	#2(R1),R

%n,e
{%nf,ef}
	F
	S1
	IBF	R1,R

%n,nw*
{%nf,nf*}
	SS*
	F
	IBF	*(sp)+,R

%n,n
{%nf,nf}
	SS
	F
	IBF	(sp)+,R

/ *
cr42:
%aw,a
	mov	A1,(r4)+
	movB2	A2,(r4)
	mov	-(r4),R

%n,a
	F
	mov	R,(r4)+
	movB2	A2,(r4)
	mov	-(r4),R

%n,e
	F
	S1
	mov	R,(r4)+
	mov	R1,(r4)
	mov	-(r4),R

%n,n
	FS
	S
	mov	(sp)+,(r4)+
	mov	R,(r4)
	mov	-(r4),R

{%nf,af
	F
	mulf	A2,R

%nf,ef*
	F
	S1*
	mulf	#2(R1),R

%nf,ef
	F
	S1
	mulf	R1,R

%nf,nf
	FS
	S
	mulf	(sp)+,R}

/ /; mod
cr43:
%a,a
	movB1	A1,(r4)
	movB2	A2,div
	mov	I,R

%a,n
	S
	movB1	A1,(r4)
	mov	R,div
	mov	I,R

%n,a
	F
	mov	R,(r4)
	movB2	A2,div
	mov	I,R

%n,e
	F
	S1
	mov	R,(r4)
	mov	R1,div
	mov	I,R

%e,n
	S
	F1
	mov	R1,(r4)
	mov	R,div
	mov	I,R

%n,n
	FS
	S
	mov	(sp)+,(r4)
	mov	R,div
	mov	I,R

{%nf,af
	F
	I'	A1,R

%nf,ef*
	F
	S1*
	I'	#2(R1),R

%nf,ef
	F
	S1
	I'	R1,R

%nf,nf
	SS
	F
	I'	(sp)+,R}

/ =*
cr72:
%a,a
	movB1	A1,(r4)
	movB2	A2,mul
	movB1	(r4),A1
	mov	(r4),R

%a,n
	S
	mov	R,(r4)+
	movB1	A1,(r4)
	mov	-(r4),R
	movB1	R,A1

%n*,a
	F*
	movB1	#1(R),(r4)
	movB2	A2,mul
	movB1	(r4),#1(R)
	mov	(r4),R

%n*,e
	F*
	S1
	movB1	#1(R),(r4)
	mov	R1,mul
	movB1	(r4),#1(R)
	mov	(r4),R

%e*,n
	S
	F1*
	movB1	#1(R1),(r4)
	mov	R,mul
	movB1	(r4),#1(R1)
	mov	(r4),R

%n*,n
	FS*
	S
	movB1	*(sp),(r4)
	mov	R,mul
	movB1	(r4),*(sp)+
	mov	(r4),R

/ =mod, =/
cr73:
%a,a
	movB1	A1,(r4)
	movB2	A2,div
	movB1	I,A1
	mov	I,R

%a,n
	S
	movB1	A1,(r4)
	mov	R,div
	mov	I,R
	movB1	R,A1

%n*,a
	F*
	movB1	#1(R),(r4)
	movB2	A2,div
	movB1	I,#1(R)
	mov	I,R

%n*,e
	F*
	S1
	movB1	#1(R),(r4)
	mov	R1,div
	movB1	I,#1(R)
	mov	I,R

%e*,n
	S
	F1*
	movB1	#1(R1),(r4)
	mov	R,div
	movB1	I,#1(R1)
	mov	I,R

%n*,n
	FS*
	S
	movB1	*(sp),(r4)
	mov	R,div
	movB1	I,*(sp)+
	mov	I,R

/ =|
cr78:
%a,a
	bisBE	A2,A1
	movB1	A1,R

%a,n
	S
	bisB1	R,A1
	movB1	A1,R

%n*,a
	F*
	bisBE	A2,#1(R)
	movB1	#1(R),R

%e*,n*
	S*
	F1*
	bisBE	#1(R1),#2(R)
	movB1	#2(R),R

%e*,n
	S
	F1*
	bisBE	R,#1(R1)
	movB1	#1(R1),R

%n*,e*
	F*
	S1*
	bisBE	#2(R1),#1(R)
	movB1	#1(R),R

%n*,e
	F*
	S1
	bisBE	R1,#1(R)
	movB2	#1(R),R

%n*,n*
	FS*
	S*
	bisBE	#2(R),*(sp)
	movB2	*(sp)+,R

%n*,n
	FS*
	S
	bisBE	R,*(sp)
	mov	*(sp)+,R

/ =&
cr77:
%a,c
	bicB1	$!C2,A1
	movB2	A1,R

%a,n
	S
	com	R
	bicB1	R,A1
	movB1	A1,R

%e*,n
	S
	F1*
	com	R
	bicB1	R,#1(R1)
	movB1	#1(R1),R

%n*,e
	F*
	S1
	com	R1
	bicB1	R1,#1(R)
	movB1	#1(R),R

%n*,n
	FS*
	S
	com	R
	bicB1	R,*(sp)
	movB1	*(sp)+,R

/ =>>, =<<
cr75:
%a,aw
	movB1	A1,I'
	I	A2,lsh
	movB1	I',A1
	movB1	I',R

%a,n
	S
	movB1	A1,I'
	I	R,lsh
	movB1	I',A1
	movB1	I',R

%n*,e
	F*
	S1
	movB1	#1(R),I'
	I	R1,lsh
	movB1	I',#1(R)
	movB1	I',R

%e*,n
	S
	F1*
	movB1	#1(R1),I'
	I	R,lsh
	movB	I',#1(R1)
	movB1	I',R

%n*,n
	FS*
	S
	movB1	*(sp),I'
	I	R,lsh
	movB1	I',*(sp)+
	movB1	I',R

/ =+
cr70:
%aw,aw
	I	A2,A1
	mov	A1,R

%aw,nw*
	S*
	I	#2(R),A1
	mov	A1,R

%aw,n
	S
	I	R,A1
	mov	A1,R

%ew*,nw*
	S*
	F1*
	I	#2(R),#1(R1)
	mov	#1(R1),R

%a,nw*
	S*
	movB1	A1,R1
	I	#2(R),R1
	movB1	R1,#2(R)
	mov	R1,R

%a,n
	S
	movB1	A1,R1
	I	R1,R
	movB1	R,A1

%ew*,n
	S
	F1*
	I	R,#1(R1)
	mov	#1(R1),R

%nw*,n
	SS
	F*
	I	(sp)+,#1(R)
	mov	#1(R),R

%n*,n
	SS
	F*
	movB1	#1(R),R1
	I	(sp)+,R1
	movB1	R1,#1(R)
	mov	R1,R

/ int -> int[]
cr97:
%n,n
	F
	asl	R

{/ int -> float
cr84:
%a,n
	M
	movif	A1,R

%n*,n
	F*
	M
	movif	#1(R),R

%n,n
	F
	movif	R,R

/ float -> double
cr87:
%af,n
	M
	movof	A1,R

%nf*,n
	F*
	M
	movof	#1(R),R

%nf,n
	F
	movof	R,R

/ float -> int
cr86:
%af,n
	M2
	movfi	A1,R

%nf*,n
	F*
	M2
	movfi	\$1(R),R

%nf,n
	F
	movfi	R,R

/ double -> int
cr88:
%ad,n
	M4
	movfi	A1,R

%nd*,n
	F*
	M4
	movfi	#1(R),R

%nd,n
	F
	movfi	R,R

.data
.even
.text

