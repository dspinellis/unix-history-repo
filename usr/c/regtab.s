/ c code tables-- compile to register

.globl	_regtab

.data
_regtab=.
	106.;	cr106
	30.;	cr30
	31.;	cr30
	32.;	cr32
	33.;	cr32
	37.;	cr37
	38.;	cr38
	98.;	cr100
	99.;	cr100
	80.;	cr80
	40.;	cr40
	41.;	cr40	/ - like +
	42.;	cr42
	43.;	cr43
	44.;	cr43
	45.;	cr45
	46.;	cr46
	47.;	cr47
	48.;	cr48
	49.;	cr49
	70.;	cr70
	71.;	cr70
	72.;	cr72
	73.;	cr73
	74.;	cr74
	75.;	cr75
	76.;	cr76
	77.;	cr77
	78.;	cr78
	81.;	cr78
	79.;	cr79
	102.;	cr102
	51.;	cr51
	52.;	cr52
	104.;	cr104
	0
.text

/ init expression
cr104:
%c,n
C1

%a,n
%af,n
A1

/ goto
cr102:
%i,n
	jmp	A1

%n*,n
	F*
	jmp	#1(R)

/ call
cr100:
%a,n
%a,nf
	jsr	pc,IA1

%n*,n
%n*,nf
	F*
	jsr	pc,#1(R)

%n,n
%n,nf
	F
	jsr	pc,(R)

/ addressible
cr106:
%z,n
	clr	R

%zf,n
	clrf	R

%a,n
%ad,n
	movB1	A1,R

%af,n
	movof	A1,R

%n*,n
%nd*,n
	F*
	movB1	#1(R),R

%nf*,n
	F*
	movof	#1(R),R

/++,-- prefix
cr30:
%ai,n
%abp,n
%ab,n
	IB1	A1'
	movB1	A1,R

%adp,n
%afp,n
%a,n
	I'	$^,A1'
	mov	A1,R

%nbp*,n
%ni*,n
%nb*,n
	F*
	IB1	#1(R)
	movB1	#1(R),R

%ndp*,n
%nfp*,n
%n*,n
	F*
	I'	$^,#1(R)
	mov	#1(R),R

/ ++,-- postfix
cr32:
%ai,n
%abp,n
%ab,n
	movB1	A1',R
	IB1	A1

%adp,n
%afp,n
%a,n
	mov	A1',R
	I'	$^,A1

%ebp*,n
%eb*,n
%ei*,n
	F1*
	movB1	#1(R1),R
	IB1	#1(R1)

%nbp*,n
%nb*,n
%ni*,n
	F*
	movB1	#1(R),-(sp)
	IB1	#1(R)
	movB1	(sp)+,R

%edp*,n
%efp*,n
%e*,n
	F1*
	mov	#1(R1),R
	I'	$^,#1(R1)

%ndp*,n
%nfp*,n
%n*,n
	F*
	mov	#1(R),-(sp)
	I'	$^,#1(R)
	mov	(sp)+,R

/ - unary
cr37:
%n,n
%nf,n
	F
	negBF	R

/ ~
cr38:
%n,n
	F
	com	R

/ =
cr80:
%a,n
%ad,nf
	S
	movB1	R,A1

%af,nf
	S
	movfo	R,A1

%nd*,af
	F*
	S
	movf	R,#1(R)

%n*,a
	F*
	movB1	A2,#1(R)
	movB1	#1(R),R

%nf*,af
	F*
	S
	movfo	R,#1(R)

%n*,e
	F*
	S1
	movB1	R1,#1(R)
	movB1	R1,R

%ed*,nf
	S
	F1*
	movf	R,#1(R1)

%ef*,nf
	S
	F1*
	movfo	R,#1(R1)

%n*,n
%nd*,nf
	FS*
	S
	movB1	R,*(sp)+

%nf*,nf
	FS*
	S
	movfo	R,*(sp)+

%a,nf
	S
	movfi	R,R
	movB1	R,A1

%e*,nf
	S
	F1*
	movfi	R,R
	movB1	R,#1(R1)

%n*,nf
	FS*
	S
	movfi	R,R
	movB1	R,*(sp)+

/ ^ -- xor
cr49:
%n,e
	F
	S1
	xor	R1,R

%n,n
	FS
	S
	xor	R,(sp)
	mov	(sp)+,R

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

/ >>
cr45:
%n,1
	F
	asr	R

%n,c
	F
	ash	$-C2,R

%n,e
	F
	S1
	neg	R1
	ash	R1,R

%n,n
	SS
	neg	(sp)
	F
	ash	(sp)+,R

/ <<
cr46:
%n,1
	F
	asl	R

%n,aw
	F
	ash	A2,R

%n,ew*
	F
	S1*
	ash	#1(R1),R

%n,e
	F
	S1
	ash	R1,R

%n,n
	SS
	F
	ash	(sp)+,R

/ +, -
cr40:
%n,z
	F

%n,1
	F
	I'	R

%n,aw
%nf,ad
	F
	IBF	A2,R

%n,ew*
%nf,ed*
	F
	S1*
	IBF	#2(R1),R

%n,e
%nf,ef
	F
	S1
	IBF	R1,R

%n,nw*
%nf,nd*
	SS*
	F
	IBF	*(sp)+,R

%n,n
%nf,nf
	SS
	F
	IBF	(sp)+,R

/ * -- R must be odd on integers
cr42:
%n,aw
%nf,ad
	F
	mulBF	A2,R

%n,ew*
%nf,ed*
	F
	S1*
	mulBF	#2(R1),R

%n,e
%nf,ef
	F
	S1
	mulBF	R1,R

%n,n
%nf,nf
	SS
	F
	mulBF	(sp)+,R

/ / R must be odd on integers
cr43:
%n,aw
	F
	T
	sxt	R-
	div	A2,R-

%n,ew*
	F
	T
	sxt	R-
	S1*
	div	#2(R1),R-

%n,e
	F
	T
	sxt	R-
	S1
	div	R1,R-

%n,n
	SS
	F
	T
	sxt	R-
	div	(sp)+,R-

%nf,ad
	F
	divf	A2,R

%nf,ed*
	F
	S1*
	divf	#2(R1),R

%nf,ef
	F
	S1
	divf	R1,R

%nf,nf
	SS
	F
	divf	(sp)+,R

/ =*
cr72:
%a,aw
%ad,ad
	movB1	A1',R
	mulBF	A2,R
	movB1	R,A1

%af,nf
	SS
	movof	A1',R
	mulf	(sp)+,R
	movfo	R,A1

%a,ew*
%ad,ed*
	movB1	A1',R
	S1*
	mulBF	#2(R1),R
	movB1	R,A1

%aw,n
%ad,n
	S
	mulBF	A1',R
	movBF	R,A1

%a,n
	SS
	movB1	A1',R
	mulBF	(sp)+,R
	movB1	R,A1

%nw*,n
%nd*,nf
	FS*
	S
	mulBF	*(sp),R
	movB1	R,*(sp)+

%n*,n
	FS*
	SS
	movB1	*2(sp),R
	mul	(sp)+,R
	movB1	R,*(sp)+

%nf*,nf
	FS*
	movof	*(sp),R
	movf	R,-(sp)
	S
	mulf	(sp)+,R
	movfo	R,*(sp)+

/ =/ ;  R must be odd on integers
cr73:
%a,aw
	movB1	A1',R
	sxt	R-
	divBF	A2,R-
	movB1	R-,A1

%a,n
	SS
	movB1	A1',R
	sxt	R-
	div	(sp)+,R-
	movB1	R-,A1

%e*,n
	SS
	F1*
	movB1	#1(R1),R
	sxt	R-
	div	(sp)+,R-
	movB1	R-,#1(R1)

%n*,n
	FS*
	SS
	movB1	*2(sp),R
	sxt	R-
	div	(sp)+,R-
	movB1	R-,*(sp)+

%ad,ad
	movf	A1',R
	divf	A2,R
	movf	R,A1

%ad,ef
	movf	A1',R
	S1
	divf	R1,R
	movf	R,A1

%ad,nf
	SS
	movf	A1',R
	divf	(sp)+,R
	movf	R,A1

%af,nf
	SS
	movof	A1',R
	divf	(sp)+,R
	movfo	R,A1

%nd*,nf
	FS*
	SS
	movf	*8(sp),R
	divf	(sp)+,R
	movf	R,*(sp)+

%nf*,nf
	FS*
	SS
	movof	*8(sp),R
	divf	(sp)+,R
	movfo	R,*(sp)+

/ =mod; R must be odd on integers
cr74:
%a,aw
	movB1	A1',R
	sxt	R-
	div	A2,R-
	movB1	R,A1

%a,n
	SS
	movB1	A1',R
	sxt	R-
	div	(sp)+,R-
	movB1	R,A1

%e*,n
	SS
	F1*
	movB1	#1(R1),R
	sxt	R-
	div	(sp)+,R-
	movB1	R,#1(R1)

%n*,n
	FS*
	SS
	movB1	*2(sp),R
	sxt	R-
	div	(sp)+,R-
	mov	R,*(sp)+

/ =| and =& ~
cr78:
%a,a
	IBE	A2,A1'
	movB1	A1,R

%a,n
	S
	IB1	R,A1'
	movB1	A1,R

%n*,a
	F*
	IBE	A2,#1(R)
	movB1	#1(R),R

%e*,n*
	S*
	F1*
	IBE	#2(R),#1(R1)
	movB1	#2(R1),R

%e*,n
	S
	F1*
	IBE	R,#1(R1)
	movB1	#1(R1),R

%n*,e*
	F*
	S1*
	IBE	#2(R1),#1(R)
	movB1	#1(R),R

%n*,e
	F*
	S1
	IBE	R1,#1(R)
	movB2	#1(R),R

%n*,n*
	FS*
	S*
	IBE	#2(R),*(sp)
	movB2	*(sp)+,R

%n*,n
	FS*
	S
	IBE	R,*(sp)
	mov	*(sp)+,R

/ =^ -- =xor
cr79:
%aw,n
	S
	xor	R,A1'
	mov	A1,R

%ab,n
	SS
	movb	A1',R
	xor	R,(sp)
	mov	(sp)+,R
	movb	R,A1

%n*,n
	FS*
	movB1	*(sp),-(sp)
	S
	xor	R,(sp)
	movB1	(sp)+,R
	movB1	R,*(sp)+

/ =&
cr77:
%a,c
	bicB1	$!C2,A1'
	movB2	A1,R

%a,n
	S
	com	R
	bicB1	R,A1'
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

/ =>>
cr75:
%a,c
	movB1	A1',R
	ash	$-C2,R
	movB1	R,A1

%a,n
	SS
	movB1	A1',R
	neg	(sp)
	ash	(sp)+,R
	movB1	R,A1

%n*,n
	SS
	F1*
	movB1	#1(R1),R
	neg	(sp)
	ash	(sp)+,R
	movB1	R,#1(R1)

/ =<<
cr76:
%a,aw
	movB1	A1',R
	ash	A2,R
	movB1	R,A1

%a,n
	SS
	movB1	A1',R
	ash	(sp)+,R
	movB1	R,A1

%n*,n
	SS
	F1*
	movB1	#1(R1),R
	ash	(sp)+,R
	movB1	R,#1(R1)

/ =+
cr70:
%aw,aw
	I	A2,A1'
	mov	A1,R

%aw,nw*
	S*
	I	#2(R),A1'
	mov	A1,R

%aw,n
	S
	I	R,A1'
	mov	A1,R

%ew*,nw*
	S*
	F1*
	I	#2(R),#1(R1)
	mov	#1(R1),R


%a,n
%ad,nf
	SS
	movB1	A1',R
	IBF	(sp)+,R
	movB1	R,A1

%af,nf
	SS
	movof	A1,R
	IBF	(sp)+,R
	movfo	R,A1

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
%nd*,nf
	SS
	F*
	movB1	#1(R),R1
	IBF	(sp)+,R1
	movB1	R1,#1(R)
	movBF	R1,R

%nf*,nf
	SS
	F*
	movof	#1(R),R1
	IBF	(sp)+,R1
	movfo	R1,#1(R)
	movf	R1,R

/ int -> float
cr51:
%aw,n
	movif	A1,R

%nw*,n
	F*
	movif	#1(R),R

%n,n
	F
	movif	R,R

/ float, double -> int
cr52:
%nf,n
	F
	movfi	R,R

.data
.even
.text

