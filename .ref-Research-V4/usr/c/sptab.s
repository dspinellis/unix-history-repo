/ c code tables-- expression to -(sp)

.globl	_sptab

.data
_sptab=.
	106.;	cs106
	30.;	cs30
	31.;	cs30
	32.;	cs32
	33.;	cs32
	35.;	cs35
	36.;	cs36
	40.;	cs40
	41.;	cs40
	47.;	cs47
	48.;	cs48
	0
.text


/ name
cs106:
%z,n
%zf,n
	clrB1	-(sp)

%aw,n
	movB1	A1,-(sp)

/ ++,-- prefix
cs30:
%ai,n
%abp,n
	I	A1
	mov	A1,-(sp)

%aw,n
	I'	$^,A1
	mov	A1,-(sp)

%nbp*,n
%ni*,n
	F*
	I	#1(R)
	mov	#1(R),-(sp)

%nip*,n
	F*
	mov	#1(R),-(sp)
	I'	$^,#1(R)

/ ++,-- postfix
cs32:
%ai,n
%abp,n
	mov	A1,-(sp)
	I	A1

%aip,n
	mov	A1,-(sp)
	I'	$^,A1

%nbp*,n
%ni*,n
	F*
	mov	#1(R),-(sp)
	I	#1(R)

%nip*,n
	F*
	mov	#1(R),-(sp)
	I'	$^,#1(R)

/ & unary
cs35:
%i,n
	mov	$A1,-(sp)

/ * unary
cs36:
%nbp*,n
	F*
	movb	*#1(R),R
	mov	R,-(sp)

%ndp*,n
	F*
	movf	*#1(R),R
	movf	R,-(sp)

%nfp*,n
	F*
	movof	*#1(R),R
	movf	R,-(sp)

%n*,n
	F*
	mov	*#1(R),-(sp)

%abp,n
	movb	*A1,R
	mov	R,-(sp)

%adp,n
	movf	*A1,R
	movf	R,-(sp)

%afp,n
	movof	*A1,R
	movf	R,-(sp)

%a,n
	mov	*A1,-(sp)

%nbp,n
	H*
	movb	~(R),R
	mov	R,-(sp)

%ndp,n
	H*
	movf	~(R),R
	movf	R,-(sp)

%nfp,n
	H*
	movof	~(R),R
	movf	R,-(sp)

%n,n
	H*
	mov	~(R),-(sp)

/ +
cs40:
%n,1
	FS
	I'	(sp)

%n,aw
	FS
	I	A2,(sp)

%n,nw*
	FS
	S*
	I	#2(R),(sp)

%n,n
	FS
	S
	I	R,(sp)

/ &
cs47:
%n,c
	FS
	bic	$!C2,(sp)

%n,n
	FS
	S
	com	R
	bic	R,(sp)

/ |
cs48:
%n,a
	FS
	bisB2	A2,(sp)

%n,n*
	FS
	S*
	bisB2	#2(R),(sp)

%n,n
	FS
	S
	bis	R,(sp)

.data
.even
.text
