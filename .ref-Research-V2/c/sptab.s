/ c code tables-- expression to -(sp)

.globl	_sptab

_sptab=.;.+2
	20.;	cs20
	21.;	cs21
	22.;	cs21
	30.;	cs30
	31.;	cs30
	32.;	cs32
	33.;	cs32
	35.;	cs35
	36.;	cs36
	40.;	cs40
	41.;	cs40
	42.;	cs42
	47.;	cs47
	48.;	cs48
	0


/ name
cs20:
%aw,n
	mov	A,-(sp)

/ constant
cs21:
%z,n
	clr	-(sp)

%a,n
	mov	A,-(sp)

/ ++,-- prefix
cs30:
%ai,n
%abp,n
	I	A1
	mov	A1,-(sp)

%aw,n
	I'	$2,A1
	mov	A1,-(sp)

%nbp*,n
%ni*,n
	F*
	I	#1(R)
	mov	#1(R),-(sp)

%nip*,n
	F*
	mov	#1(R),-(sp)
	I'	$2,#1(R)

/ ++,-- postfix
cs32:
%ai,n
%abp,n
	mov	A1,-(sp)
	I	A1

%aip,n
	mov	A1,-(sp)
	I'	$2,A1

%nbp*,n
%ni*,n
	F*
	mov	#1(R),-(sp)
	I	#1(R)

%nip*,n
	F*
	mov	#1(R),-(sp)
	I'	$2,#1(R)

/ & unary
cs35:
%i,n
	mov	$A1,-(sp)

/ * unary
cs36:
%aw,n
	mov	*A1,-(sp)

%nw*,n
	F*
	mov	#1(R),-(sp)

/ +
cs40:
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

/ *
cs42:
%aw,a
	mov	A1,(r4)+
	movB2	A2,(r4)
	mov	-(r4),-(sp)

%n,a
	F
	mov	R,(r4)+
	movB2	A2,(r4)
	mov	-(r4),-(sp)

%n,nw*
	FS
	S*
	mov	(sp)+,(r4)+
	mov	#2(R),(r4)
	mov	-(r4),-(sp)

%n,n
	FS
	S
	mov	(sp)+,(r4)+
	mov	R,(r4)
	mov	-(r4),-(sp)

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
