/ c code tables-- expression to -(sp)

.globl	_sptab

.data
_sptab=.
	106.;	cs106
	30.;	cs30
	31.;	cs30
	32.;	cs32
	33.;	cs32
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
	mov	A1,-(sp)

%nw*,n
	F*
	mov	#1(R),-(sp)

/ ++,-- prefix
cs30:
%nbp*,n
%ni*,n
	F*
	I	#1(R)
	mov	#1(R),-(sp)

%nip*,n
	F*
	I'	$^,#1(R)
	mov	#1(R),-(sp)

/ ++,-- postfix
cs32:
%nbp*,n
%ni*,n
	F*
	mov	#1(R),-(sp)
	I	#1(R)

%nip*,n
	F*
	mov	#1(R),-(sp)
	I'	$^,#1(R)

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
