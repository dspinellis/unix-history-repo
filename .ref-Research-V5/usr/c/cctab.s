/ c code tables-- set condition codes

.globl	_cctab

.data
_cctab=.
	106.;	cc60
	28.;	rest
	34.;	rest
	35.;	rest
	36.;	rest
	37.;	rest
	40.;	rest
	41.;	rest
	42.;	rest
	43.;	rest
	45.;	rest
	46.;	rest
	47.;	cc47
	48.;	rest
	60.;	cc60
	61.;	cc60
	62.;	cc60
	63.;	cc60
	64.;	cc60
	65.;	cc60
	66.;	cc60
	67.;	cc60
	68.;	cc60
	69.;	cc60
	72.;	rest
	73.;	rest
	79.;	rest
	0
.text

/ relationals
cc60:
%a,z
%ad,zf
	tstB1	A1

%af,z
	movof	A1,R

%n*,z
%nd*,zf
	F*
	tstB1	#1(R)

%nf*,z
	F*
	movof	#1(R),R

%n,z
%nf,zf
	FC

%aw,aw
%ab,ab
	cmpBE	A1,A2

%nw*,aw
%nb*,ab
	F*
	cmpBE	#1(R),A2

%n,aw
%nf,ad
	F
	V
	cmpB2	A2,R

%nw*,ew*
%nb*,eb*
	F*
	S1*
	cmpBE	#1(R),#2(R1)

%nw*,e
%nd*,ef
	F*
	S1
	cmpB1	#1(R),R1

%n,ew*
%nf,ed*
	F
	S1*
	V
	cmpB2	#2(R1),R

%n,e
%nf,ef
	F
	S1
	cmpBF	R,R1

%nw*,nw*
%nb*,nb*
	FS*
	S*
	cmpBE	*(sp)+,#2(R)

%nw*,n
%nd*,nf
	FS*
	S
	cmpB1	*(sp)+,R

%n,nw*
	FS
	S*
	cmp	(sp)+,#2(R)

%n,n
%nf,nf
	SS
	F
	V
	cmpBF	(sp)+,R

/ & as in "if ((a&b) ==0)"
cc47:
%a,a
	bitBE	A2,A1

%n*,a
	F*
	bitBE	A2,#1(R)

%n,a
	F
	bitB2	A2,R

%n,e
	F
	S1
	bit	R1,R

%n,n
	FS
	S
	bit	(sp)+,R

/ set codes right
rest:
%n,n
%nf,nf
	H

.data
.even
.text

