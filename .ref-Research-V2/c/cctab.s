/ c code tables-- set condition codes

.globl	_cctab

_cctab=.;.+2
	20.;	rest
	21.;	rest
	22.;	rest
	30.;	rest
	31.;	rest
	34.;	rest
	35.;	rest
	36.;	rest
	37.;	rest
	40.;	rest
	41.;	rest
	42.;	rest
	43.;	rest
	44.;	rest
	45.;	rest
	46.;	rest
	47.;	rest
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
	70.;	rest
	71.;	rest
	72.;	rest
	73.;	rest
	74.;	rest
	75.;	rest
	76.;	rest
	77.;	rest
	78.;	rest
	79.;	rest
	80.;	rest

/ relationals
cc60:
%a,z
	tstB1	A1

%n*,z
	F*
	tstB1	#1(R)

%n,z
	F
	tst	R

%a,a
	cmpBE	A1,A2

%n*,a
	F*
	cmpBE	#1(R),A2

%n,a
	F
	cmpB2	R,A2

%n*,e*
	F*
	S1*
	cmpBE	#1(R),#2(R1)

%n*,e
	F*
	S1
	cmpB1	#1(R),R1

%n,e*
	F
	S1*
	cmpB2	R,#2(R1)

%n,e
	F
	S1
	cmp	R,R1

%n*,n*
	FS*
	S*
	cmpBE	(sp)+,#2(R)

%n*,n
	FS*
	S
	cmpB1	*(sp)+,R

%n,n*
	FS
	S*
	cmpB2	(sp)+,#2(R)

%n,n
	FS
	S
	cmp	(sp)+,R

/ set codes right
rest:
%n,n
	H

.data
.even
.text

