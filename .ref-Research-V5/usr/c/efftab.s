/ c code tables

.globl	_efftab

.data
_efftab=.
	30.;	ci30
	31.;	ci30
	32.;	ci30	/ same as 30
	33.;	ci30	/ same as 31
	80.;	ci80
	70.;	ci70
	71.;	ci70	/ - like +
	77.;	ci77
	78.;	ci78
	81.;	ci78
	75.;	ci75
	76.;	ci76
	0
.text

/ ++,-- prefix, postfix
ci30:
%ai,n
%abp,n
%ab,n
	IB1	A1

%aip,n
%adp,n
%afp,n
	I'	$^,A1

%nbp*,n
%ni*,n
%nb*,n
	F*
	IB1	#1(R)

%nip*,n
%ndp*,n
%nfp*,n
	F*
	I'	$^,#1(R)

/ =
ci80:
%a,z
%ad,zf
	clrB1	A1

%n*,z
%nd*,zf
	F*
	clrB1	#1(R)

%a,aw
%ab,a
	movBE	A2,A1

%ab,n*
%a,nw*
	S*
	movBE	#2(R),A1

%a,n
	S
	movB1	R,A1

%n*,aw
%nb*,a
	F*
	movBE	A2,#1(R)

%n*,ew*
%nb*,e*
	F*
	S1*
	movBE	#2(R1),#1(R)

%n*,e
	F*
	S1
	movB1	R1,#1(R)

%e*,nw*
%eb*,n*
	S*
	F1*
	movBE	#2(R),#1(R1)

%e*,n
	S
	F1*
	movB1	R,#1(R1)

%n*,nw*
%nb*,n*
	FS*
	S*
	movBE	#2(R),*(sp)+

%n*,n
	FS*
	S
	movBE	R,*(sp)+

/ =| and =& ~
ci78:
%a,a
	IBE	A2,A1

%a,n
	S
	IB1	R,A1

%n*,a
	F*
	IBE	A2,#1(R)

%e*,n*
	S*
	F1*
	IBE	#2(R),#1(R1)

%e*,n
	S
	F1*
	IBE	R,#1(R1)

%n*,e*
	F*
	S1*
	IBE	#2(R1),#1(R)

%n*,e
	F*
	S1
	IBE	R1,#1(R)

%n*,n*
	FS*
	S*
	IBE	#2(R),*(sp)+

%n*,n
	FS*
	S
	IBE	R,*(sp)+

/ =& i
ci77:
%a,c
	bicB1	$!C2,A1

%a,n
	S
	com	R
	bicB1	R,A1

%n*,c
	F*
	bicB1	$!C2,#1(R)

%e*,n
	S
	F1*
	com	R
	bicB1	R,#1(R1)

%n*,e
	F*
	S1
	com	R1
	bicB1	R1,#1(R)

%n*,n
	FS*
	S
	com	R
	bicB1	R,*(sp)+

/ =+
ci70:
%n*,z
%a,z

%a,1
	I'B1	A1

%aw,aw
	I	A2,A1

%aw,nw*
	S*
	I	#2(R),A1

%aw,n
	S
	I	R,A1

%n*,1
	F*
	I'B1	#1(R)

%ew*,nw*
	S*
	F1*
	I	#2(R),#1(R1)

%a,nw*
	S*
	movB1	A1',R1
	I	#2(R),R1
	movB1	R1,A1

%a,n
	S
	movB1	A1',R1
	I	R,R1
	movB1	R1,A1

%ew*,n
	S
	F1*
	I	R,#1(R1)

%nw*,n
	SS
	F*
	I	(sp)+,#1(R)

%n*,n
	SS
	F*
	movB1	#1(R),R1
	I	(sp)+,R1
	movB1	R1,#1(R)

/ =>>
ci75:
%a,1
	asrB1	A1

%n*,1
	F*
	asrB1	#1(R)

%r,c
	ash	$-C2,A1

%r,n
	S
	neg	R
	ash	R,A1

/ =<<
ci76:
%a,1
	aslB1	A1

%n*,1
	F*
	aslB1	#1(R)

%r,aw
	ash	A2,A1

%r,nw*
	S*
	ash	#2(R),A1

%r,n
	S
	ash	R,A1

.data
.even

