/ c code tables

.globl	_efftab

_efftab=.;.+2
	30.;	ci30
	31.;	ci30
	32.;	ci30	/ same as 30
	33.;	ci30	/ same as 31
	80.;	ci80
	70.;	ci70
	71.;	ci70	/ - like +
	77.;	ci77
	78.;	ci78
	0

/ ++ prefix
ci30:
%ai,n
%abp,n
%ab,n
	IB1	A1

%aip,n
	I'	$2,A1

%nbp*,n
%ni*,n
%nb*,n
	F*
	IB1	#1(R)

%nip*,n
	F*
	I'	$2,#1(R)

/ =
ci80:
%a,z
	clrB1	A1

%n*,z
	F*
	clrB1	#1(R)

%a,aw
	movB1	A2,A1

%a,nw*
	S*
	movB1	#2(R),A1

%a,n
	S
	movB1	R,A1

%n*,aw
	F*
	movB1	A2,#1(R)

%n*,ew*
	F*
	S1*
	movB1	#2(R1),#1(R)

%n*,e
	F*
	S1
	movB1	R1,#1(R)

%e*,nw*
	S*
	F1*
	movB1	#2(R),#1(R1)

%e*,n
	S
	F1*
	movB1	R,#1(R1)

%n*,nw*
	FS*
	S*
	movB1	#2(R),*(sp)+

%n*,n
	FS*
	S
	movB1	R,*(sp)+

/ =| i
ci78:
%a,a
	bisBE	A2,A1

%a,n
	S
	bisB1	R,A1

%n*,a
	F*
	bisBE	A2,#1(R)

%e*,n*
	S*
	F1*
	bisBE	#2(R),#1(R1)

%e*,n
	S
	F1*
	bisBE	R,#1(R1)

%n*,e*
	F*
	S1*
	bisBE	#2(R1),#1(R)

%n*,e
	F*
	S1
	bisBE	R1,#1(R)

%n*,n*
	FS*
	S*
	bisBE	#2(R),*(sp)+

%n*,n
	FS*
	S
	bisBE	R,*(sp)+

/ =& i
ci77:
%a,c
	bicB1	$!C2,A1

%a,n
	S
	com	R
	bicB1	R,A1

%e*,n
	S
	F1*
	com	R
	bicB1	R,#1(R1)

%n*,c
	F*
	bicB1	$!C2,#1(R)

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
%aw,aw
	I	A2,A1

%aw,nw*
	S*
	I	#2(R),A1

%aw,n
	S
	I	R,A1

%ew*,nw*
	S*
	F1*
	I	#2(R),#1(R1)

%a,nw*
	S*
	movB1	A1,R1
	I	#2(R),R1
	movB1	R1,#2(R)

%a,n
	S
	movB1	A1,R1
	I	R1,R
	movB1	R,A1

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

.data
.even

