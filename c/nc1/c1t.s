/ C operator tables

.globl	_getwrd

.globl	getw
.globl	fopen
.globl	_tmpfil

.data
_getwrd: 1f
.text
1:
	tst	buf
	bne	1f
	mov	_tmpfil,r0
	jsr	r5,fopen; buf
	bes	botchp
1:
	jsr	r5,getw; buf
	bes	botchp
	rts	pc
botchp:
	mov	$1,r0
	sys	write; botch; ebotch-botch
	sys	exit
botch:
	<Temp file botch.\n>; ebotch:
.even
.bss
buf:	.=.+518.
.text
.globl	_opdope
.globl	_instab

_instab:.+2
	40.; 1f; 1f; .data; 1:<add\0>; .text
	70.; 1b; 1b
	41.; 2f; 2f; .data; 2:<sub\0>; .text
	71.; 2b; 2b
	30.; 3f; 1b; .data; 3:<inc\0>; .text
	31.; 4f; 2b; .data; 4:<dec\0>; .text
	32.; 3b; 1b
	33.; 4b; 2b

	45.; 2b; 5f; .data; 5:<ac\0>; .text
	46.; 6f; 7f; .data; 6:<mov\0>; 7:<(r4)\0>; .text
	75.; 2b; 5b
	76.; 6b; 7b
	43.; 7b; 1f; .data; 1:<divf\0>; .text
	44.; 5b; 0
	73.; 7b; 1b
	74.; 5b; 0

	60.; 0f; 1f; .data; 0:<beq\0>; 1:<bne\0>; .text
	61.; 1b; 0b
	62.; 2f; 5f; .data; 2:<ble\0>; 5:<bgt\0>; .text
	63.; 3f; 4f; .data; 3:<blt\0>; 4:<bge\0>; .text
	64.; 4b; 3b
	65.; 5b; 2b
	66.; 6f; 9f; .data; 6:<blos\0>; 9:<bhi\0>; .text
	67.; 7f; 8f; .data; 7:<blo\0>; 8:<bhis\0>; .text
	68.; 8b; 7b
	69.; 9b; 6b
	0
	.data
	.even
	.text

_opdope:.+2
	00000	/ EOF
	00000	/ ;
	00000	/ {
	00000	/ }
	36000	/ [
	02000	/ ]
	36000	/ (
	02000	/ )
	02000	/ :
	07001	/ ,
	00000	/ 10
	00000	/ 11
	00000	/ 12
	00000	/ 13
	00000	/ 14
	00000	/ 15
	00000	/ 16
	00000	/ 17
	00000	/ 18
	00000	/ 19
	00000	/ name
	00000	/ short constant
	00000	/ string
	00000	/ float
	00000	/ double
	00000	/ 25
	00000	/ 26
	00000	/ 27
	00000	/ 28
	00000	/ 29
	34002	/ ++pre
	34002	/ --pre
	34002	/ ++post
	34002	/ --post
	34020	/ !un
	34002	/ &un
	34020	/ *un
	34000	/ -un
	34020	/ ~un
	00000	/ 39
	30101	/ +
	30001	/ -
	32101	/ *
	32001	/ /
	32001	/ %
	26061	/ >>
	26061	/ <<
	20161	/ &
	16161	/ |
	16161	/ ^
	00000	/ 50
	00000	/ 51
	00000	/ 52
	00000	/ 53
	00000	/ 54
	00000	/ 55
	00000	/ 56
	00000	/ 57
	00000	/ 58
	00000	/ 59
	22105	/ ==
	22105	/ !=
	24105	/ <=
	24105	/ <
	24105	/ >=
	24105	/ >
	24105	/ <p
	24105	/ <=p
	24105	/ >p
	24105	/ >=p
	12013	/ =+
	12013	/ =-
	12013	/ =*
	12013	/ =/
	12013	/ =%
	12053	/ =>>
	12053	/ =<<
	12053	/ =&
	12053	/ =|
	12053	/ =^
	12013	/ =
	00000	/ 81
	00000	/ 82
	00000	/ 83
	00000	/ int -> float
	00000	/ int -> double
	00000	/ float -> int
	00000	/ float -> double
	00000	/ double -> int
	00000	/ double -> float
	14001	/ ?
	00000	/ 91
	00000	/ 92
	00000	/ 93
	00000	/ int -> float
	00000	/ int -> double
	00000	/ float -> double
	00000	/ int -> int[]
	00000	/ int -> float[]
	00000	/ int -> double[]
	36001	/ call
	36001	/ mcall
