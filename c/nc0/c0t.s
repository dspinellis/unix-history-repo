/ word I/O

.globl	_putwrd

.globl	_tmpfil
.globl	putw
.globl	fcreat
.globl	flush

.data
_putwrd: 1f
.text
1:
	tst	buf
	bne	1f
	mov	_tmpfil,r0
	jsr	r5,fcreat; buf
	bec	1f
	mov	$1,r0
	sys	write; botch; ebotch-botch
	sys	exit
1:
	mov	2(sp),r0
	jsr	r5,putw; buf
	rts	pc
.globl	_flshw
.data
_flshw:	1f
.text
1:
	jsr	r5,flush; buf
	rts	pc

botch:	<Temp file?\n\0>; ebotch:
.even

.bss
buf:	.=.+518.
.text

/ C operator and conversion tables

.globl	_opdope
.globl	_cvtab

_opdope:.+2
	00000	/ EOF
	00000	/ ;
	00000	/ {
	00000	/ }
	36000	/ [
	02000	/ ]
	36000	/ (
	02000	/ )
	14201	/ :
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
	34202	/ ++pre
	34202	/ --pre
	34202	/ ++post
	34202	/ --post
	34220	/ !un
	34202	/ &un
	34220	/ *un
	34200	/ -un
	34220	/ ~un
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
	12213	/ =+
	12213	/ =-
	12213	/ =*
	12213	/ =/
	12213	/ =%
	12253	/ =>>
	12253	/ =<<
	12253	/ =&
	12253	/ =|
	12253	/ =^
	12213	/ =
	00000	/ 81
	00000	/ 82
	00000	/ 83
	00000	/ int -> float
	00000	/ int -> double
	00000	/ float -> int
	00000	/ float -> double
	00000	/ double -> int
	00000	/ double -> float
	14201	/ ?
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

_cvtab:	.+2
	.byte	000	/ i:i
	.byte	000	/ i:c
	.byte	113	/ i:f
	.byte	125	/ i:d
	.byte	140	/ i:i[]
	.byte	100	/ i:c[]
	.byte	150	/ i:f[]
	.byte	160	/ i:d[]
	.byte	140	/ i:[][]

	.byte	100	/ c:i
	.byte	100	/ c:c
	.byte	113	/ c:f
	.byte	125	/ c:d
	.byte	140	/ c:i[]
	.byte	100	/ c:c[]
	.byte	150	/ c:f[]
	.byte	160	/ c:d[]
	.byte	140	/ c[][]

	.byte	211	/ f:i
	.byte	211	/ f:c
	.byte	000	/ f:f
	.byte	136	/ f:d
	.byte	211	/ f:i[]
	.byte	211	/ f:c[]
	.byte	211	/ f:f[]
	.byte	211	/ f:d[]
	.byte	211	/ f:[][]

	.byte	222	/ d:i
	.byte	222	/ d:c
	.byte	234	/ d:f
	.byte	000	/ d:d
	.byte	222	/ d:i[]
	.byte	222	/ d:c[]
	.byte	222	/ d:f[]
	.byte	222	/ d:d[]
	.byte	222	/ d:[][]

	.byte	240	/ i[]:i
	.byte	240	/ i[]:c
	.byte	113	/ i[]:f
	.byte	125	/ i[]:d
	.byte	000	/ i[]:i[]
	.byte	000	/ i[]:c[]
	.byte	100	/ i[]:f[]
	.byte	100	/ i[]:d[]
	.byte	100	/ i[]:[][]

	.byte	000	/ c[]:i
	.byte	000	/ c[]:c
	.byte	113	/ c[]:f
	.byte	125	/ c[]:d
	.byte	200	/ c[]:i[]
	.byte	000	/ c[]:c[]
	.byte	200	/ c[]:f[]
	.byte	200	/ c[]:d[]
	.byte	200	/ c[]:[][]

	.byte	250	/ f[]:i
	.byte	250	/ f[]:c
	.byte	113	/ f[]:f
	.byte	125	/ f[]:d
	.byte	000	/ f[]:i[]
	.byte	000	/ f[]:c[]
	.byte	000	/ f[]:f[]
	.byte	100	/ f[]:d[]
	.byte	000	/ f[]:[][]

	.byte	260	/ d[]:i
	.byte	260	/ d[]:c
	.byte	113	/ d[]:f
	.byte	125	/ d[]:d
	.byte	000	/ d[]:i[]
	.byte	000	/ d[]:c[]
	.byte	000	/ d[]:f[]
	.byte	000	/ d[]:d[]
	.byte	000	/ d[]:[][]

	.byte	240	/ [][]:i
	.byte	240	/ [][]:c
	.byte	113	/ [][]:f
	.byte	125	/ [][]:d
	.byte	000	/ [][]:i[]
	.byte	000	/ [][]:c[]
	.byte	100	/ [][]:f[]
	.byte	100	/ [][]:d[]
	.byte	000	/ [][]:[][]

.even

/ character type table

.globl	_ctab

_ctab: .+2
	.byte 000.,127.,127.,127.,127.,127.,127.,127.
	.byte 127.,126.,125.,127.,127.,127.,127.,127.
	.byte 127.,127.,127.,127.,127.,127.,127.,127.
	.byte 127.,127.,127.,127.,127.,127.,127.,127.
	.byte 126.,034.,122.,127.,127.,044.,047.,121.
	.byte 006.,007.,042.,040.,009.,041.,127.,043.
	.byte 124.,124.,124.,124.,124.,124.,124.,124.
	.byte 124.,124.,008.,001.,063.,080.,065.,090.
	.byte 127.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,004.,127.,005.,049.,127.
	.byte 127.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,123.,123.,123.,123.,123.
	.byte 123.,123.,123.,002.,048.,003.,127.,127.

