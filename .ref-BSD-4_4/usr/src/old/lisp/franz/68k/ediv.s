| /* Copyright (c) 1982, Regents, University of California */
	.text
	.globl	_ediv
_ediv:
	link	a6,#-.F1
	tstb	sp@(-132)
	moveml	#.S1,a6@(-.F1)
	movl	a6@(8),a5
	movl	a5@,d7
	movl	a5@(4),d6
	moveq	#0,d5
	moveq	#0,d4
	movl	a6@(0xc),d3
	clrb	a6@(0xfffffffd)
	clrb	a6@(0xfffffffb)
	movl	d7,a6@(0xfffffff4)
	jge	.L13
	eorb	#1,a6@(0xfffffffd)
	negl	d6
	negxl	d7
.L13:
	tstl	d3
	jge	.L16
	eorb	#1,a6@(0xfffffffd)
	negl	d3
.L16:
	tstl	d3
	jne	.L17
	clrl	a5@
	movl	a6@(0x10),a0
	movb	#1,a0@
	movl	d6,d0
	jra	.L12
.L17:
	movw	#0x20,a6@(0xfffffffe)
	jra	.L20
.L20001:
	lsll	#1,d3
	addqw	#1,a6@(0xfffffffe)
	addql	#1,d5
.L20:
	cmpl	#0x40000000,d3
	jcs	.L20001
	cmpl	d3,d7
	jcs	.L24
	subl	d3,d7
	addql	#1,d4
	jra	.L24
.L20003:
	lsll	#1,d6
	roxll	#1,d7
	asll	#1,d4
	cmpl	d3,d7
	jcs	.L26
	subl	d3,d7
	addql	#1,d4
.L26:
	tstl	d4
	jge	.L22
	movl	a6@(0x10),a0
	movb	#1,a0@
.L22:
	subqw	#1,a6@(0xfffffffe)
.L24:
	tstw	a6@(0xfffffffe)
	jne	.L20003
	lsrl	d5,d7
	tstl	a6@(0xfffffff4)
	jge	.L28
	negl	d7
.L28:
	movl	d7,a5@
	andl	#0x7fffffff,d4
	tstb	a6@(0xfffffffd)
	jeq	.L29
	negl	d4
.L29:
	movl	d4,d0
.L12:
	moveml	a6@(-.F1),#0x20f8
	unlk	a6
	rts
.F1 = 36
.S1 = 0x20f8
| end
	.data
