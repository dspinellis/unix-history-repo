| /* Copyright (c) 1982, Regents, University of California */
	.data
	.text
	.globl	_dmlad
_dmlad:
	link	a6,#-.F1
	tstb	sp@(-132)
	moveml	#.S1,a6@(-.F1)
	movl	a6@(12),d7
	movl	a6@(16),d6
|l 8
	movl	a6@(8),a5
|e 8
.L15:
|l 12
	pea	a6@(0xfffffff8)
	movl	d6,sp@-
	movl	d7,sp@-
	movl	a5@,sp@-
	jsr	_emul
	addl	#16,sp
|e 12
|l 13
	movl	a6@(0xfffffff8),d6
|e 13
|l 14
	asll	#2,d6
|e 14
|l 15
	tstl	a6@(0xfffffffc)
	bge	.L17
|e 15
|l 15
	addql	#2,d6
|e 15
.L17:
|l 16
	movl	a6@(0xfffffffc),d0
	asll	#1,d0
	tstl	d0
	bge	.L18
|e 16
|l 16
	addql	#1,d6
|e 16
.L18:
|l 17
	movl	a6@(0xfffffffc),d0
	andl	#0x3fffffff,d0
	movl	d0,a5@
|e 17
|l 18
	tstl	a5@(4)
	bne	.L19
|e 18
	bra	.L14
.L19:
|l 19
	movl	a5@(4),a5
|e 19
.L13:
	bra	.L15
.L14:
|l 21
	tstl	d6
	beq	.L20
|e 21
|l 24
	cmpl	#0xffffffff,d6
	bne	.L22
|e 24
|l 25
	orl	#0xc0000000,a5@
|e 25
	bra	.L23
.L22:
|l 27
	jsr	_newdot
	movl	d0,a5@(4)
	movl	a5@(4),a5
|e 27
|l 28
	movl	d6,a5@
|e 28
|l 29
	clrl	a5@(4)
|e 29
.L23:
.L20:
|l 32
	movl	a6@(8),d0
|e 32
	bra	.L12
	bra	.L12
.L12:	moveml	a6@(-.F1),#0x20c0
	unlk	a6
	rts
.F1 = 20
.S1 = 0x20c0
| end
	.data
