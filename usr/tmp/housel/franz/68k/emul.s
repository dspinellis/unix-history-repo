| /* Copyright (c) 1982, Regents, University of California */
|
| $Header: emul.s,v 1.2 83/11/26 12:13:45 sklower Exp $
| $Locker:  $
|
	.text
	.globl	_emul
_emul:
	link	a6,#-_F1
	moveml	#_S1,a6@(-_F1)
	movl	a6@(20),a5
| A1 = 24
	clrb	a6@(-9)
	clrb	a6@(-13)
	clrl	d7
	tstl	a6@(8)
	bge	.L13
	eorb	#1,a6@(-9)
	negl	a6@(8)
.L13:
	tstl	a6@(12)
	bge	.L14
	eorb	#1,a6@(-9)
	negl	a6@(12)
.L14:
	movw	a6@(10),d1
	mulu	a6@(14),d1
	movl	d1,a6@(-4)
	movw	a6@(8),d1
	mulu	a6@(12),d1
	movl	d1,a6@(-8)
	movw	a6@(8),d1
	mulu	a6@(14),d1
	addl	d1,a6@(-6)
	bcc	.L16
	addqw	#1,a6@(-8)
.L16:
	movw	a6@(10),d1
	mulu	a6@(12),d1
	addl	d1,a6@(-6)
	bcc	.L17
	addqw	#1,a6@(-8)
.L17:
	tstb	a6@(-9)
	beq	.L18
	negl	a6@(-4)
	negxl	a6@(-8)
.L18:
	tstl	a6@(16)
	bge	.L20
	moveq	#-1,d7
.L20:
	movl	a6@(-8),d1
	movl	a6@(16),d0
	addl	a6@(-4),d0
	addxl	d1,d7
	movl	d0,a5@(4)
	movl	d7,a5@
.L12:	moveml	a6@(-_F1),#8320
	unlk	a6
	rts
_F1 = 24
_S1 = 8320
| M1 = 0
