/ Startup code for two-stage bootstrap

/ non-UNIX instructions
mfpi	= 6500^tst
stst	= 170300^tst
mtpi	= 6600^tst
mfpd	= 106500^tst
mtpd	= 106600^tst
spl	= 230
ldfps	= 170100^tst
stfps	= 170200^tst
wait	= 1
rtt	= 6
reset	= 5
/ trap	= 104400

PS	= 177776

.globl	_end
.globl	_main, __rtt
.globl	_edata
	jmp	start

/
/ trap vectors
/
	trap;340
	trap;341	/ illegal instruction
	trap;342	/ BPT
	trap;343	/ IOT
	trap;344	/ POWER FAIL
	trap;345	/ EMT
tvec:
	start;346	/ TRAP
.=400^.
.text


start:
	mov	$340,*$PS
	mov	$trap,tvec
/ fix up stack segment clobbered by trap
	mov	$1400,*$KDSA6
	mov	$157776,sp
	mov	$_edata,r0
	mov	$_end,r1
	sub	r0,r1
	inc	r1
	clc
	ror	r1
1:
	clr	(r0)+
	sob	r1,1b
	jsr	pc,_main

/ fix up stack to point at trap ps-pc pair
/ so we can return to the bootstrap
__rtt:
	clr	*$KDSA6
	mov	$140000,sp
	rtt				/ we hope!
	br	.


.globl	_trap
trap:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	*$PS,-(sp)
	jsr	pc,_trap
	tst	(sp)+
	mov	(sp)+,r1
	mov	(sp)+,r0
	rtt

KDSA6 = 172374
