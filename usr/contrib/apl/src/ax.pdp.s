/
/	APL assembly-language assist for floating-point
/

.globl _fppinit, _error

SIGFPE = 8.			/ signal # for flt-point exception
rtt = 6				/ opcode for RTT instruction
stst = 170300^tst		/ "stst" opcode
bhs = bcc

/
/ Enable FPP traps and route floating-point execeptions
/ to special handler.
/

_fppinit:
	mov	r5,-(sp)
	mov	sp,r5
	sys	signal; SIGFPE; fpetrap
	stfps	r0
	bis	$7400,r0
	ldfps	r0
	mov	(sp)+,r5
	rts	pc


/
/	Exception trap handler
/
/ Attempt to figure out the reason for the floating-point
/ exception by immediately dumping the error register.
/ If another process generates a floating-point exception
/ before this service routine is called, the error register
/ will be meaningless.  We just take our lumps in that case.
/

fpetrap:
	stst	fpstatus	/ note: static allocation
	mov	r5,-(sp)
	mov	sp,r5
	mov	r1,-(sp)
	mov	r0,-(sp)
	sys	signal; SIGFPE; fpetrap
	mov	mesg,-(sp)	/ default message
	mov	fpstatus,r0
	cmp	r0,$nmesg
	bhs	1f
	add	$mesg,r0	/ pointer to specific message
	mov	(r0),(sp)
1:
	jsr	pc, _error	/ print error and APL traceback
	tst	(sp)+		/ "can't" ever execute this code
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r5
	rtt

.data

mesg:	mesg0
	mesg2
	mesg4
	mesg6
	mesg8
	mesg10
	mesg12
	mesg14
nmesg = . - mesg

mesg0:	<floating exception\0>
mesg2:	<floating opcode error\0>
mesg4:	<floating divide by zero\0>
mesg6:	<floating-to-integer conversion error\0>
mesg8:	<floating overflow\0>
mesg10:	<floating underflow\0>
mesg12:	<undefined floating-point variable\0>
mesg14:	<floating-point maintenance trap\0>
.even

.bss

fpstatus: .=.+4
