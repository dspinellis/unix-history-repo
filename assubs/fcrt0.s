/ C runtime startup
/ with floating point interpreter

.globl	savr5
.globl	fptrap,fpjsr
.globl	_exit

.globl	_main

start:
	sys	signal; 4; fptrap
	jsr	pc,fpjsr
	setd
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,_main
	cmp	(sp)+,(sp)+
	mov	r0,(sp)
	jsr	pc,*$_exit
	sys	exit

.bss
savr5:	.=.+2
