/ C runtime startoff
/ with floating point interpreter

.globl	savr5
.globl	fptrap
.globl	_exit

.globl	_main

start:
	sys	signal; 4; fptrap
	setd
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,_main
	mov	r0,(sp)
	jsr	pc,*$_exit
	sys	exit

.bss
savr5:	.=.+2
