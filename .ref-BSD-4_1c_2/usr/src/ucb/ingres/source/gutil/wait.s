/ C library -- wait

/	@(#)wait.s	7.1	2/5/81

/ pid = wait(&status);
/
/ pid == -1 if error
/ status indicates fate of process, if given

.globl	_wait, cerror

_wait:
	mov	r5,-(sp)
	mov	sp,r5
	sys	wait
	bec	1f
	jmp	cerror
1:
	mov	r1,*4(r5)	/ status return
	mov	(sp)+,r5
	rts	pc
