/ C library -- fork

/ pid = fork();
/
/ pid == 0 in child process; pid == -1 means error return

.globl	_fork, retrn, cerror

_fork:
	mov	r5,-(sp)
	mov	sp,r5
	sys	fork
		br 1f
	bec	2f
	jmp	cerror
1:
	clr	r0
2:
	jmp	retrn

