/ pipe -- C library

/	pipe(f)
/	int f[2];

.globl	_pipe, cerror

pipe = 42.

_pipe:
	mov	r5,-(sp)
	mov	sp,r5
	sys	pipe
	bec	1f
	jmp	cerror
1:
	mov	4(r5),r2
	mov	r0,(r2)+
	mov	r1,(r2)
	clr	r0
	mov	(sp)+,r5
	rts	pc
