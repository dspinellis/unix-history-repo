/ ptrace -- C library

/	result = ptrace(req, pid, addr, data);

ptrace = 26.
indir = 0

.globl	_ptrace
.globl	cerror, _errno

_ptrace:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4.(r5),0f+4
	mov	6.(r5),0f+0
	mov	8.(r5),0f+2
	mov	10.(r5),r0
	clr	_errno
	sys	indir; 9f
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc

.data
9:
	sys	ptrace; 0: .=.+6
