/ C library -- dup

/	f = dup(of [ ,nf])
/	f == -1 for error

.globl	_dup,_dup2
.globl	cerror
.dup = 41.

_dup2:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	bis	$100,r0
	br	1f

_dup:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
1:
	mov	6(r5),r1
	sys	.dup
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
