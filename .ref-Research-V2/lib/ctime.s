/ C library -- ctime

/ ctime(v1, v2);
/ v1 is input time [2]
/ v2 is char[16] ascii time
/ format is 0123456789012345
/           Mmm dd hh:mm:sse
/

.globl	_ctime
.globl	ctime

.data
_ctime:
	1f
.text
1:
	mov	2(sp),r0
	mov	(r0)+,-(sp)
	mov	(r0)+,mq
	mov	(sp)+,ac
	mov	4(sp),r0
	clrb	15.(r0)
	jsr	pc,ctime
	rts	pc

