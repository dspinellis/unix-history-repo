/ C library -- time

/ tvec = time(tvec);
/
/ tvec[0], tvec[1] contain the time

.globl	_time

_time:
	mov	r5,-(sp)
	mov	sp,r5
	sys	time
	mov	r2,-(sp)
	mov	4(r5),r2
	mov	r0,(r2)+
	mov	r1,(r2)+
	mov	(sp)+,r2
	mov	(sp)+,r5
	rts	pc
