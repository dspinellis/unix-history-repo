/ C library -- time

/ tvec = time(tvec);
/
/ tvec[0], tvec[1] contain the time

	.globl	_time

.data
_time:
	1f
.text
1:
	mov	2(sp),r0
	sys	time
	mov	ac,(r0)
	mov	mq,2(r0)
	rts	pc

