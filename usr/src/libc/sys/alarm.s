/ C library - alarm

.globl	_alarm
.alarm = 27.

_alarm:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	.alarm
	mov	(sp)+,r5
	rts	pc
