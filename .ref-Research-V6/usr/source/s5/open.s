/ C library -- open

/ file = open(string, mode)
/
/ file == -1 means error

.globl	_open, cerror

_open:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	6(r5),0f+2
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	open; 0:..; ..
