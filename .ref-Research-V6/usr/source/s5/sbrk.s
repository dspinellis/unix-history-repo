/old = sbrk(increment);
/
/sbrk gets increment more core, and returns a pointer
/	to the beginning of the new core area
/
.globl	_sbrk,_end, cerror

_sbrk:
	mov	r5,-(sp)
	mov	sp,r5
	mov	nd,0f
	add	4(r5),0f
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	mov	nd,r0
	add	4(r5),nd
	mov	(sp)+,r5
	rts	pc

.globl	_brk
/ brk(value)
/ as described in man2.
/ returns 0 for ok, -1 for error.

_brk:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	mov	4(r5),nd
	clr	r0
	mov	(sp)+,r5
	rts	pc

.data
9:
	sys	break; 0:..
nd:	_end
