/ access(file, request)
/  test ability to access file in all indicated ways
/  4 - read
/  2 - write
/  1 - execute

.globl	_access
.globl	csv, cret
.comm	_errno,2

access = 33.

_access:
	jsr	r5,csv
	mov	4(r5),0f+2
	mov	6(r5),0f+4
	clr	r0
	sys	0; 0f
.data
0:	sys	access; ..; ..
.text
	bec	1f
	mov	r0,_errno
	mov	$-1,r0
1:
	jmp	cret
