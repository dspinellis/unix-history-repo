#old = sbrk(increment);
#
#sbrk gets increment more core, and returns a pointer
#	to the beginning of the new core area
#
	.set	break,17
.globl	_sbrk
.globl _end
.globl  cerror

	.align	1
_sbrk:
	.word	0x0000
	addl3	nd,4(ap),-(sp)
	pushl	$1
	movl	ap,r3
	movl	sp,ap
	chmk	$break
	bcc 	noerr1
	jmp 	cerror
noerr1:
	movl	nd,r0
	addl2	4(r3),nd
	ret

.globl	_brk
# brk(value)
# as described in man2.
# returns 0 for ok, -1 for error.

_brk:
	.word	0x0000
	chmk	$break
	bcc 	noerr2
	jmp 	cerror
noerr2:
	movl	4(ap),nd
	clrl	r0
	ret

	.data
nd:	.long	_end
