	.asciz	"@(#)mcount.s	35.1	5/6/81"
# count subroutine called during profiling

.globl	mcount
.globl _mcount
.comm	_countbase,4

_mcount:
mcount:
	movl	(r0),r1
	beql	init
incr:
	incl	(r1)
return:
	rsb
init:
	movl	_countbase,r1
	beql	return
	addl2	$8,_countbase
	movl	(sp),(r1)+
	movl	r1,(r0)
	brb 	incr
