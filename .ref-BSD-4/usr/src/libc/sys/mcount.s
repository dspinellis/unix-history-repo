# count subroutine called during profiling

.globl	mcount
.comm	countbase,4

mcount:
	movl	(r0),r1
	beql	init
incr:
	incl	(r1)
return:
	rsb
init:
	movl	countbase,r1
	beql	return
	addl2	$8,countbase
	movl	(sp),(r1)+
	movl	r1,(r0)
	brb 	incr
