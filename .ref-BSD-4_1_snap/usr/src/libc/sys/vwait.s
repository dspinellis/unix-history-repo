# @(#)vwait.s	4.1 (Berkeley) 12/21/80
# C library -- vwait

# pid = vwait(0, &vms);
#   or,
# pid = vwait(&status, &vms);
#
# pid == -1 if error
# status indicates fate of process, if given
# vms is a vmstat structure <vmstat.h>

	.set	vwait,64+7
.globl	_vwait
.globl  cerror

	.align	1
_vwait:
	.word	0x0000
	chmk	$vwait
	bcc 	noerror
	jmp 	cerror
noerror:
	tstl	4(ap)		# status desired?
	beql	nostatus	# no
	movl	r1,*4(ap)	# store child's status
nostatus:
	ret
