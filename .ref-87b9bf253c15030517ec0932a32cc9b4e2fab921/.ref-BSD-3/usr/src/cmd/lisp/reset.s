# C library -- reset, setexit

#	reset(x)
# will generate a "return" from
# the last call to
#	setexit()
# by restoring r6 - r12, ap, fp
# and doing a return.
# The returned value is x; on the original
# call the returned value is 0.
#
# useful for going back to the main loop
# after a horrible error in a lowlevel
# routine.

.globl	_setexit
.globl	_getexit
.globl	_reset
.globl	_resexit
.globl	_setsav
.globl	_svkludg

_setexit:
	.word	0x0000
	movab	_setsav,r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	8(fp),(r0)+		# ap, fp
	movab	4(ap),(r0)+		# sp
	movl	16(fp),(r0)		# pc
	clrl	r0
	ret

_reset:
	.word	0x0000
	movl	4(ap),r0	# returned value
	movab	_setsav,r1
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,r12
	movl	(r1)+,sp
	jmp 	*(r1)

_resexit:
	.word	0x0000
	movc3	$40,*4(ap),_setsav
	ret
_svkludg:
	movl	(sp)+,out
	movq	r0,myregs
	movq	r2,myregs+8
	movq	r4,myregs+16
	subl2	$40,sp
	movc3	$40,_setsav,(sp)
	movq	myregs,r0
	movq	myregs+8,r2
	movq	myregs+16,r4
	jmp	*out

_getexit:
	.word	0x0000
	movc3	$40,_setsav,*4(ap)
	ret


	.data
_setsav:.space	10*4
out:	.space	4
myregs:	.space	6*4

