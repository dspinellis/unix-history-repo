# @(#)time.s	4.1 (Berkeley) 12/21/80
# C library -- time

# tvec = time(tvec);
#

	.set	time,13
.globl	_time

_time:
	.word	0x0000
	chmk	$time
	movl	4(ap),r1
	beql	nostore
	movl	r0,(r1)
nostore:
	ret

# ftime
#
	.set	ftime,35
.globl	_ftime

_ftime:
	.word	0x0000
	chmk	$ftime
	ret
