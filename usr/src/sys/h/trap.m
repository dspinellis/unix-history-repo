#
#	Trap,type values
#

	.set	RESADFLT,0		# reserved addressing fault
	.set	PRIVINFLT,1		# privileged instruction fault
	.set	BPTFLT,2		# bpt instruction fault
	.set	XFCFLT,3		# xfc instruction fault
	.set	RESOPFLT,4		# reserved operand fault
	.set	SYSCALL,5		# chmk instruction (syscall trap)
	.set	ARITHTRAP,6		# arithmetic trap
	.set	RESCHED,7		# software level 1 trap (reschedule trap)
	.set	SEGFLT,8		# segmentation fault
	.set	PROTFLT,9		# protection fault

	.set	TRCTRAP,10		# trace trap
	.set	COMPATFLT,11	# compatibility mode fault
