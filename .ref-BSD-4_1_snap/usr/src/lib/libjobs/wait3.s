# C library -- wait3

# pid = wait3(&status, flags, &vmstat);
#
# pid == -1 if error
# status indicates fate of process, if given
# flags may indicate process is not to hang or
# that untraced stopped children are to be reported.
# vmstat optionally returns detailed resource usage information
#

	.set	wait3,7		# same as wait!
.globl	_wait3
.globl  cerror

	.align	1
_wait3:
	.word	0x0000
	movl	8(ap),r0	# make it easy for system to get
	movl	12(ap),r1	# these extra arguments
	bispsw	$0xf		# flags wait3()
	chmk	$wait3
	bcc 	noerror
	jmp 	cerror
noerror:
	tstl	4(ap)		# status desired?
	beql	nostatus	# no
	movl	r1,*4(ap)	# store child's status
nostatus:
	ret
