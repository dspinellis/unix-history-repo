# C library -- wait

# pid = wait(0);
#   or,
# pid = wait(&status);
#
# pid == -1 if error
# status indicates fate of process, if given

	.set	wait,7
.globl	_wait
.globl  cerror

	.align	1
_wait:
	.word	0x0000
	chmk	$wait
	bcc 	noerror
	jmp 	cerror
noerror:
	tstl	4(ap)		# status desired?
	beql	nostatus	# no
	movl	r1,*4(ap)	# store child's status
nostatus:
	ret
