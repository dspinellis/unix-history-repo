# C library -- exect

# exect(file, argv, env);
#
# where argv is a vector argv[0] ... argv[x], 0
# last vector element must be 0
#
# The same as execve except that it sets the TBIT causing
# a trace trap on the first instruction of the executed process,
# to give a chance to set breakpoints.

.globl	_exect
.globl	cerror

	.set	execve,59
_exect:
	.word	0x0000
	bispsw	$0x10		# set tbit
	chmk	$execve
	jmp 	cerror
