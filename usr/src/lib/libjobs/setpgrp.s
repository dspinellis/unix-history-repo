# C library -- setpgrp, getpgrp

# setpgrp(pid, pgrp);	/* set pgrp of pid and descendants to pgrp */
# if pid==0 use current pid
#
# getpgrp(pid)
# implemented as setpgrp(pid, -1)

	.set	setpgrp,39
.globl	_setpgrp
.globl	_getpgrp
.globl  cerror

	.align	1
_setpgrp:
	.word	0x0000
	chmk	$setpgrp
	bcc 	noerror
	jmp 	cerror
noerror:
	ret

	.align	1
_getpgrp:
	.word	0x0000
	pushl	$-1
	pushl	4(ap)
	calls	$2,gpgrp
	ret
gpgrp:
	.word	0x0000
	chmk	$setpgrp
	bcc	noerror
	jmp	cerror
