# C library -- getuid

# uid = getuid();
#

	.set	getuid,24
.globl	_getuid

_getuid:
	.word	0x0000
	chmk	$getuid
	ret



# C library -- geteuid

# uid = geteuid();
#  returns effective uid

.globl	_geteuid

_geteuid:
	.word	0x0000
	chmk	$getuid
	movl	r1,r0
	ret
