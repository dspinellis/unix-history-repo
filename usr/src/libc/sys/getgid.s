# C library -- getgid

# gid = getgid();
#

	.set	getgid,47
.globl	_getgid

_getgid:
	.word	0x0000
	chmk	$getgid
	ret

# C library -- getegid

# gid = getegid();
# returns effective gid

.globl	_getegid

_getegid:
	.word	0x0000
	chmk	$getgid
	movl	r1,r0
	ret
