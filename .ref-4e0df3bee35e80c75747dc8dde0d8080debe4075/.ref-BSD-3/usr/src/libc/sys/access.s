# access(file, request)
#  test ability to access file in all indicated ways
#  1 - read
#  2 - write
#  4 - execute

	.set	access,33
.globl	_access

_access:
	.word	0x0000
	chmk	$access
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
