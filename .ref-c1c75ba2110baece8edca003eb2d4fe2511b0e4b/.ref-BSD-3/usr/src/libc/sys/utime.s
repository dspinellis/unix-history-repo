# C library -- utime
 
#  error = utime(string,timev);
 
.globl	_utime
.globl	cerror
	.set	utime,30
 
_utime:
	.word	0x0000
	chmk	$utime
	bcc	noerror
	jmp	cerror
noerror:
	ret
