# C library -- vlimit

# oldvalue = vlimit(what, newvalue);
# if newvalue == -1 old value is returned and the limit is not changed

	.set	vlimit,64+13
.globl	_vlimit

_vlimit:
	.word	0x0000
	chmk	$vlimit
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
