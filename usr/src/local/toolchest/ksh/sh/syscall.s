	.file	"syscall.s"
#	@(#)syscall.s	1.4
#	syscall(number, arg0, arg1, ... )

	.globl	syscall
syscall:
	save	&0
	addw2	&12,%sp			# get a place to store a call
	movh	code,0(%fp)		# copy the "ost" to the stack
	movh	code+2,2(%fp)
	movh	code+4,4(%fp)
	movw	&return,8(%fp)		# store the "return" address
	movb	3(%ap),1(%fp)		# set the call code
	addw2	&4,%ap			# point to the first arg
	jmp	0(%fp)			# execute the call
code:
	ost	&0			# this code is copied to the stack
	jmp	*8(%fp)
return:
	jcs	error
	subw2	&4,%ap			# "ap" must be restored!
	cmpw	&0,&1			# clear the carry flag
	jmp	_csysret
error:
	subw2	&4,%ap
	cmpw	&1,&1			# set the carry flag
	jmp	_csysret
