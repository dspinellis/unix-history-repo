/*
 * pushframe : stack a frame 
 * mess with the standard calls/callf call frame to put things in
 * the proper order. On entry to _Pushframe we have:
 * On entry to Pushframe:		On exit:
 * ______________			______________	
 * | second arg |			| second arg |
 * +------------+			+------------+
 * | first arg  |			| first arg  |
 * +------------+			+------------+
 * | 'class' arg|			| 'class' arg|
 * +------------+			+------------+
 * | saved fp   |  <<= fp		| return addr|
 * +------------+			+------------+
 * |mask|removed|			| old _errp  |
 * +------------+			+------------+
 * |return addr.|  <<= sp		| saved _bnp |
 * +------------+			+------------+
 *					| saved _np  |
 *					+------------+
 *					| saved lbot |	<<= d0
 *					+------------+
 *					| saved r13  |
 *					|       ...  |
 *					| saved r8   |  <<= sp
 *					+------------+
 */
	.text
	.globl	_Pushframe
	.globl	_qpushframe

_qpushframe:
_Pushframe:
	.word 0x0
	movl	(fp),fp		# give user his fp back
	movl	(sp),r0		# put program counter in temporary
	moval	12(sp),sp	# remove pc, 'mask', 'removed', and fp
	pushl	r0		# stack the return address
	pushl	_errp
	pushl	_bnp
	pushl	r6		# save _np
	pushl	r7		# save _lbot
	moval	(sp),r0		# return addr of lbot on stack to caller
	moval	-24(sp),sp
	storer	$0x3f00,(sp)	# save r13(fp), r12,r11,r10,r9,r8
	clrl	_retval		# set retval to C_INITIAL
	jmp	*40(sp)		# return to caller
