#
#
# 31writeold.s
#
#    The following opcodes are provided to maintain compatability
# with the previous pascal interpreter. They convert the stack to
# the standard I/O format then branch to the appropriate function.
#
# These routines, especially "calcfmt", are an abomination for which
# I apologize in advance. The formats should be calculated by the
# compiler and this module discarded!
#
_WRITB:
	subl2	$len,sp		#allocate space for format
	tstw	len(sp)		#'true' => minimum width = 4
	beql	l3120
	pushl	$4
	brb	l3121
l3120:
	pushl	$5		#'false' => minimum width = 5
l3121:
	pushl	$2		#data size
	pushal	wrbool		#default format
	calls	$3,calcfmt	#r0 pts to new top of stack
	movl	r0,sp		#update stack
	movl	$len,r6		#set format length
	jbr	bentry		#execute normal write
_WRITC:
	tstb	(r10)		#check for formatting
	jeql	_WRITEC		#none, so go to it !
	movzwl	(sp)+,-(sp)	#align data
	moval	wrchr,r0	#default format string to be used
fprint:
	subl2	$len,sp		#allocate space for format
	pushl	$1		#minimum field width
	pushl	$4		#data size
	pushl	r0		#default format
	calls	$3,calcfmt	#r0 pts to new top of stack
	movl	r0,sp		#update stack
	movl	$len,r6		#set format length
	movl	$1,r5		#number of data arguements
	jbr	fentry		#execute normal write
_WRHEX2:
	cvtwl	(sp)+,-(sp)	#align data
_WRHEX4:
	moval	wrhex,r0	#default format string to be used
	jbr	fprint
_WROCT2:
	cvtwl	(sp)+,-(sp)	#align data
_WROCT4:
	moval	wroct,r0	#default format string to be used
	jbr	fprint
_WRIT2:
	cvtwl	(sp)+,-(sp)	#align data
_WRIT4:
	moval	wrint,r0	#default format string to be used
	jbr	fprint
_WRIT8:
	subl2	$len,sp		#allocate space for format
	pushl	$8		#minimum field width
	pushl	$8		#data size
	pushal	wrreal		#default format
	calls	$3,calcfmt	#r0 pts to new top of stack
	movl	r0,sp		#update stack
	pushl	$14		#push default precision
	pushal	-(sp)		#get field width spec, so that
	pushal	rd4		# maximum permissable precision
	pushal	26(sp)		# can be calculated
	calls	$3,_sscanf	#field width returned on stack
	subl3	$7,(sp)+,r0	#r0 = max_precision = width - required_space
	cmpl	(sp),r0		#if precision requested < r0
	bleq	l3122		# then use request
	movl	r0,(sp)		# else use max permissable
l3122:
	movl	$len,r6		#set format length
	movl	$3,r5		#real data and precision
	jbr	fentry
_WRIT82:
	subl2	$len,sp		#allocate space for format
	pushl	$1		#minimum field width
	pushl	$8		#data size
	pushal	wrfixd		#default format
	calls	$3,calcfmt	#r0 pts to new top of stack
	movl	r0,sp		#update stack
	bbs	$4,-1(r10),l3123#check precision size
	movl	len+8(sp),-(sp)	#push precision
	movl	$len+4,r6	#set format + precision length
	brb	l3124
l3123:
	cvtwl	len+8(sp),-(sp)	#push precision
	movl	$len+2,r6	#set format + precision length
l3124:
	movl	$3,r5		#real data and precision
	jbr	fentry
#
_WRITG:
	cvtwl	1(r10),r5	#r5 has string length
	addl3	$1,r5,r6	#make space for null terminater
	blbc	r6,l3125	#r6 has data length
	incl	r6
l3125:
	subl3	r5,r6,r0	#check for need to add terminater
	blbs	r0,l3126	#1 => already terminated; 2 => needed
	subl2	$2,sp		#allocate terminater space
	movc5	r5,2(sp),$0,r6,(sp)  #put in terminater
l3126:
	subl2	$len,sp		#allocate space for format
	cvtwl	1(r10),-(sp)	#push minimum string length
	pushl	r6		#data size
	pushal	wrstr		#default format
	calls	$3,calcfmt	#r0 pts to new top of stack
	movl	r0,sp		#update stack
	addl2	$2,r10		#update lc
	movl	$len,r5		#set format length
	jbr	sentry		#execute normal write string
#
# Default formats
#
	.set	len,12		#maximum format size
wrbool:	.byte	'%,'1,'0,'s, 0
wrhex:	.byte	'%,'8,'X, 0
wroct:	.byte	'%,'1,'1,'O, 0
wrint:	.byte	'%,'1,'0,'D, 0
wrchr:	.byte	'%,'c, 0
wrstr:	.byte	'%,'s, 0
	.data
wrreal:	.byte	' ,'%,'2,'1,'.,'*,'e, 0
wrfixd:	.byte	' ,'%,'2,'1,'.,'*,'f, 0
	.text
#
#
# Routine to calculate formats and place them on the stack
#

calcfmt:#(format, datasize, minsize)

#char	*format;	/* pointer to default format */
#long	datasize;	/* number of bytes of data */
#long	minsize;	/* minimum width of output field */

	.set	args,16		#size of arguements on the stack

	.word	R2|R3|R4|R5|R6|R9
	cvtbl	(r10)+,r0	#r0 has width spec
	bneq	l3140		#check for width spec
	moval	args(ap), r9	# r9 pts to return sp value
	movl	4(ap),r6	#r6 pts to default format
	jbr	l3148
l3140:
	moval	len+args(ap),r1	#r1 pts to data
	addl2	8(ap),r1	#r1 pts to width specification
	bbs	$1,r0,l3141	#check format size
	moval	args+4(ap), r9	# r9 pts to return sp value
	movl	(r1),r2		#r2 has width spec
	brb	l3142
l3141:
	moval	args+2(ap), r9	# r9 pts to return sp value
	cvtwl	(r1),r2		#r2 has width spec
l3142:
	cmpl	r2,12(ap)	#check to meet minimum width
	bgeq	l3143
	movl	12(ap),r2	#if not, set to minimum width
l3143:
	movl	4(ap),r6	#r6 pts to default format
	subl2	$len,sp		#allocate workspace for new format
	movl	sp,r5		#r5 pts to new format
l3144:
	movb	(r6),(r5)+	#move format up to data spec
	cmpb	(r6)+,$'%
	bneq	l3144
	subl2	$4,sp		#convert length to ascii
	cvtlp	r2,$7,(sp)
	cvtps	$7,(sp),$7,(r5)	#place ascii in format
	addl2	$4,sp
	skpc	$'0,$7,1(r5)	#remove sign and leading zeros
	movc3	r0,(r1),(r5)	#r3 pts to next free byte
l3145:
	subb3	$'0,(r6)+,r0	#move r6 over default width
	blss	l3146		#(note: could use 'spanc' for this)
	cmpb	r0,$9
	bleq	l3145
l3146:
	decl	r6
l3147:
	movb	(r6)+,(r3)+	#copy remainder of format
	bneq	l3147		#(note: could use 'movtuc' for this)
	movl	sp,r6		#r6 pts to format to be used
l3148:
	movc3	8(ap),len+args(ap),( r9)  #move up data to make room for format
	addl3	8(ap), r9,r0	#r0 pts to space for format
	movc3	$len,(r6),(r0)	#copy in format
	movl	 r9,r0		#r0 pts to top of stack on return
	ret
