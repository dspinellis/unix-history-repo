#
# 33iofile.s
#
# FILE HOUSEKEEPING OPERATIONS
#
_DEFNAME:
	calls	$0,_getname
	movl	r1,sp
	bisw2	$FDEF,FUNIT(r0)
	jmp	(r8)
_BUFF:
	cvtbw	(r10)+,_bufopt
	jmp	(r8)
_RESET:
	cvtbl	(r10),r3		#attempt to rewind only if stdin
	bneq	l3301		# and no name is given
	cmpl	*(sp),$stdin
	bneq	l3301
	tstb	stdin+FNAME
	bneq	l3301
	pushl	stdin+FBUF
	calls	$1,_rewind
	tstl	r0		# -1 => error
	blss	eseek
	addl2	$3,r10
	addl2	$4,sp		#clear stack
	bicw2	$EOF+EOLN,stdin+FUNIT
	bisw2	$SYNC,stdin+FUNIT
	jmp	(r8)
l3301:
	calls	$0,_getname
	movl	r1,sp
	movl	r0,r6
	pushal	rdopen
	pushal	FNAME(r6)
	calls	$2,_fopen
	tstl	r0
	beql	eopen
	movl	r0,FBUF(r6)
	bisw2	$SYNC+FREAD,FUNIT(r6)
	jmp	(r8)
eseek:
	movl	stdin+PFNAME,_file
	movw	$ESEEK,_perrno
	jbr	error
eopen:
	movl	PFNAME(r6),_file
	movw	$EOPEN,_perrno
	jbr	error
_REWRITE:
	calls	$0,_getname
	movl	r1,sp
	movl	r0,r6
	movl	PFNAME(r6),_file
	pushal	wtopen
	pushal	FNAME(r6)
	calls	$2,_fopen
	tstl	r0
	beql	ecreat
	movl	r0,FBUF(r6)
	bisw2	$EOF+FWRITE,FUNIT(r6)
	jmp	(r8)
ecreat:
	movw	$ECREATE,_perrno
	jbr	error
_FLUSH:
	incl	r10
	calls	$1,_unit
	bbc	$fWRITE,FUNIT(r7),l3302
	pushl	FBUF(r7)
	calls	$1,_fflush
l3302:
	jmp	(r8)
_REMOVE:
	cvtbl	(r10)+,r3	#r3 has filename length
	bneq	l3303
	cvtwl	(r10)+,r3
l3303:
	movl	r3,r6		#r6 has stack length
	blbc	r6,l3304
	incl	r6
l3304:
	addl3	r3,sp,r1	#r1 pts to end of name
l3305:
	cmpb	-(r1),$blank	#delete trailing blanks
	bneq	l3306		#(note: could use "spanc" here)
	clrb	(r1)
	sobgtr	r3,l3305
l3306:
	movl	sp,_file	#remove file
	pushl	sp
	calls	$1,_unlink
	tstl	r0
	bneq	eremove
	addl2	r6,sp
	jmp	(r8)
eremove:
	movl	_file,sp	#recover filename
	movw	$EREMOVE,_perrno
	jbr	error
_MESSAGE:
	incl	r10
	calls	$0,_pflush
	pushal	stderr
	calls	$1,_unit
	jmp	(r8)
