#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)33iofile.s 4.2 10/14/80";
#
# FILE HOUSEKEEPING OPERATIONS
#
_DEFNAME:
	incl	r10
	calls	$4,_getname
	bisw2	$FDEF,FUNIT(r0)
	jmp	(r8)
_BUFF:
	cvtbl	(r10)+,r0
	bneq	l3301
	pushl	$0
	brb	l3303
l3301:
	cmpl	r0,$1
	bgtr	l3302
	jmp	(r8)
l3302:
	pushal	__sobuf
l3303:
	pushl	stdout+FBUF
	calls	$2,_setbuf
	jmp	(r8)
_RESET:
	incl	r10
	tstl	8(sp)		#attempt to rewind only if stdin
	bneq	l3304		# and no name is given
	cmpl	*12(sp),$stdin
	bneq	l3304
	tstb	stdin+FNAME
	bneq	l3304
	pushl	stdin+FBUF
	calls	$1,_rewind
	tstl	r0		# -1 => error
	blss	eseek
	addl2	$16,sp		#clear stack
	bicw2	$EOF+EOLN,stdin+FUNIT
	bisw2	$SYNC,stdin+FUNIT
	jmp	(r8)
l3304:
	calls	$4,_getname
	movl	r0,r6
	pushal	rdopen
	pushal	FNAME(r6)
	calls	$2,_fopen
	tstl	r0			#check for valid open
	bneq	l3305
	bbc	$fTEMP,FUNIT(r6),eopen	#if TEMP file, set at EOF
	bisw2	$EOF,FUNIT(r6)
l3305:
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
	incl	r10
	calls	$4,_getname
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
_FILE:
	incl	r10
	pushl	FBUF(r7)
	jmp	(r8)
_FLUSH:
	incl	r10
	calls	$1,_unit
	bbc	$fWRITE,FUNIT(r7),l3306
	pushl	FBUF(r7)
	calls	$1,_fflush
l3306:
	jmp	(r8)
_REMOVE:
	incl	r10
	movl	(sp)+,r4		#r4 has max name length
	movl	(sp)+,r5		#r5 pts to name
	locc	$blank,r4,(r5)		#check for trailing blanks
	subl2	r0,r4			#deduct blanks if any
	addl3	$2,r4,r6		#r6 has name + 1 aligned to word
	bicl2	$1,r6
	subl2	r6,sp			#allocate space
	movc5	r4,(r5),$0,r6,(sp)	#move in name with zero end
	pushl	sp			#unlink file
	calls	$1,_unlink
	tstl	r0			#check for errors
	bneq	eremove
	addl2	r6,sp			#deallocate space
	jmp	(r8)
eremove:
	movl	sp,_file		#point to name
	movw	$EREMOVE,_perrno
	jbr	error
_MESSAGE:
	incl	r10
	calls	$0,_pflush
	pushal	stderr
	calls	$1,_unit
	jmp	(r8)
