#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)32iostat.s 4.2 10/16/80";
#
# FILE ACTIVATION AND STATUS OPERATIONS
#
_UNIT:
	incl	r10
	calls	$1,_unit
	jmp	(r8)
_UNITINP:
	incl	r10
	pushal	stdin
	calls	$1,_unit
	jmp	(r8)
_UNITOUT:
	incl	r10
	moval	stdout,r7
	movl	stdout+PFNAME,_file
	jmp	(r8)
_EOF:
	incl	r10
	movl	r7,r6		#save active file
	calls	$1,_unit
	movw	$1,-(sp)
	bitw	$EOF,FUNIT(r7)
	bneq	l3202
	calls	$0,_iosync
	bitw	$EOF,FUNIT(r7)
	bneq	l3202
	clrw	(sp)
l3202:
	movl	r6,r7		#restore active file
	movl	PFNAME(r7),_file
	jmp	(r8)
_EOLN:
	incl	r10
	movl	r7,r6		#save active file
	calls	$1,_unit
	clrw	-(sp)
	calls	$0,_iosync
	bitw	$EOLN,FUNIT(r7)
	beql	l3205
	incw	(sp)
l3205:
	movl	r6,r7		#restore active file
	movl	PFNAME(r7),_file
	jmp	(r8)
