#
# 32iostat.s
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
	cvtwl	$EOF,r5
	brb	l3202
_EOLN:
	cvtwl	$EOF+EOLN,r5
l3202:
	incl	r10
	movl	_file,r4	#save active file
	movl	r7,r3
	calls	$1,_unit
	clrw	-(sp)
	bbs	$fEOF,FUNIT(r7),l3204
	calls	$0,_iosync
	bitw	r5,FUNIT(r7)
	beql	l3205
l3204:
	incw	(sp)
l3205:
	movl	r3,r7		#restore active file
	movl	r4,_file
	jmp	(r8)
