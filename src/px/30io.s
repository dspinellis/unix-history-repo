/
/ IO operations
/
_GET:
	jsr	pc,get
	jsr	pc,iosync
	return
_PUT:
	jsr	pc,put
	return
_MESSAGE:
	mov	$_unit2,r0
	jsr	pc,unit
	mov	fchain,r2	/ flush all files
1:
	beq	2f
	bit	$FWRITE,FUNIT(r2)
	beq	3f
	mov	FBUF(r2),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
3:
	mov	FCHAIN(r2),r2
	br	1b
2:
	mov	$_unit1,r2
	mov	FBUF(r2),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
	return
_FNIL:
	mov	(sp),r0
	mov	_file,-(sp)
	mov	buf,-(sp)
	jsr	pc,unit
	jsr	pc,iosync
	mov	(sp)+,buf
	mov	(sp)+,_file
	return
_BUFF:
	mov	r3,bufopt
	return
_EOF:
	mov	(sp)+,r0
	mov	_file,-(sp)
	mov	buf,-(sp)
	mov	$EOF,-(sp)
	br	1f
_EOLN:
	mov	(sp)+,r0
	mov	_file,-(sp)
	mov	buf,-(sp)
	mov	$EOF+EOLN,-(sp)
1:
	jsr	pc,unit
	cmp	buf,$_unit0
	bne	1f
	cmp	bufopt,$1
	bne	1f
	mov	$u1buf,-(sp)
	jsr	pc,_fflush
	tst	(sp)+
1:
	mov	(sp)+,r1
	clr	-(sp)
	mov	buf,r2
	bit	$EOF,FUNIT(r2)
	bne	2f
	jsr	pc,iosync
	bit	r1,FUNIT(r2)
	beq	1f
2:
	mov	$1,(sp)
1:
	mov	(sp)+,r0
	mov	(sp)+,buf
	mov	(sp)+,_file
	mov	r0,-(sp)
	return
_RESET:
	bne	1f		/ branch if name given
	cmp	*(sp),$_unit0
	bne	1f
	tst	_unit0+FNAME
	bne	1f
	clr	r0
	sys	seek; 0; 0
	bes	9f
	tst	(sp)+
	tst	(lc)+
	bic	$EOF+EOLN,_unit0+FUNIT
	bis	$SYNC,_unit0+FUNIT
	return
9:
	mov	$stdin,_file
	mov	r0,_errno
	mov	$ESEEK,_perrno
	error	ESEEK
1:
	jsr	pc,getname
	mov	r0,r1
	mov	PFNAME(r1),_file
	mov	FNAME(r1),openrnm
	sys	indir; openr
	bes	eopen
	mov	r0,*FBUF(r1)
	bis	$SYNC|FREAD,r0
	bis	r0,FUNIT(r1)
	return
ecreat:
	mov	r0,_errno
	mov	$ECREATE,_perrno
	error	ECREATE
eopen:
	mov	r0,_errno
	mov	$EOPEN,_perrno
	error	EOPEN
.data
openr:	sys	open; openrnm: .. ; 0
creit:	sys	creat; crenm: ..; 0644
openw:	sys	open; openwnm: .. ; 1
.text
_REWRITE:
	jsr	pc,getname
	mov	r0,r1
	mov	PFNAME(r1),_file
	mov	FNAME(r1),crenm
	sys	indir; creit
	bes	ecreat
	sys	close
	mov	FNAME(r1),openwnm
	sys	indir; openw
	bes	eopen
	mov	r0,*FBUF(r1)
	bis	$EOF|FWRITE,r0
	bis	r0,FUNIT(r1)
	return
_REMOVE:
	bne	1f
	mov	(lc)+,r3
1:
	mov	sp,r1
	mov	r3,r2
	inc	r2
	bic	$1,r2
	add	r2,sp
	add	r3,r1
	mov	r1,r0
	movb	(r1),r2
	clrb	(r1)
1:
	cmpb	-1(r1),$' 
	bne	1f
	clrb	-(r1)
	dec	r3
	bne	1b
1:
	sub	r3,r1
	mov	r1,1f
	mov	r1,_file
	sys	indir; 0f
	bes	9f
	movb	r2,(r0)
	return
.data
0:	sys	unlink; 1: ..
.text
9:
	mov	_file,sp
	mov	r0,_errno
	mov	$EREMOVE,_perrno
	error	EREMOVE
_UNITINP:
	cmp	bufopt,$2
	bge	1f			/ bufopt >= 2, don't flush on read
	cmp	$512.,u1cnt
	beq	1f
	mov	$u1buf,-(sp)
	jsr	pc,_fflush
	tst	(sp)+
1:
	mov	$_unit0,buf
	mov	$stdin,_file
	return
_UNITOUT:
	mov	$_unit1,buf
	mov	$stdout,_file
	return
_UNIT:
	mov	(sp)+,r0
	cmp	r0,$_unit0
	beq	_UNITINP
	jsr	pc,unit
	return
_READ8:
	sub	$32.,sp
	mov	sp,-(sp)
	mov	$32.,-(sp)
	mov	$1,-(sp)
	jsr	pc,_preadn
	bcs	1f
	add	$6,sp
	mov	sp,-(sp)
	jsr	pc,_atof
	add	$34.,sp
	movf	fr0,-(sp)
	return
1:
	add	$38.,sp
	clrf	-(sp)
	return
_READ4:
	sub	$20.,sp
	mov	sp,-(sp)
	mov	$20.,-(sp)
	clr	-(sp)
	jsr	pc,_preadn
	bcs	1f
	add	$6.,sp
	mov	sp,-(sp)
	jsr	pc,_atol
	add	$22.,sp
	mov	r1,-(sp)
	mov	r0,-(sp)
	return
1:
	add	$26.,sp
	clr	-(sp)
	sxt	-(sp)
	return
_READC:
	jsr	pc,iosync
	clr	-(sp)
	movb	*buf,(sp)
	jsr	pc,get
	return
_READLN:
	jsr	pc,iosync
	mov	buf,r0
	bit	$EOLN+EOF,FUNIT(r0)
	bne	1f
	jsr	pc,get
	br	_READLN
1:
	jsr	pc,get
	return
_PAGE:
	mov	$14,-(sp)
	br	1f
_WRITC:
	beq	1f
	mov	$323,r0
	br	pwriteit
1:
	movb	(sp)+,*buf
	jsr	pc,put
	tst	bufopt
	beq	2f
	return
_WRITLN:
	mov	$327,r0
	mov	$'\n,-(sp)
	mov	$1,r2
	cmp	buf,$_unit1
	bne	1f
	cmp	bufopt,$1
	bgt	1f
	clr	r2
	br	1f
_WRITG:
	mov	$325,r0
	mov	(lc)+,-(sp)
	br	pwriteit
_WRHEX2:
	mov	$267,r0
	br	3f
_WRHEX4:
	mov	$277,r0
	br	3f
_WROCT2:
	mov	$330,r0
	br	3f
_WROCT4:
	mov	$331,r0
	br	3f
_WRIT2:
	mov	$320,r0
	br	3f
_WRIT4:
	mov	$321,r0
	br	3f
_WRIT8:
	mov	$324,r0
	br	3f
_WRITB:
	mov	$322,r0
	br	3f
_WRIT82:
	mov	$326,r0
3:
pwriteit:
	mov	bufopt,r2
1:
	mov	r3,-(sp)
	mov	r0,-(sp)
	jsr	pc,_pwrite
	mov	r0,sp
	tst	r2
	bne	1f
2:
	cmp	buf,$_unit1
	bne	1f
	mov	buf,r0
	mov	FBUF(r0),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
1:
	return
_DEFNAME:
	jsr	pc,getname
	bis	$100000,FUNIT(r0)
	return
_FLUSH:
	mov	(sp)+,r0
	jsr	pc,unit
	mov	buf,r0
	bit	$FWRITE,FUNIT(r0)
	beq	1f
	mov	FBUF(r0),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
1:
	return
