HZ = 60.
/
/ Berkeley Pascal Assembler Kernel
/
.globl _interpret
.globl _pcttot, _pdattim, _alloc, _free, _error, _fflush, _maxstk
.globl _pputch, _pmessage, _pwrite, _pwril
.globl atan, cos, exp, log, sin, sqrt
.globl fptrap
.globl _display, _dp, _lino, _draino
.globl _seed, _randa, _randc, _randm, _randim
.globl _atof
.globl _argc, _argv, _errno
.globl _file, _nodump, _pxpbuf, _pmflush
.if FP
fptrap:
	4
.endif
/
indir = 0
return = 115		/ jmp (lp)
times = 43.
lp = r5
lc = r4
error = 104000 ^ sys
/
ONE = 040200		/$1.0
HALF = 040000		/$.5
/
_interpret:
	setl
	sys	signal; 7.; onemt
.if !FP
	sys	signal; 4.; fptrap
.endif
.if FP
	sys	signal; 4.; 0
.endif
	mov	$_display,_dp
	mov	$loop,lp
	mov	2(sp),lc
	mov	sp,*_dp
	mov	$_unit0,-(sp)
	mov	$_unit1,-(sp)
/
/ main interpreter loop
/ the pseudo-op 'return'
/ transfers here
/
loop:
	movb	(lc)+,r0
	add	r0,r0
	movb	(lc)+,r3
	jmp	*optab(r0)
badop:
	mov	$EBADOP,_perrno
	error	EBADOP
/cloop:
/	movb	(lc)+,r0
/	mov	r0,r1
/	add	r0,r0
/	add	$128.,r1
/	ash	$2,r1
/	add	$cntab,r1
/	add	$1,2(r1)
/	adc	(r1)
/	movb	(lc)+,r3
/	jmp	*optab(r0)
/.bss
/cntab:
/	.=.+1024.
/.text
.bss
_perrno:	.=.+2
.text
onemt:
	setd
/	mov	(sp)+,r0
/	sub	$2,r0
/.if SEPID
/	sys	61.		/fetchi
/.endif
/.if !SEPID
/	mov	(r0),r0
/.endif
/	bic	$!377,r0
/	mov	r0,-(sp)
	mov	_perrno,-(sp)
	jsr	pc,_error
/
_ABORT:
	4
_HALT:
	mov	$EHALT,_perrno
	error	EHALT
_NODUMP:
	inc	_nodump
_BEG:
	mov	(lc)+,r3
	add	$2,_dp
	mov	*_dp,-(sp)
	mov	lc,-(sp)
	add	$10.,lc
	mov	buf,-(sp)
	mov	_file,-(sp)
	tst	-(sp)
	mov	sp,*_dp
	mov	sp,r0
	add	r3,r0
	cmp	r0,sp
	bhi	9f
	cmp	r0,_maxstk
	blos	9f
	tst	r3
	beq	2f
1:
	clr	-(sp)
	cmp	sp,r0
	bhi	1b
2:
	mov	*_dp,r1
	mov	sp,(r1)
	return
9:
	mov	$ESTKOVFLO,_perrno
	error	ESTKOVFLO
_END:
	jsr	pc,blkexit
	mov	*_dp,sp
	tst	(sp)+
	mov	(sp)+,_file
	mov	(sp)+,buf
	tst	(sp)+
	mov	(sp)+,*_dp
	cmp	_dp,$_display+2
	bhi	3f
	mov	$_unit1,r0
	mov	FBUF(r0),-(sp)
	jsr	pc,_fflush
	jsr	pc,_pmflush
	clr	r0
/	sys	creat; cntdata; 0644
/	sys	write; cntab; 1024.
/.data
/cntdata: <counts\0>
/.even
/.text
	jsr	pc,_psexit
.globl _psexit
3:
	mov	(sp)+,_dp
	mov	(sp)+,lc
	mov	(sp)+,_lino
	return
_CALL:
	mov	lc,r0
	add	(lc)+,r0
	mov	_lino,-(sp)
	mov	lc,-(sp)
	mov	_dp,-(sp)
	add	$_display,r3
	mov	r3,_dp
	mov	r0,lc
	return
_PUSH:
	bne	1f
	mov	(lc)+,r3
1:
	mov	sp,r0
	add	r3,r0
1:
	cmp	sp,r0
	blos	1f
	clr	-(sp)
	br	1b
1:
	return
_POP:
	bne	1f
	mov	(lc)+,r3
1:
	add	r3,sp
	return
_SDUP:
	mov	(sp),-(sp)
	return
_IF:
	tstb	(sp)+
	beq	_TRA
	tst	(lc)+
	return
_ASRT:
	tstb	(sp)+
	beq	9f
	return
9:
	mov	$EASRT,_perrno
	error	EASRT
_TRA:
	add	(lc),lc
	return
_LINO:
	bne	1f
	mov	(lc)+,r3
1:
	mov	r3,_lino
	mov	*_dp,r1
	cmp	(r1),sp
	bne	2f
	add	$1,_stcnt+2
	adc	_stcnt
	sub	$1,_stlim+2
	sbc	_stlim
	bne	1f
	tst	_stlim+2
	bne	1f
	mov	$ESTLIM,_perrno
	error	ESTLIM
1:
	return
2:
	mov	$ESTKNEMP,_perrno
	error	ESTKNEMP
.data
.globl	_stcnt, _stlim
_stcnt:	0
	0
_stlim: 7
	120440
.text
_GOTO:
	add	(lc),lc
	add	$_display,r3
1:
	mov	*_dp,r2
	cmp	r3,_dp
	beq	1f
	cmp	_dp,$_display
	beq	9f
	jsr	pc,blkexit
	tst	(r2)+
	mov	(r2)+,_file
	mov	(r2)+,buf
	tst	(r2)+
	mov	(r2)+,*_dp
	mov	(r2)+,_dp
	br	1b
1:
	mov	(r2),sp
	return
9:
	mov	$EGOTO,_perrno
	error	EGOTO
blkexit:
	mov	r2,-(sp)
	mov	fchain,r2
1:
	tst	r2
	beq	2f
	cmp	FLEV(r2),*_dp
	bhi	2f
	bit	$FWRITE,FUNIT(r2)
	beq	4f
	mov	FBUF(r2),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
4:
	mov	FUNIT(r2),r0
	bmi	3f
	bic	$!17,r0
	sys	close
	bec	3f
	mov	PFNAME(r2),_file
	mov	$ECLOSE,_perrno
	error	ECLOSE
3:
	bit	$TEMP,FUNIT(r2)
	beq	4f
	mov	PFNAME(r2),0f
	sys	indir;8f
.data
8:	sys	unlink;0: ..
.text
	bec	4f
	mov	PFNAME(r2),_file
	mov	$EREMOVE,_perrno
	error	EREMOVE
4:
	mov	FNAME(r2),-(sp)
	jsr	pc,_free
	mov	r2,(sp)
	mov	FCHAIN(r2),r2
	sub	$518.+14.,(sp)
	jsr	pc,_free
	tst	(sp)+
	br	1b
2:
	mov	r2,fchain
	mov	(sp)+,r2
	rts	pc
_pmflush:
	tst	_pxpbuf
	beq	1f
	sys	creat; pmonout; 644
	bcs	9f
	sys	indir; 8f
.data
8:	sys	write; _pxpbuf: ..; _pxpsize: ..
pmonout: <pmon.out\0>
.even
.text
	bcs	9f
1:
	rts	pc
9:
.globl	_errno
	mov	r0,_errno
	mov	$pmonout,-(sp)
.globl	_perror
	jsr	pc,_perror
	mov	$1,r0
	sys	exit
.globl __exit
	mov	2(sp),r0
	sys	exit
