/
/ IO SUBROUTINES
/
	.globl	_getc,_putc
_pmessage:
	mov	r3,-(sp)
	mov	_file,r3
	mov	$_unit2,r0
	jsr	pc,unit
	mov	r3,_file
	mov	fchain,r3	/ flush all files
1:
	tst	r3
	beq	2f
	bit	$FWRITE,FUNIT(r3)
	beq	3f
	mov	FBUF(r3),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
3:
	mov	FCHAIN(r3),r3
	br	1b
2:
	mov	$_unit1,r3
	mov	FBUF(r3),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
	mov	(sp)+,r3
	rts	pc
_pputch:
	mov	2(sp),*buf
	jsr	pc,put
	rts	pc
/
/ iosync insures that
/ a useable image is in
/ the buffer window.
/
2:
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
iosync:
	mov	buf,r0
	bit	$SYNC,FUNIT(r0)		/ dirty bit
	bne	1f
	rts	pc
1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r0,r2
	bic	$SYNC,FUNIT(r2)
	bit	$EOF,FUNIT(r2)
	bne	pasteof
	mov	FSIZE(r2),r3
1:
	mov	buf,r0
	mov	FBUF(r0),-(sp)
	jsr	pc,_getc
	tst	(sp)+
	tst	r0
	bmi	eof
	movb	r0,(r2)+
	sob	r3,1b
	mov	buf,r0
	bic	$EOLN,FUNIT(r0)
	bit	$FTEXT,FUNIT(r0)
	beq	2b
	cmpb	*buf,$'\n
	bne	2b
	bis	$EOLN,FUNIT(r0)
	movb	$' ,*buf
	br	2b
eof:
	mov	buf,r0
	bis	$EOF,FUNIT(r0)
	br	2b
pasteof:
	mov	$EPASTEOF,_perrno
	error	EPASTEOF
/
/ get insures that
/ something is in the window
/ and then sets the dirty
/ bit effectively throwing the
/ windowed data away
/
get:
	mov	r1,-(sp)
	mov	buf,r1
	bit	$FREAD,FUNIT(r1)
	bne	1f
	mov	$EREADIT,_perrno
	error	EREADIT
1:
	jsr	pc,iosync
	bis	$SYNC,FUNIT(r1)
	mov	(sp)+,r1
	rts	pc
put:
	cmp	buf,$_unit2
	beq	6f
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	buf,r2
	bit	$FWRITE,FUNIT(r2)
	bne	1f
	mov	$EWRITEIT,_perrno
	error	EWRITEIT
1:
	mov	FSIZE(r2),r3
1:
	mov	buf,r0
	mov	FBUF(r0),-(sp)	/ the hidden buffer
	movb	(r2)+,-(sp)	/ the character to write
	jsr	pc,_putc
	cmp	(sp)+,(sp)+
	tst	_errno		/ error occurred?
	bne	9f
	sob	r3,1b
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc
9:
	mov	$EWRITE,_perrno
	error	EWRITE
6:
	mov	$2,r0
	sys	write; _unit2; 1
	bes	9b
	cmp	r0,$1
	bne	9b
	rts	pc
unit:
	mov	r0,buf
	beq	1f
	tst	FUNIT(r0)
	bmi	1f
	mov	PFNAME(r0),_file
	rts	pc
1:
	mov	$EREFINAF,_perrno
	error	EREFINAF
/
/ standard files
/
.data
/
/ unit 0 for "input"
/
u0buf:	.=.+518.
	.-518.		/ buffer pointer
	0
	0
	stdin
	0		/ file name
	FTEXT+FREAD+SYNC+0		/ flags & unit number
	1		/ size of items
_unit0:
	0		/ window
/
/ unit 1 for "output"
/
u1buf:	1		/ file descriptor for putc
_draino:
u1cnt:	.=.+516.
	u1buf		/ putc buffer pointer
	0
	0
	stdout
	0
	FTEXT+FWRITE+1+EOF
	1
_unit1:
	0
/
/ unit2 for <message>
/
	0
	0
	0
	mesgf
	0
	FTEXT+FWRITE+2
	1
_unit2:
	0
/
FBUF = -14.
FCHAIN = -12.
FLEV = -10.
PFNAME = -8.
FNAME = -6
FUNIT = -4
FSIZE = -2
nextu:	'0		/ next temp file
tnam:	<tmp.x\0>
.even
.bss
buf:	.=.+2
fchain:	.=.+2
.data
bufopt:	1
stdin:	<standard input\0>
stdout:	<standard output\0>
mesgf:	<message file\0>
.text
EOF = 400
EOLN = 1000
SYNC = 2000
TEMP = 4000
FREAD = 10000
FWRITE = 20000
FTEXT = 40000
