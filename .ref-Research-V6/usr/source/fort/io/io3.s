/
/

/ io3 --  Fortran I/O

.globl	getbuf
.globl	chkunit
.globl	creatf
.globl	openf

setio:
	mov	r1,unit
	jsr	r5,chkunit
	movb	utable(r1),r0
	beq	1f
	bpl	2f
	mov	r1,r0
	asl	r0
	mov	btable(r0),r0
	mov	r0,r2
	br	4f
2:
	cmp	(r5),r0
	beq	3f
	jsr	r5,rerr; 101.		/ inconsistent use of unit
	sys	exit
1:
	mov	r1,-(sp)
	clr	r0
	dvd	$10.,r0
	swab	r1
	bis	r1,r0
	add	$"00,r0
	mov	r0,filnam+4
	mov	(sp)+,r1
	jsr	r5,getbuf
	mov	$filnam,r0
4:
	movb	(r5),utable(r1)
	bit	$1,(r5)
	bne	2f
	jsr	r5,creatf
	br	3f
2:
	jsr	r5,openf
3:
	tst	(r5)+
	asl	r1
	mov	btable(r1),buffer
	rts	r5

getbuf:
	mov	$utable,r0
	mov	$btable,r2
1:
	tstb	(r0)+
	beq	2f
	tst	(r2)+
	br	3f
2:
	tst	(r2)+
	beq	3f
	mov	-(r2),r0
	clr	(r2)
	mov	r0,r2
	br	2f
3:
	cmp	r0,$utable+20.
	blo	1b
	mov	bufp,r2
	add	$134.,bufp
	mov	bufp,0f
	sys	break; 0:..
2:
	mov	r1,r0
	asl	r0
	mov	r2,btable(r0)
	mov	r2,buffer
	rts	r5

chkunit:
	cmp	r1,$20.
	blo	1f
	jsr	r5,rerr; 100.		/ illegal unit number
	sys	exit
1:
	rts	r5

creatf:
	cmp	unit,$6
	bne	2f
	mov	$1,r0
	br	1f
2:
	mov	r0,0f
	sys	creat; 0:..; 666
	bec	1f
	jsr	r5,rerr; 102.		/ create error
	sys	exit
1:
	mov	r2,-(sp)
	mov	r0,(r2)+
	clr	(r2)+
	clr	(r2)+
	mov	r2,-(r2)
	mov	(sp)+,r2
	rts	r5

openf:
	cmp	unit,$5
	bne	2f
	clr	r0
	br	1f
2:
	mov	r0,0f
	sys	open; 0:..; 0
	bec	1f
	jsr	r5,rerr; 103.		/ open error
	sys	exit
1:
	mov	r2,-(sp)
	mov	r0,(r2)+
	clr	(r2)+
	clr	(r2)+
	mov	(sp)+,r2
	rts	r5

fputc:
	mov	r1,-(sp)
	mov	buffer,r1
	dec	2(r1)
	bge	1f
	mov	r0,-(sp)
	jsr	pc,flush1
	dec	2(r1)
	mov	(sp)+,r0
1:
	movb	r0,*4(r1)
	inc	4(r1)
	mov	(sp)+,r1
	rts	r5

fflush:
	mov	r1,-(sp)
	mov	buffer,r1
	jsr	pc,flush1
	mov	(sp)+,r1
	rts	r5

flush1:
	mov	r1,r0
	add	$6,r0
	mov	r0,-(sp)
	mov	r0,0f
	neg	r0
	add	4(r1),r0
	bhis	1f
	mov	r0,0f+2
	mov	(r1),r0
	sys	write; 0:..; ..
1:
	mov	(sp)+,4(r1)
	mov	$128.,2(r1)
	rts	pc

fgetc:
	tst	nlflg
	bne	4f
	mov	r1,-(sp)
	mov	buffer,r1
	dec	2(r1)
	bge	1f
	mov	r1,r0
	add	$6,r0
	mov	r0,0f
	mov	r0,4(r1)
	mov	(r1),r0
	sys	read; 0:..; 128.
	bes	2f
	tst	r0
	bne	3f
2:
	jsr	r5,rerr; 104.		/ EOF on input
	sys	exit
3:
	dec	r0
	mov	r0,2(r1)
1:
	clr	r0
	bisb	*4(r1),r0
	inc	4(r1)
	mov	(sp)+,r1
	tst	binflg
	bne	1f
	cmp	r0,$'\n
	bne	1f
4:
	mov	pc,nlflg
	mov	$' ,r0
1:
	rts	r5

gnum:
	mov	r1,-(sp)
	clr	r1
1:
	jsr	r5,fmtchr
	cmp	r0,$'  /
	beq	1b
	sub	$'0,r0
	cmp	r0,$9.
	bhi	1f
	mpy	$10.,r1
	add	r0,r1
	br	1b
1:
	mov	r1,r0
	mov	(sp)+,r1
	dec	formp
	rts	r5

switch:
	mov	(r5)+,r1
1:
	tst	(r1)
	beq	1f
	cmp	r0,(r1)+
	bne	1b
	tst	(sp)+
	jmp	*(r1)
1:
	rts	r5

fmtchr:
	movb	*formp,r0
	inc	formp
	rts	r5

getitm:
	tst	itmflg
	bne	1f
	mov	r5,-(sp)
	jmp	*(r4)+
1:
	clr	itmflg
	tst	(r5)+
	rts	r5

/ just a fake, there's no carriage control

fputcc:
	cmp	$' ,r0
	bne	1f
	inc	nspace
	rts	r5
1:
	mov	r0,-(sp)
1:
	dec	nspace
	blt	1f
	mov	$' ,r0
	jsr	r5,fputc
	br	1b
1:
	clr	nspace
	mov	(sp)+,r0
	beq	1f
	jsr	r5,fputc
1:
	rts	r5

eorec:
	mov	unit,r0
	bitb	$1,utable(r0)
	bne	1f
	clr	nspace
	mov	$'\n,r0
	jsr	r5,fputc
eorec1:
	clr	r0
	jsr	r5,fputcc
/	cmp	unit,$6			/ tty output
/	bne	2f
	jsr	r5,fflush
2:
	rts	r5
1:
	tst	nlflg
	bne	1f
	jsr	r5,fgetc
	br	1b
1:
	clr	nlflg
	rts	r5

spaces:
	add	r1,nspace
	rts	r5

