/ ls -- list file or directory

.globl	flush
.globl	fopen
.globl	getw
.globl 	getc
.globl	putc
.globl	ctime
.globl	end

	sys	break; end+512.
	mov	$1,obuf
	mov	sp,r5
	mov	(r5)+,count
	tst	(r5)+
	dec	count
	mov	count,ocount
	bgt	loop
	mov	$dotp,r5
loop:
	mov	(r5)+,r4
	cmpb	(r4)+,$'-
	bne	1f
	dec	ocount
3:
	movb	(r4)+,r0
	beq	eloop
	cmp	r0,$'l
	bne	4f
	inc	longf
	br	3b
4:
	cmpb	r0,$'t
	bne	4f
	mov	$14.,sortoff
	br	3b
4:
	cmpb	r0,$'a
	bne	4f
	inc	allflg
	br	3b
4:
	cmpb	r0,$'s
	bne	4f
	incb	longf+1
	br	3b
4:
	cmpb	r0,$'d
	bne	3b
	inc	dirflg
	br	3b
1:
	dec	r4
	jsr	pc,do
eloop:
	dec	count
	bgt	loop
	tst	dnp
	bne	1f
	mov	$dotp,r5
	br	loop
1:
	jsr	r5,flush; obuf
	sys	exit

do:
	clr	tblocks
	mov	$end,r1
	mov	$filnam,r3
	mov	r4,dnp
	clr	isadir
	tst	dirflg
	bne	nondir
	sys	stat; dnp: 0; statb
	bec	1f
	jsr	r5,questf; < nonexistent\n\0>; .even
	rts	pc
1:
	bit	$40000,statb+2	/test directory
	beq	nondir
	inc	isadir
	mov	r4,r0
	jsr	r5,fopen; dbuf
	bcc	1f
	jsr	r5,questf; < unreadable\n\0>; .even
	rts	pc
1:
	movb	(r4)+,(r3)+
	bne	1b
	dec	r3
	cmpb	-1(r3),$'/
	beq	1f
	movb	$'/,(r3)+
1:
	jsr	r5,getw; dbuf
	bcs	pass2
	mov	$4,-(sp)
	tst	r0
	bne	2f
3:
	jsr	r5,getw; dbuf
	dec	(sp)
	bne	3b
	tst	(sp)+
	br	1b
2:
	mov	r3,r2
2:
	jsr	r5,getw; dbuf
	mov	r0,(r1)+
	movb	r0,(r2)+
	swab	r0
	movb	r0,(r2)+
	dec	(sp)
	bne	2b
	tst	(sp)+
	clrb	(r2)+
	tst	allflg
	bne	2f
	cmpb	(r3),$'.
	bne	2f
	sub	$8.,r1
	br	1b
2:
	jsr	r5,gstat
	br	1b
nondir:
	mov	r3,r2
1:
	movb	(r4)+,(r2)+
	bne	1b
1:
	cmp	r2,r3
	blos	1f
	cmpb	-(r2),$'/
	bne	1b
	inc	r2
1:
	mov	$8.,-(sp)
1:
	movb	(r2)+,(r1)+
	bne	2f
	dec	r2
2:
	dec	(sp)
	bne	1b
	jsr	r5,gstat
	tst	(sp)+

pass2:
	mov	dbuf,r0
	sys	close
	mov	$end,r2
	cmp	r1,r2
	bne	1f
	rts	pc
1:
	mov	r5,-(sp)
	mov	r1,-(sp)
	add	sortoff,r2
1:
	mov	r2,(r1)+
	add	$20.,r2
	cmp	r2,(sp)
	blo	1b
	mov	(sp),r2
	tst	-(r1)
1:
	mov	r2,r3
2:
	tst	(r3)+
	cmp	r3,r1
	bhi	2f
	mov	(r2),r4
	mov	(r3),r5
	tst	sortoff
	beq	4f
	cmp	(r4)+,(r5)+
	blo	3f
	bhi	2b
	cmp	(r4)+,(r5)+
	blo	3f
	br	2b
4:
4:
	cmpb	(r4)+,(r5)+
	bhi	3f
	blo	2b
	dec	r0
	br	4b
3:
	mov	(r2),-(sp)
	mov	(r3),(r2)
	mov	(sp)+,(r3)
	br	2b
2:
	tst	(r2)+
	cmp	r2,r1
	blo	1b
1:
	mov	(sp)+,r2
	mov	(sp)+,r5

pass3:
	cmp	ocount,$1
	ble	1f
	tst	isadir
	beq	2f
	mov	dnp,0f
	jsr	r5,pstring; 0:..
	jsr	r5,pstring; colon
1:
	tst	longf
	beq	1f
	jsr	r5,pstring; totmes
	mov	tblocks,r0
	jsr	r5,decimal; 4
	jsr	r5,pstring; nl
2:
	tstb	longf
	beq	1f
	mov	$passwd,r0
	jsr	r5,fopen; iobuf
	bes	1f
	mov	$uidbuf,r3
3:
2:
	jsr	r5,getc; iobuf
	bes	3f
	movb	r0,(r3)+
	cmpb	r0,$':
	bne	2b
2:
	jsr	r5,getc; iobuf
	cmpb	r0,$':
	bne	2b
2:
	jsr	r5,getc; iobuf
	cmpb	r0,$':
	beq	2f
	movb	r0,(r3)+
	br	2b
2:
	movb	$'\n,(r3)+
	cmp	r3,$euidbuf
	bhis	3f
2:
	jsr	r5,getc; iobuf
	cmpb	r0,$'\n
	bne	2b
	br	3b
3:
	mov	r3,euids
	sys	close
1:
	cmp	r2,r1
	bhi	1f
	mov	(r2)+,r3
	sub	sortoff,r3
	jsr	r5,pentry
	mov	$8.,-(sp)
2:
	movb	(r3)+,r0
	beq	2f
	jsr	r5,putc; obuf
	dec	(sp)
	bne	2b
2:
	tst	(sp)+
	jsr	r5,pstring; nl
	br	1b
1:
	cmp	ocount,$1
	ble	1f
	tst	isadir
	beq	1f
	jsr	r5,pstring; nl
1:
	rts	pc

pentry:
	mov	r2,-(sp)
	tstb	longf
	bne	listl
	tstb	longf+1
	bne	2f
	mov	(sp)+,r2
	rts	r5
2:
	mov	12.(r3),r0
	jsr	r5,calcb
	jsr	r5,decimal; 3
	jsr	r5,pstring; space
	mov	(sp)+,r2
	rts	r5
pstring:
	mov	r5,-(sp)
	mov	(r5),r5
1:
	movb	(r5)+,r0
	beq	1f
	jsr	r5,putc; obuf
	br	1b
1:
	mov	(sp)+,r5
	tst	(r5)+
	rts	r5

questf:
	mov	r4,0f
	jsr	r5,pstring; 0:..
	mov	r5,0f
	jsr	r5,pstring; 0:..
1:
	tstb	(r5)+
	bne	1b
	inc	r5
	bic	$1,r5
	rts	r5
listl:
	mov	18.(r3),r0	/ inode
	jsr	r5,decimal; 4
	jsr	r5,pstring; space
	mov	r3,r4
	add	$8.,r4		/ get to flags
	bit	$10000,(r4)  /large
	beq	2f
	jsr	r5,mode; 'l
	br	3f
2:
	jsr	r5,mode; 's
3:
	bit	$40000,(r4) /directory
	beq	2f
	jsr	r5,mode; 'd
	br	3f
2:
	bit	$40,(r4)  /set uid
	beq	2f
	jsr	r5,mode; 'u
	br	3f
2:
	bit	$20,(r4)   /executable
	beq	2f
	jsr	r5,mode; 'x
	br	3f
2:
	jsr	r5,mode; '-
3:
	bit	$10,(r4)  /read owner
	beq	2f
	jsr	r5,mode; 'r
	br	3f
2:
	jsr	r5, mode; '-
3:
	bit	$4,(r4)  /write owner
	beq	2f
	jsr	r5,mode; 'w
	br	3f
2:
	jsr	r5,mode; '-
3:
	bit	$2,(r4)  /read non-owner
	beq	2f
	jsr	r5,mode; 'r
	br	3f
2:
	jsr	r5,mode; '-
3:
	bit	$1,(r4)+  /write non-owner
	beq	2f
	jsr	r5,mode; 'w
	br	3f
2:
	jsr	r5,mode; '-
3:
	jsr	r5,pstring; space
	movb	(r4)+,r0
	jsr	r5,decimal; 2
	movb	(r4)+,r2
	jsr	pc,puid
	mov	(r4)+,r0
	jsr	r5,decimal; 5
	jsr	r5,pstring; space
	mov	r1,-(sp)
	mov	(r4)+,r0
	mov	(r4)+,r1
	sub	$16.,sp
	mov	sp,r2
	jsr	pc,ctime
	mov	sp,r2
	mov	$15.,-(sp)
1:
	movb	(r2)+,r0
	jsr	r5,putc; obuf
	dec	(sp)
	bne	1b
	add	$18.,sp
	mov	(sp)+,r1
	jsr	r5,pstring; space
	mov	(sp)+,r2
	rts	r5

puid:
	mov	r1,-(sp)
	mov	$uidbuf,r1
1:
	cmp	r1,euids
	bhis	1f
	mov	r1,0f
2:
	tstb	(r1)+
	beq	3f
	cmpb	-1(r1),$':
	bne	2b
	clrb	-1(r1)
3:
	clr	-(sp)
3:
	movb	(r1)+,r0
	sub	$'0,r0
	cmp	r0,$9.
	bhi	3f
	mov	r1,-(sp)
	mov	2(sp),r1
	mpy	$10.,r1
	add	r0,r1
	mov	r1,2(sp)
	mov	(sp)+,r1
	br	3b
3:
	mov	(sp)+,r0
	cmp	r0,r2
	bne	1b
	jsr	r5,pstring; space
	jsr	r5,pstring; 0:..
	mov	0b,r1
	mov	$6,-(sp)
3:
	tstb	(r1)+
	beq	3f
	dec	(sp)
	br	3b
3:
	jsr	r5,pstring; space
	dec	(sp)
	bgt	3b
	tst	(sp)+
	mov	(sp)+,r1
	rts	pc
1:
	jsr	r5,pstring; space
	mov	r2,r0
	jsr	r5,decimal; 3
	jsr	r5,pstring; space3
	mov	(sp)+,r1
	rts	pc

mode:
	mov	(r5)+,r0
	jsr	r5,putc; obuf
	rts	r5


gstat:
	mov	r1,-(sp)
	add	$512.,r1
	cmp	r1,0f
	blo	1f
	mov	r1,0f
	sys	break; 0: end+512.
1:
	mov	(sp)+,r1
	tst	longf
	bne	2f
	tst	sortoff
	beq	1f
2:
	sys	stat; filnam; statb
	bec	2f
	mov	r4,-(sp)
	mov	$filnam,r4
	jsr	r5,questf; < unstatable\n\0>; .even
	mov	(sp)+,r4
	add	$12.,r1
	rts	r5
2:
	mov	$statb+2,r0
	mov	(r0)+,(r1)+	/flags
	mov	(r0)+,(r1)+	/nlinks, uid
	mov	r0,-(sp)
	mov	(r0),r0
	jsr	r5,calcb
	add	r0,tblocks
	mov	(sp)+,r0
	mov	(r0)+,(r1)+	/size
	add	$20.,r0		/dska, ctim
	mov	(r0)+,(r1)+	/mtim
	mov	(r0)+,(r1)+
	mov	statb,(r1)+	/inode
	rts	r5
1:
	add	$12.,r1
	rts	r5

decimal:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	$6,r2
	mov	$numbuf+6,r3
1:
	mov	r0,r1
	clr	r0
	dvd	$10.,r0
	add	$'0,r1
	movb	r1,-(r3)
	sob	r2,1b
1:
	cmp	r3,$numbuf+5
	beq	1f
	cmpb	(r3),$'0
	bne	1f
	movb	$' ,(r3)+
	br	1b
1:
	mov	$numbuf+6,r1
	sub	(r5),r1
	mov	(r5)+,-(sp)
1:
	movb	(r1)+,r0
	jsr	r5,putc; obuf
	dec	(sp)
	bne	1b
	tst	(sp)+
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5

calcb:
	add	$511.,r0
	clrb	r0
	swab	r0
	asr	r0
	cmp	r0,$8
	blo	1f
	inc	r0
1:
1:
	rts	r5


dotp:	dot
euids:	uidbuf

dot:	<.\0>
nl:	<\n\0>
totmes:	<total \0>
space3:	<  >
space:	< \0>
passwd:	</etc/passwd\0>
colon:	<:\n\0>
	.even

.bss

count:	.=.+2
ocount:	.=.+2
longf:	.=.+2
sortoff: .=.+2
allflg:	.=.+2
dirflg:	.=.+2
isadir:	.=.+2
filnam:	.=.+32.
statb:	.=.+34.
dbuf:	.=.+518.
obuf:	.=.+518.
numbuf:	.=.+6
tblocks: .=.+2
uidbuf:	.=.+1024.
euidbuf:
iobuf:	.=.+518.

(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5

calcb:
	add	$511.,r0
	clrb	r0
	swab	r0
	asr	r0
	cmp	r0,$8
	blo	1f
	inc	r0
1:
1:
	rts	r5


dotp:	dot
euids:	uidbuf

dot:	<.\0>
nl:	<\n\0>
totmes:	<total \0>
space3:	<  >
space:	< \0>
passwd:	</etc/passwd\0>
colon:	<:\n\0>
	.even

.bss

count:	.=.+2
ocount:	.=.+2
longf:	.=.+2
sortoff: .=.+2
allflg:	.=.+2
dirflg:	.=.+2
isadir:	.=.+2
filnam:	.=.+32.
statb:	.=.+34.
dbuf:	.=.+518.
obuf:	.=.+518.
numbuf:	.=.+6
tblocks: .=.+2
uidbuf:	.=.+1024.
euidbuf:
iobuf:	.=.+518.