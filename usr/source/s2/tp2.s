/ tap2 -- dec-tape lod/dmp

pstr:
	movb	(r1)+,r0
	beq	1f
	jsr	pc,putc
	br	pstr
1:
	rts	pc

mesg:
	movb	(r5)+,r0
	beq	1f
	jsr	pc,putc
	br	mesg
1:
	inc	r5
	bic	$1,r5
	rts	r5

putc:
	movb	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	pc

getc:
	clr	r0
	sys	read; ch; 1
	movb	ch,r0
	rts	pc

clrdir:
	mov	$dir,r1
	mov	ndirent,r2
1:
	jsr	pc,clrent
	sob	r2,1b
	rts	pc

clrent:
	mov	r1,-(sp)
	add	$dirsiz,(sp)
1:
	clr	(r1)+
	cmp	r1,(sp)
	blo	1b
	tst	(sp)+
	rts	pc

rddir:
	clr	sum
	jsr	pc,clrdir
	clr	r0
	jsr	pc,rseek
	jsr	pc,tread
	mov	tapeb+510.,r0
	beq	1f
	tstb	flm
	beq	1f
	mov	r0,ndirent
1:
	mov	$dir,r1
	mov	ndirent,r2
1:
	bit	$7,r2
	bne	2f
	jsr	pc,tread
	mov	$tapeb,r3
2:
	mov	r1,-(sp)
	mov	r3,-(sp)
	mov	$32.,r0
	clr	-(sp)
2:
	add	(r3)+,(sp)
	sob	r0,2b
	bis	(sp),sum
	tst	(sp)+
	bne	2f
	mov	(sp),r3
	tst	(r3)
	beq	2f
	mov	r3,0f
	jsr	pc,9f
.data
9:
	jsr	r5,encode; 0:..
	rts	pc
.text
	add	$32.,r3
	mov	r1,-(sp)
	add	$dirsiz,(sp)
	tst	(r1)+		/ namep
9:
	mov	(r3)+,(r1)+
	cmp	r1,(sp)
	blo	9b
	tst	(sp)+
2:
	mov	(sp)+,r3
	add	$64.,r3
	mov	(sp)+,r1
	bic	$100000,mode(r1)
	add	$dirsiz,r1
	sob	r2,1b
	tst	sum
	beq	1f
	jsr	r5,mesg
		<Directory checksum\n\0>; .even
	tstb	fli
	bne	1f
	jmp	done
1:
	jsr	pc,bitmap
	rts	pc

wrdir:
	clr	r0
	jsr	pc,wseek
	tstb	flm
	bne	1f
	sys	open; tboot; 0
	bes	3f
	br	2f
1:
	sys	open; mboot; 0
	bes	3f
2:
	mov	r0,r1
	sys	read; tapeb; 512.
	mov	r1,r0
	sys	close
	mov	ndirent,tapeb+510.
3:
	jsr	pc,twrite
	mov	$dir,r1
	mov	ndirent,r2
1:
	bit	$7,r2
	bne	2f
	mov	$256.,r0
	mov	$tapeb,r3
3:
	clr	(r3)+
	sob	r0,3b
	mov	$tapeb,r3
2:
	mov	r3,-(sp)
	tst	(r1)
	beq	2f
	mov	r3,0f
	jsr	pc,9f
.data
9:
	jsr	r5,decode; 0:..
	rts	pc
.text
2:
	add	$32.,r3
	mov	r1,-(sp)
	add	$dirsiz,(sp)
	tst	(r1)+
9:
	mov	(r1)+,(r3)+
	cmp	r1,(sp)
	blo	9b
	tst	(sp)+
	mov	(sp)+,r3
	clr	-(sp)
	mov	$31.,r0
2:
	sub	(r3)+,(sp)
	sob	r0,2b
	mov	(sp)+,(r3)+
	dec	r2
	bit	$7,r2
	bne	1b
	jsr	pc,twrite
	tst	r2
	bne	1b
	rts	pc

tread:
	mov	fio,r0
	sys	read; tapeb; 512.
	bes	trderr
	cmp	r0,$512.
	bne	trderr
1:
	inc	rseeka
	rts	pc

trderr:
	jsr	r5,mesg
		<Tape read error\n\0>; .even
	tstb	fli
	beq	1f
	mov	$tapeb,r0
2:
	clr	(r0)+
	cmp	r0,$tapeb+512.
	blo	2b
	br	1b
1:
	jmp	done

twrite:
	mov	fio,r0
	sys	write; tapeb; 512.
	bes	twrerr
	cmp	r0,$512.
	bne	twrerr
	inc	wseeka
	rts	pc

twrerr:
	jsr	r5,mesg
		<Tape write error\n\0>; .even
	jmp	done

rseek:
	mov	r0,rseeka
	mov	r0,0f
	mov	fio,r0
	sys	0; 9f
.data
9:
	sys	seek; 0:..; 3
.text
	bes	seekerr
	rts	pc

wseek:
	mov	r0,-(sp)
	sub	wseeka,r0
	bge	1f
	neg	r0
1:
	cmp	r0,$25.			/ longest write seek
	ble	1f
	mov	(sp),0f
	beq	2f
	dec	0f
2:
	mov	fio,r0
	sys	0; 9f
.data
9:
	sys	seek; 0:..; 3
.text
	mov	fio,r0
	sys	read; wseeka; 1
1:
	mov	(sp),wseeka
	mov	(sp)+,0f
	mov	fio,r0
	sys	0; 9f
.data
9:
	sys	seek; 0:..; 3
.text
	bes	seekerr
	rts	pc

seekerr:
	jsr	r5,mesg
<Tape seek error\n\0>; .even
	jmp	done

verify:
	movb	(r5)+,0f
	inc	r5
	tstb	flw
	bne	1f
	tstb	flv
	beq	2f
1:
	jsr	pc,9f
.data
9:
	jsr	r5,mesg
		0:<x \0>; .even
	rts	pc
.text
	mov	r1,-(sp)
	mov	$name,r1
	jsr	pc,pstr
	mov	(sp)+,r1
	tstb	flw
	beq	1f
	jsr	r5,mesg
		< \0>
	jsr	pc,getc
	cmp	r0,$'x
	bne	3f
	jsr	pc,getc
	jmp	done
3:
	cmp	r0,$'\n
	beq	3f
	cmp	r0,$'y
	bne	4f
	jsr	pc,getc
	cmp	r0,$'\n
	beq	2f
4:
	jsr	pc,getc
	cmp	r0,$'\n
	bne	4b
	br	1b
1:
	jsr	r5,mesg
		<\n\0>
2:
	tst	(r5)+
3:
	rts	r5

getfiles:
	cmp	narg,$2
	bne	1f
	mov	$".\0,name
	jsr	pc,callout
1:
	cmp	narg,$2
	ble	1f
	dec	narg
	mov	*parg,r1
	add	$2,parg
	mov	$name,r2
2:
	movb	(r1)+,(r2)+
	bne	2b
	jsr	pc,callout
	br	1b
1:
	rts	pc

expand:
	sys	open; name; 0
	bes	fserr
	mov	r0,-(sp)
1:
	mov	(sp),r0
	sys	read; catlb; 16.
	bes	fserr
	tst	r0
	beq	1f
	tst	catlb
	beq	1b
	mov	$name,r0
	mov	$catlb+2,r1
	cmpb	(r1),$'.
	beq	1b
2:
	tstb	(r0)+
	bne	2b
	dec	r0
	mov	r0,-(sp)
	cmpb	-1(r0),$'/
	beq	2f
	movb	$'/,(r0)+
2:
	movb	(r1)+,(r0)+
	bne	2b
	jsr	pc,callout
	clrb	*(sp)+
	br	1b
1:
	mov	(sp)+,r0
	sys	close
	rts	pc

fserr:
	mov	$name,r1
	jsr	pc,pstr
	jsr	r5,mesg
		< -- Cannot open file\n\0>; .even
	jmp	done

callout:
	sys	stat; name; statb
	bes	fserr
	mov	statb+4,r0
	bic	$!60000,r0
	beq	1f
	cmp	r0,$40000
	beq	expand
	rts	pc
1:
	mov	$dir,r1
	clr	-(sp)
1:
	tst	(r1)
	bne	3f
	tst	(sp)
	bne	2f
	mov	r1,(sp)
2:
	add	$dirsiz,r1
	cmp	r1,edir
	blo	1b
	mov	(sp)+,r1
	bne	4f
	jsr	r5,mesg
		<Directory overflow\n\0>; .even
	jmp	done
4:
	jsr	r5,verify; 'a
		rts pc
	jsr	r5,encode; name
	br	2f
3:
	jsr	r5,decode; name1
	mov	$name,r2
	mov	$name1,r3
3:
	cmpb	(r2)+,(r3)
	bne	2b
	tstb	(r3)+
	bne	3b
	tst	(sp)+
	tstb	flu
	beq	3f
	cmp	time0(r1),statb+32.
	blo	3f
	bhi	1f
	cmp	time1(r1),statb+34.
	bhis	1f
3:
	jsr	r5,verify; 'r
		rts pc
2:
	mov	statb+4,mode(r1)
	bis	$100000,mode(r1)
	movb	statb+7,uid(r1)
	movb	statb+8,gid(r1)
	tstb	flf
	beq	2f
	clrb	statb+9.
	clr	statb+10.
2:
	movb	statb+9.,size0(r1)
	mov	statb+10.,size1(r1)
	mov	statb+32.,time0(r1)
	mov	statb+34.,time1(r1)
1:
	rts	pc
