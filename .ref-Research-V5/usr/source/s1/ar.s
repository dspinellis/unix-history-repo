/ ar -- archive/library

	mov	(sp)+,r0
	sub	$2,r0
	ble	userr
	tst	(sp)+
	mov	(sp)+,r1
	clr	r2
1:
	tstb	(r1)
	beq	1f
	cmpb	(r1),$'v
	bne	2f
	inc	r1
	incb	vflg
	br	1b
2:
	tst	r2
	bne	userr
	movb	(r1)+,r2
	br	1b
1:
	tst	r2
	beq	userr
	mov	$arglst,r1
1:
	mov	(sp)+,(r1)+
	dec	r0
	bgt	1b
	clr	(r1)+
	mov	$swlst,r1
1:
	cmp	r2,(r1)+
	beq	1f
	tst	(r1)+
	bne	1b
	br	userr
1:
	jmp	*(r1)

swlst:
	'r; comr
	'u; comu
	'd; comd
	'x; comx
	't; comt
	 0; 0

userr:
	jsr	r5,diag
		<bad usage\n\0>
	.even

putc:
	movb	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5

print:
	movb	(r1)+,r0
	beq	1f
	jsr	r5,putc
	br	print
1:
	rts	r5

diag:
	mov	r5,r1
	jsr	r5,print
	tst	tfo
	beq	1f
	sys	unlink; tfil
1:
	sys	exit

getaf:
	mov	arglst,0f
	sys	open; 0:..; 0
	bes	1f
	mov	r0,afi
	sys	read; buf; 2
	cmp	buf,magic
	bne	magerr
	tst	(r5)+
1:
	rts	r5

magerr:
	mov	arglst,r1
	jsr	r5,print
	jsr	r5,diag
		< -- not in archive format\n\0>
	.even

mktmp:
	sys	stat; tfil; buf
	bes	1f
	incb	tfil+8
	cmpb	tfil+8,$'z
	blo	mktmp
	br	tferr
1:
	sys	signal; 2; 1
	ror	r0
	bcs	1f
	sys	signal; 2; done
1:
	sys	creat; tfil; 600
	bes	tferr
	mov	r0,tfo
	sys	open; tfil; 0
	bes	tferr
	mov	r0,tfi
	rts	r5

tferr:
	jsr	r5,diag
		<cannot open temp file\n\0>
	.even

getdir:
	mov	afi,r0
	sys	read; dir; 16.
	cmp	r0,$16.
	bne	1f
	jsr	r5,mvname
	tst	(r5)+
1:
	rts	r5

mvname:
	mov	name,rname
	mov	name+2,rname+2
	mov	name+4,rname+4
	mov	name+6,rname+6
	rts	r5

skip:
	mov	size,r0
	inc	r0
	bic	$1,r0
	mov	r0,0f
	mov	afi,r0
	sys	seek; 0:..; 1
	rts	r5

trim:
	mov	r0,r2
1:
	tstb	(r0)
	beq	1f
	cmpb	(r0)+,$'/
	beq	trim
	br	1b
1:
	rts	r5

match:
	mov	$arglst+2,r1
1:
	mov	(r1)+,r0
	beq	1f
	cmp	r0,$-1
	beq	1b
	jsr	r5,trim
	mov	$name,r0
2:
	cmp	r0,$name+8.
	beq	2f
	cmpb	(r0),(r2)+
	bne	1b
	tstb	(r0)+
	bne	2b
2:
	cmp	(r5)+,-(r1)
1:
	rts	r5

mvfil:
	mov	(r1),9f
	mov	(r1),0f
	sys	stat; 0:..; buf
	bes	operr
	sys	open; 9:..; 0
	bes	operr
	mov	r0,fio
	mov	(r1),r0
	mov	$-1,(r1)
	jsr	r5,trim
	mov	$name,r0
1:
	cmp	r0,$name+8.
	beq	1f
	movb	(r2)+,(r0)+
	bne	1b
1:
	mov	buf+32.,mtim
	mov	buf+34.,mtim+2
	movb	buf+7.,ouid
	movb	buf+4.,mode
	mov	buf+10.,size
	mov	tfo,r0
	sys	write; dir; 16.
	mov	size,r2
1:
	mov	fio,r0
	sys	read; buf; 512.
	sub	r0,r2
	mov	r0,0f
	beq	1f
	mov	tfo,r0
	sys	write; buf; 0:..
	br	1b
1:
	tst	r2
	bne	phserr
	bit	$1,size
	beq	1f
	mov	tfo,r0
	sys	seek; 1; 1
1:
	mov	fio,r0
	sys	close
	jsr	r5,mvname
	rts	r5

operr:
	mov	9b,r1
	jsr	r5,print
	jsr	r5,diag
		< -- cannot open\n\0>
	.even

phserr:
	mov	9b,r1
	jsr	r5,print
	jsr	r5,diag
		< -- phase error\n\0>
	.even

copyfl:
	mov	tfo,r0
	sys	write; dir; 16.
	mov	size,r1
	mov	$rname,9b
1:
	mov	r1,0f
	beq	1f
	cmp	r1,$512.
	blo	2f
	mov	$512.,0f
2:
	mov	afi,r0
	sys	read; buf; 0:..
	sub	r0,r1
	mov	r0,0f
	beq	phserr
	mov	tfo,r0
	sys	write; buf; 0:..
	br	1b
1:
	bit	$1,size
	beq	1f
	mov	afi,r0
	sys	seek; 1; 1
	mov	tfo,r0
	sys	seek; 1; 1
1:
	rts	r5

xtract:
/	movb	mode,0f
	sys	creat; rname; 0:666
	bes	noxerr
	mov	r0,fio
	mov	size,r1
	mov	$rname,9b
1:
	mov	r1,0f
	beq	1f
	cmp	r1,$512.
	blo	2f
	mov	$512.,0f
2:
	mov	afi,r0
	sys	read; buf; 0:..
	sub	r0,r1
	mov	r0,0f
	beq	phserr
	mov	fio,r0
	sys	write; buf; 0:..
	br	1b
1:
	mov	fio,r0
	sys	close
	bit	$1,size
	beq	1f
	mov	afi,r0
	sys	seek; 1; 1
1:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	mtim+2,r1
	mov	mtim,r0
/	sys	mdate
	mov	(sp)+,r1
	mov	(sp)+,r1
	rts	r5

noxerr:
	mov	$rname,r1
	jsr	r5,print
	jsr	r5,diag
		< -- cannot create\n\0>
	.even

table:
	mov	$rname,r1
	jsr	r5,print
	mov	$'\n,r0
	jsr	r5,putc
	rts	r5

mesg:
	mov	r1,-(sp)
	mov	(r5)+,r0
	tstb	vflg
	beq	1f
	jsr	r5,putc
	mov	$' ,r0
	jsr	r5,putc
	mov	$rname,r1
	jsr	r5,print
	mov	$'\n,r0
	jsr	r5,putc
1:
	mov	(sp)+,r1
	rts	r5

oldnew:
	sys	stat; rname; buf
	bes	1f
	cmp	buf+32.,mtim
	blo	1f
	bhi	2f
	cmp	buf+34.,mtim+2
	blos	1f
2:
	tst	(r5)+
	mov	$rname,tname
	mov	$tname,r1
1:
	rts	r5

comr:
	jsr	r5,mktmp
	jsr	r5,getaf
		br copfl
1:
	jsr	r5,getdir
		br copfl
	jsr	r5,match
		br 2f
	jsr	r5,mesg; 'r
	jsr	r5,skip
	jsr	r5,mvfil
	br	1b
2:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b

comu:
	jsr	r5,mktmp
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br copfl
	tst	arglst+2
	beq	2f
	jsr	r5,match
		br 3f
	mov	$-1,(r1)
2:
	jsr	r5,oldnew
		br 3f
	jsr	r5,mesg; 'r
	jsr	r5,skip
	jsr	r5,mvfil
	br	1b
3:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b

comd:
	jsr	r5,mktmp
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	jsr	r5,match
		br 2f
	mov	$-1,(r1)
	jsr	r5,skip
	jsr	r5,mesg; 'd
	br	1b
2:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b
1:
	jsr	r5,nfound
	br	copfl

noaf:
	jsr	r5,diag
		<no archive file\n\0>
	.even

crterr:
	jsr	r5,diag
		<cannot create archive file\n\0>
	.even

copfl:
	mov	$arglst,r1
	mov	(r1)+,0f
1:
	tst	(r1)+
	beq	1f
	cmp	-2(r1),$-1
	beq	1b
	tst	-(r1)
	jsr	r5,mvfil
	jsr	r5,mesg; 'a
	br	1b
1:
	sys	signal; 2; 1 / no interrupts during copy back
	sys	creat; 0:..; 666
	bes	crterr
	mov	r0,afo
	sys	write; magic; 2
1:
	mov	tfi,r0
	sys	read; buf; 512.
	mov	r0,0f
	beq	done
	mov	afo,r0
	sys	write; buf; 0:..
	br	1b

done:
	jsr	r5,diag
		<\0>
	.even

comx:
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	tst	arglst+2
	beq	3f
	jsr	r5,match
		br 2f
	mov	$-1,(r1)
3:
	jsr	r5,xtract
	jsr	r5,mesg; 'x
	br	1b
2:
	jsr	r5,skip
	br	1b
1:
	jsr	r5,nfound
	br	done

comt:
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	tst	arglst+2
	beq	2f
	jsr	r5,match
		br 3f
	mov	$-1,(r1)
2:
	jsr	r5,table
3:
	jsr	r5,skip
	br	1b
1:
	jsr	r5,nfound
	br	done

nfound:
	mov	$arglst+2,r2
1:
	mov	(r2)+,r1
	beq	1f
	cmp	r1,$-1
	beq	1b
	mov	$-1,-(r2)
	jsr	r5,print
	mov	$notfnd,r1
	jsr	r5,print
	br	1b
1:
	rts	r5

notfnd:
	< -- not found\n\0>
	.even

tfil:	</tmp/vtma\0>
	.even
magic:	-147.

	.bss

afi:	.=.+2
afo:	.=.+2
tfi:	.=.+2
tfo:	.=.+2
fio:	.=.+2
rname:	.=.+9.
ch:	.=.+1
vflg:	.=.+1
	.even
tname:	.=.+2
dir:
	name: .=.+8.
	mtim: .=.+4
	ouid: .=.+1
	mode: .=.+1
	size: .=.+2
arglst:	.=.+200.
buf:	.=.+512.

