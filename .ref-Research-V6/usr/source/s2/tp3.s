/ tap3 -- dec-tape lod/dmp

gettape:
	mov	$dir,r1
	clr	-(sp)
1:
	tst	(r1)
	beq	2f
	jsr	r5,decode; name
	cmp	rnarg,$2
	ble	4f
	mov	$name,r2
	mov	*parg,r3
3:
	tstb	(r3)
	beq	3f
	cmpb	(r2)+,(r3)+
	beq	3b
	br	2f
3:
	tstb	(r2)
	beq	4f
	cmpb	(r2),$'/
	bne	2f
4:
	mov	r1,-(sp)
	jsr	pc,*(r5)
	mov	(sp)+,r1
	inc	(sp)
2:
	add	$dirsiz,r1
	cmp	r1,edir
	blo	1b
	tst	(sp)+
	bne	2f
	cmp	rnarg,$2
	ble	2f
	mov	*parg,r1
	jsr	pc,pstr
	jsr	r5,mesg
		< not found\n\0>; .even
2:
	dec	narg
	add	$2,parg
	cmp	narg,$2
	bgt	gettape
	tst	(r5)+
	rts	r5

delete:
	jsr	r5,verify; 'd
		rts pc
	jsr	pc,clrent
	rts	pc

numb:
	mov	r1,-(sp)
	mov	r0,-(sp)
	clr	r0
	br	1f

numbx:
	mov	r1,-(sp)
	mov	r0,-(sp)
	movb	size0(r1),r0
1:
	mov	$catlb,r2
1:
	mov	$"  ,(r2)+
	cmp	r2,$catlb+12.
	blo	1b
	cmp	(r5),$2
	bne	1f
	mov	$"00,-2(r2)
1:
	mov	(sp)+,r1
	jsr	pc,numb2
	mov	(r5)+,r0
	sub	r0,r2
	mov	r2,0f
	mov	r0,0f+2
	mov	$1,r0
	sys	0; 9f
.data
9:
	sys	write; 0:..; ..
.text
	mov	(sp)+,r1
	rts	r5

numb1:
	clr	r0
numb2:
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	pc,numb1
1:
	mov	(sp)+,r0
	add	$'0,r0
	movb	r0,(r2)+
	rts	pc

update:
	jsr	pc,bitmap
	mov	$dir,r1
1:
	tst	(r1)
	beq	2f
	bit	$100000,mode(r1)
	beq	2f
	tstb	size0(r1)
	bne	9f
	tst	size1(r1)
	beq	2f
9:
	mov	ndentd8,-(sp)
	inc	(sp)
	movb	size0(r1),r2
	mov	size1(r1),r3
	add	$511.,r3
	adc	r2
	ashc	$-9,r2
	mov	r3,size
3:
	mov	(sp),r2
	mov	size,r3
4:
	jsr	pc,bitcalc
	inc	r2
	bitb	(sp)+,map(r0)
	bne	4f
	sob	r3,4b
	mov	(sp)+,tapea(r1)
	jsr	pc,setmap
	br	2f
4:
	inc	(sp)
	br	3b
2:
	add	$dirsiz,r1
	cmp	r1,edir
	blo	1b
	jsr	pc,wrdir

update1:
	mov	$dir,r1
	clr	-(sp)
	mov	$-1,-(sp)
1:
	tst	(r1)
	beq	2f
	bit	$100000,mode(r1)
	beq	2f
	cmp	tapea(r1),(sp)
	bhis	2f
	mov	tapea(r1),(sp)
	mov	r1,2(sp)
2:
	add	$dirsiz,r1
	cmp	r1,edir
	blo	1b
	tst	(sp)+
	mov	(sp)+,r1
	bne	1f
	rts	pc
1:
	bic	$100000,mode(r1)
	movb	size0(r1),mss
	mov	size1(r1),r2
	bne	4f
	tst	mss
	beq	update1
4:
	jsr	r5,decode; name
	mov	tapea(r1),r0
	jsr	pc,wseek
	clr	r3
	sys	open; name; 0
	bes	phserr
	mov	r0,r3
3:
	tst	mss
	bne	4f
	cmp	r2,$512.
	blo	3f
4:
	mov	r3,r0
	sys	read; tapeb; 512.
	bes	phserr
	cmp	r0,$512.
	bne	phserr
	jsr	pc,twrite
	sub	$512.,r2
	sbc	mss
	br	3b
3:
	mov	r2,0f
	beq	3f
	mov	r3,r0
	sys	0; 9f
.data
9:
	sys	read; tapeb; 0:..
.text
	bes	phserr
	cmp	r0,0b
	bne	phserr
	jsr	pc,twrite
3:
	mov	r3,r0
	sys	read; tapeb; 512.
	bes	phserr
	tst	r0
	bne	phserr
	mov	r3,r0
	sys	close
2:
	jmp	update1

phserr:
	mov	r1,-(sp)
	mov	$name,r1
	jsr	pc,pstr
	jsr	r5,mesg
		< -- Phase error\n\0>; .even
	mov	(sp)+,r1
	clr	time0(r1) / time
	beq	2b
	sys	close
	br	2b

bitmap:
	mov	$map,r0
1:
	clr	(r0)+
	cmp	r0,$emap
	blo	1b
	mov	$dir,r1
1:
	tst	(r1)
	beq	2f
	bit	$100000,mode(r1)
	bne	2f
	tst	size1(r1)
	bne	3f
	tstb	size0(r1)
	beq	2f
3:
	jsr	pc,setmap
2:
	add	$dirsiz,r1
	cmp	r1,edir
	blo	1b
	rts	pc

setmap:
	movb	size0(r1),r2
	mov	size1(r1),r3
	add	$511.,r3
	adc	r2
	ashc	$-9.,r2
	mov	tapea(r1),r2
1:
	jsr	pc,bitcalc
	bitb	(sp),map(r0)
	bne	maperr
	bisb	(sp)+,map(r0)
	inc	r2
	sob	r3,1b
	rts	pc

bitcalc:
	mov	(sp),-(sp)
	cmp	r2,tapsiz
	bhis	maperr
	mov	r2,r0
	bic	$!7,r0
	mov	r0,-(sp)
	mov	$1,r0
	als	(sp)+,r0
	mov	r0,2(sp)
	mov	r2,r0
	ash	$-3,r0
	bic	$160000,r0
	rts	pc

maperr:
	jsr	r5,mesg
		<Tape overflow\n\0>; .even
	jmp	done

usage:
	jsr	pc,bitmap
	mov	$dir,r2
1:
	tst	(r2)
	beq	2f
	inc	nentr
2:
	add	$dirsiz,r2
	cmp	r2,edir
	blo	1b
	mov	ndentd8,r2
	inc	r2
	mov	tapsiz,r3
	dec	r3
	sub	ndentd8,r3
1:
	jsr	pc,bitcalc
	bitb	(sp)+,map(r0)
	beq	2f
	inc	nused
	mov	r2,lused
	br	3f
2:
	inc	nfree
	tstb	flm
	bne	1f
3:
	inc	r2
	sob	r3,1b
1:
	mov	nentr,r0
	jsr	r5,numb; 4
	jsr	r5,mesg
		< entries\n\0>; .even
	mov	nused,r0
	jsr	r5,numb; 4
	jsr	r5,mesg
		< used\n\0>; .even
	tstb	flm
	bne	1f
	mov	nfree,r0
	jsr	r5,numb; 4
	jsr	r5,mesg
		< free\n\0>; .even
1:
	mov	lused,r0
	jsr	r5,numb; 4
	jsr	r5,mesg
		< last\n\0>; .even
	rts	pc

taboc:
	tstb	flv
	beq	4f
	mov	mode(r1),r0
	mov	r0,-(sp)
	ash	$-6,r0
	bit	$40,r0
	jsr	pc,pmod
	mov	(sp),r0
	ash	$-3,r0
	bit	$200,r0
	jsr	pc,pmod
	mov	(sp)+,r0
	bit	$1000,r0
	jsr	pc,pmod
	clr	r0
	bisb	uid(r1),r0
	jsr	r5,numb; 4
	clr	r0
	bisb	gid(r1),r0
	jsr	r5,numb; 4
	mov	tapea(r1),r0
	jsr	r5,numb; 5
	mov	size1(r1),r0
	jsr	r5,numbx; 9.
	mov	r1,-(sp)
	add	$time0,(sp)
	jsr	pc,_localtime
	mov	r0,(sp)
	mov	10.(r0),r0
	jsr	r5,numb; 3
	mov	$'/,r0
	jsr	pc,putc
	mov	(sp),r0
	mov	8.(r0),r0
	inc	r0
	jsr	r5,numb; 2
	mov	$'/,r0
	jsr	pc,putc
	mov	(sp),r0
	mov	6(r0),r0
	jsr	r5,numb; 2
	mov	(sp),r0
	mov	4(r0),r0
	jsr	r5,numb; 3
	mov	$':,r0
	jsr	pc,putc
	mov	(sp)+,r0
	mov	2(r0),r0
	jsr	r5,numb; 2
	mov	$' ,r0
	jsr	pc,putc
4:
	mov	$name,r1
	jsr	pc,pstr
	jsr	r5,mesg
		<\n\0>
	rts	pc

pmod:
	beq	1f
	mov	$'s,-(sp)
	br	2f
1:
	bit	$1,r0
	beq	1f
	mov	$'x,-(sp)
	br	2f
1:
	mov	$'-,-(sp)
2:
	bit	$2,r0
	beq	1f
	mov	$'w,-(sp)
	br	2f
1:
	mov	$'-,-(sp)
2:
	bit	$4,r0
	beq	1f
	mov	$'r,r0
	br	2f
1:
	mov	$'-,r0
2:
	jsr	pc,putc
	mov	(sp)+,r0
	jsr	pc,putc
	mov	(sp)+,r0
	jsr	pc,putc
	rts	pc

xtract:
	movb	size0(r1),mss
	bne	2f
	tst	size1(r1)
	beq	1f
2:
	jsr	r5,verify; 'x
		rts pc
	mov	size1(r1),r3
	mov	tapea(r1),r0
	jsr	pc,rseek
	sys	unlink; name
	mov	mode(r1),0f
	sys	0; 9f
.data
9:
	sys	creat; name; 0:..
.text
	bes	crterr
	mov	r0,r2
2:
	tst	mss
	bne	3f
	cmp	r3,$512.
	blo	2f
3:
	jsr	pc,tread
	mov	r2,r0
	sys	write; tapeb; 512.
	bes	crterr1
	cmp	r0,$512.
	bne	crterr1
	sub	r0,r3
	sbc	mss
	br	2b
2:
	mov	r3,0f
	beq	2f
	jsr	pc,tread
	mov	r2,r0
	sys	0; 9f
.data
9:
	sys	write; tapeb; 0:..
.text
	bes	crterr1
	cmp	r0,0b
	bne	crterr1
2:
	mov	r2,r0
	sys	close
	movb	gid(r1),0f+1
	movb	uid(r1),0f
	sys	0; 9f
.data
9:
	sys	chown; name; 0:..
.text
	mov	time0(r1),r0
	mov	r1,-(sp)
	mov	time1(r1),r1
/	sys	0; 9f
.data
9:
	sys	smdate; name
.text
	mov	(sp)+,r1
1:
	rts	pc

crterr1:
	clr	r0
	mov	r1,-(sp)
	clr	r1
/	sys	smdate; name
	mov	(sp)+,r1
	mov	r2,r0
	sys	close

crterr:
	mov	$name,r1
	jsr	pc,pstr
	jsr	r5,mesg
		< -- create error\n\0>; .even
	rts	pc
