/ a21 -- pdp-11 assembler pass 2 

main:
	sys	intr; aexit
	jmp	start

/ set up sizes and origins

go:
	jsr	pc,getw
	mov	r4,txtsiz
	mov	r4,r1
	jsr	pc,getw
	mov	r4,datsiz
	mov	r4,r2
	jsr	pc,getw
	mov	r4,bsssiz
	mov	r1,r3
	mov	r3,datbase	/ txtsiz
	mov	r3,savdot+2
	add	r2,r3
	mov	r3,bssbase	/ txtsiz+datsiz
	mov	r3,savdot+4
	asl	r3
	add	$20,r3
	mov	r3,symseek	/ 2*txtsiz+2*datsiz+20
	sub	r2,r3
	mov	r3,drelseek	/ 2*txtsiz+datsiz
	sub	r1,r3
	mov	r3,trelseek	/ txtsiz+datsiz+20
	sub	r2,r3
	mov	r3,datseek	/ txtsiz+20

/ read in symbol table itself

	mov	$usymtab,r1
1:
	jsr	pc,getw
	bvs	1f
	add	$14,symsiz		/ count symbols
	jsr	pc,getw
	jsr	pc,getw
	bic	$!377,r4
	bne	2f
	mov	defund,r4		/ (perhaps) globalize undef.
2:
	mov	r4,(r1)+
	jsr	r5,doreloc
	jsr	pc,getw
	add	r3,r4
	mov	r4,(r1)+
	jsr	pc,setbrk
	br	1b
1:

/ read in f-b definitions

	mov	r1,fbbufp
	movb	fbfil,fin
	clr	ibufc
1:
	jsr	pc,getw
	bvs	1f
	mov	r4,(r1)+
	swab	r4
	jsr	r5,doreloc
	jsr	pc,getw
	add	r3,r4
	mov	r4,(r1)+
	jsr	pc,setbrk
	br	1b
1:
	mov	$-1,(r1)+

/ set up input text file; initialize f-b table

	mov	txtfil,fin
	clr	ibufc
	clr	r4
1:
	jsr	pc,fbadv
	tstb	(r4)+
	cmp	r4,$10.
	blt	1b

/ go

	clr	r0
	jsr	r5,oset; txtp
	mov	trelseek,r0
	jsr	r5,oset; relp
	mov	$8.,r2
	mov	$txtmagic,r1
1:
	mov	(r1)+,r0
	jsr	r5,putw; txtp
	dec	r2
	bne	1b
	jsr	pc,assem

/polish off text and relocation

	jsr	r5,flush; txtp
	jsr	r5,flush; relp

/ append full symbol table

	mov	symf,r0
	mov	r0,fin
	clr	ibufc
	sys	seek; 6; 0
	mov	symseek,r0
	jsr	r5,oset; txtp
	mov	$usymtab,r1
1:
	jsr	pc,getw
	bvs	1f
	jsr	pc,convs
	jsr	pc,getw
	jsr	pc,convs
	jsr	pc,getw
	mov	r4,r0
	als	$-10.,r0
	bic	$!77,r0
	movb	chartab(r0),r0
	jsr	r5,putw; txtp
	mov	(r1)+,r0
	jsr	r5,putw; txtp
	jsr	pc,getw
	mov	(r1)+,r0
	jsr	r5,putw; txtp
	br	1b
1:
	jsr	r5,flush; txtp

aexit:
	mov	a.tmp1,0f
	sys	unlink; 0:..
	mov	a.tmp2,0f
	sys	unlink; 0:..
	mov	a.tmp3,0f
	sys	unlink; 0:..
	sys	chmod; a.out; outmod: 37
	sys	exit

filerr:
	mov	*(r5),r5
1:
	movb	(r5)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	mov	$1,r0
	sys	write; qnl; 2
	br	aexit

doreloc:
	clr	r3
	bic	$!37,r4
	cmp	r4,$5
	bhis	1f
	cmp	r4,$3
	blo	1f
	beq	2f
	mov	bssbase,r3
	br	1f
2:
	mov	datbase,r3
1:
	rts	r5

setbrk:
	mov	r1,-(sp)
	add	$20,r1
	cmp	r1,0f
	blo	1f
	add	$512.,0f
	sys	break; 0: end
1:
	mov	(sp)+,r1
	rts	pc


	mov	a.tmp1,0f
	sys	unlink; 0:..
	mov	a.tmp2,0f
	sys	unlink; 0:..
	mov	a.tmp3,0f
	sys	unlink; 0:..
	sys	chmod; a.out; outmod: 37
	sys	exit

filerr:
	mov	*(r5),r5
1:
	movb	(r5)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	mov	$1,r0
	sys	write; 