/
/

/ a21 -- pdp-11 assembler pass 2 

indir	= 0

main:
	sys	signal; 2; 1
	ror	r0
	bcs	1f
	sys	signal; 2; aexit
1:
	jmp	start

/ set up sizes and origins

go:

/ read in symbol table

	mov	$usymtab,r1
1:
	jsr	pc,getw
	bvs	1f
	add	$14,symsiz		/ count symbols
	jsr	pc,getw
	jsr	pc,getw
	jsr	pc,getw
	jsr	pc,getw
	mov	r4,r0
	bic	$!37,r0
	cmp	r0,$2			/text
	blo	2f
	cmp	r0,$3			/data
	bhi	2f
	add	$31,r4			/mark "estimated"
	mov	r4,(r1)+
	jsr	pc,getw
	mov	r4,(r1)+
	br	3f
2:
	clr	(r1)+
	clr	(r1)+
	jsr	pc,getw
3:
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
	add	$31,r4			/ "estimated"
	mov	r4,(r1)+
	jsr	pc,getw
	mov	r4,(r1)+
	jsr	pc,setbrk
	br	1b
1:
	mov	r1,endtable
	mov	$100000,(r1)+

/ set up input text file; initialize f-b table

	jsr	pc,setup
/ do pass 1

	jsr	pc,assem

/ prepare for pass 2
	cmp	outmod,$777
	beq	1f
	jmp	aexit
1:
	clr	dot
	mov	$2,dotrel
	mov	$..,dotdot
	clr	brtabp
	movb	fin,r0
	sys	close
	jsr	r5,ofile; a.tmp1
	movb	r0,fin
	clr	ibufc
	jsr	pc,setup
	inc	passno
	inc	bsssiz
	bic	$1,bsssiz
	mov	txtsiz,r1
	inc	r1
	bic	$1,r1
	mov	r1,txtsiz
	mov	datsiz,r2
	inc	r2
	bic	$1,r2
	mov	r2,datsiz
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
	mov	$usymtab,r1
1:
	jsr	pc,doreloc
	add	$4,r1
	cmp	r1,endtable
	blo	1b
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
	sys	seek; 0; 0;
	clr	ibufc
	mov	symseek,r0
	jsr	r5,oset; txtp
	mov	$usymtab,r1
1:
	jsr	pc,getw
	bvs	1f
	mov	r4,r0
	jsr	r5,putw; txtp
	jsr	pc,getw
	mov	r4,r0
	jsr	r5,putw; txtp
	jsr	pc,getw
	mov	r4,r0
	jsr	r5,putw; txtp
	jsr	pc,getw
	mov	r4,r0
	jsr	r5,putw; txtp
	mov	(r1)+,r0
	jsr	r5,putw; txtp
	mov	(r1)+,r0
	jsr	r5,putw; txtp
	jsr	pc,getw
	jsr	pc,getw
	br	1b
1:
	jsr	r5,flush; txtp
	jmp	aexit

	.data
aexit:
	mov	a.tmp1,0f
	sys	unlink; 0:..
	mov	a.tmp2,0f
	sys	unlink; 0:..
	mov	a.tmp3,0f
	sys	unlink; 0:..
	sys	chmod; a.out; outmod: 777
	sys	exit
	.text

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
	jmp	aexit

doreloc:
	movb	(r1),r0
	bne	1f
	bisb	defund,(r1)
1:
	bic	$!37,r0
	cmp	r0,$5
	bhis	1f
	cmp	r0,$3
	blo	1f
	beq	2f
	add	bssbase,2(r1)
	rts	pc
2:
	add	datbase,2(r1)
1:
	rts	pc

setbrk:
	mov	r1,-(sp)
	add	$20,r1
	cmp	r1,0f
	blo	1f
	add	$512.,0f
	sys	indir; 9f
	.data
9:	sys	break; 0: end
	.text
1:
	mov	(sp)+,r1
	rts	pc

setup:
	mov	$curfb,r4
1:
	clr	(r4)+
	cmp	r4,$curfb+40.
	blo	1b
	mov	txtfil,fin
	clr	ibufc
	clr	r4
1:
	jsr	pc,fbadv
	tstb	(r4)+
	cmp	r4,$10.
	blt	1b
	rts	pc

ofile:
	mov	*(r5),0f
	sys	indir; 9f
	.data
9:	sys	open; 0:..; 0
	.text
	bes	1f
	tst	(r5)+
	rts	r5
1:
	jmp	filerr
