/ ld1 -- link editor

orig:
	sys	signal; 2; 1
	ror	r0
	bcs	1f
	sys	signal; 2; sintr
1:
	mov	(sp)+,r0
	dec	r0
	bgt	1f
	clr	r0
	sys	exit
1:
	mov	r0,argc
	mov	sp,argp
1:
	jsr	r5,nxtarg
		br pass2
	jsr	r5,load1
	br	1b

pass2:
	sys	creat; l.out; 666
	bec	1f
	clr	filnam
	jsr	r5,mesg; outfile
	mov	$10,r0
	sys	exit
1:
	mov	r0,fout
	mov	$symtab,r5
1:
	cmp	r5,esymp
	bhis	1f
	cmp	10(r5),$40	/ undefined?
	bne	2f
	tst	12(r5)		/ common?
	bne	2f
	jsr	r0,issymbol; _end
		br 2f
	jsr	r0,issymbol; _etext
		br 2f
	jsr	r0,issymbol; _edata
		br 2f
	clr	relflg		/ save relocation
	br	1f
2:
	add	$14,r5
	br	1b
1:
	mov	txtsiz,r1
	tst	nflg
	beq	1f
	add	$77,r1		/ 0 mod 64
	bic	$77,r1
	mov	r1,txtsiz
	add	$17777,r1
	bic	$17777,r1
1:
	mov	datsiz,r2
	mov	r1,r3
	add	r2,r3		/ bss origin
	clr	r4
	tst	dcom
	bne	1f
	tst	relflg
	beq	9f
1:
	mov	$symtab,r5
1:
	cmp	r5,esymp
	bhis	1f
	cmp	10(r5),$40	/ undefined
	bne	2f
	mov	12(r5),r0	/ common?
	beq	2f		/ no
	mov	r4,12(r5)	/ common origin
	add	r3,12(r5)
	inc	r0
	bic	$1,r0		/ even
	add	r0,r4		/ new common origin
	mov	$47,10(r5)	/ temp. common type
2:
	add	$14,r5
	br	1b
1:
	add	r4,bsssiz
9:
	mov	$symtab,r5
1:
	cmp	r5,esymp
	bhis	1f
	tst	dcom
	bne	3f
	tst	relflg
	beq	2f
3:
	cmp	10(r5),$40	/ undefined
	bne	2f
	tst	12(r5)		/ common?
	bne	2f
	jsr	r0,issymbol; _end
		br 3f
	br	4f
3:
	mov	$44,10(r5)	/ bss type
	mov	bsssiz,12(r5)	/ at end of bss
	sub	r4,12(r5)	/ common compensation
	br	2f
4:
	jsr	r0,issymbol; _etext
		br 3f
	br	4f
3:
	mov	$42,10(r5)	/ text type
	mov	txtsiz,12(r5)	/ at end of text
4:
	jsr	r0,issymbol; _edata
		br 3f
	br	2f
3:
	mov	$43,10(r5)	/ data type
	mov	datsiz,12(r5)
2:
	cmp	10(r5),$43
	blt	2f		/ undef, abs or text
	beq	3f
	cmp	10(r5),$47	/ common
	bne	4f
	mov	$44,10(r5)	/ set bss
	br	2f
4:
	add	r2,12(r5)	/ bss
	add	r4,12(r5)	/ common total
3:
	add	r1,12(r5)	/ data
2:
	add	$14,r5
	br	1b
1:
	mov	r1,fdatorg
	mov	r3,fbssorg
	add	r4,fbssorg
	mov	symsiz,locsymsiz
	add	esymp,symsiz
	sub	$symtab,symsiz
	tst	sqflg
	beq	1f
	clr	symsiz
1:
	jsr	r5,oattach; otxtp; zero
	add	$20,oattpt
	jsr	r5,oattach; odatp; txtsiz
	tst	relflg		/ suppress relocation?
	bne	1f
	jsr	r5,oattach; otrelp; datsiz
	jsr	r5,oattach; odrelp; txtsiz
1:
	jsr	r5,oattach; osymp; datsiz
	tst	entptr
	beq	1f
	tst	exorig
	beq	2f
	jsr	r5,mesg; mulent		/ too many entry points
2:
	mov	entptr,r0
	mov	12(r0),exorig
	inc	exorig		/ 0 entry means none
1:
	mov	$7,-(sp)
	mov	$magic,r1	/ write magic at start
	tst	sqflg
	beq	1f
	clr	symsiz
1:
	mov	(r1)+,r0
	tst	nflg
	beq	1f
	inc	r0		/ ro text segment
1:
	jsr	r5,putwrd; otxtp
1:
	mov	(r1)+,r0
	jsr	r5,putwrd; otxtp
	dec	(sp)
	bne	1b
	tst	(sp)+
	clr	txtsiz
	clr	datsiz
	clr	bsssiz
	clr	*rlistp
	mov	$rlist,rlistp
1:
	jsr	r5,nxtarg2
		br 1f
	jsr	r5,load2
	br	1b
1:
	jsr	r5,flush; otxtp
	jsr	r5,flush; odatp
	tst	relflg
	bne	1f
	jsr	r5,flush; otrelp
	jsr	r5,flush; odrelp
1:
	mov	$symtab,r1
1:
	cmp	r1,esymp
	bhis	1f
	tst	clrelflg
	bne	2f
	cmp	10(r1),$40
	bne	2f
	tst	12(r1)		/ common?
	bne	2f
	mov	r1,r0
	clr	filnam
	jsr	r5,smesg; undmes
2:
	tst	sqflg
	beq	3f
	add	$14,r1
	br	1b
3:
	mov	$6,r2
2:
	mov	(r1)+,r0
	jsr	r5,putwrd; osymp
	dec	r2
	bne	2b
	br	1b
1:
	jsr	r5,flush; osymp
done:
	sys	unlink; a.out
	sys	link; l.out; a.out
	bec	1f
	clr	filnam
	jsr	r5,mesg; movemes
	mov	$10,r0
	sys	exit
1:
	sys	unlink; l.out
	sys	chmod; a.out; outmod: 777
	mov	errcnt,r0
	sys	exit
sintr:
	sys	unlink; l.out
	mov	$6,r0
	sys	exit

load1:
	mov	txtsiz,txtorg
	mov	datsiz,datorg
	sub	ctxtsiz,datorg
	mov	bsssiz,bssorg
	sub	ctxtsiz,bssorg
	sub	cdatsiz,bssorg
	mov	$14,locsymsiz
	clr	ndef
	mov	r5,-(sp)
	mov	esymp,-(sp)
	mov	$local,r5
	jsr	r5,attach; txtp; csymorg
1:
	jsr	r5,getsym
	bvs	1f
	cmp	symbol+10,$40
	bge	2f		/ external
	tst	xtflg
	bne	1b
	add	$14,locsymsiz	/ count locals
	br	1b
2:
	jsr	r5,lookup
	mov	(r4),r0
	beq	2f		/ not yet in table
	cmp	10(r0),$40
	bgt	1b		/ multiply defined
	cmp	symbol+10,$42	/ is text?
	bne	3f
	tst	12(r0)		/ is genuine common?
	bne	1b		/ yes, don't satisfy with text
3:
	inc	ndef		/ remember def occurred
	cmp	symbol+10,$40
	bgt	3f
	dec	ndef		/ forget def occurred
	cmp	12(r0),symbol+12	/ extend common region?
	bge	1b
	br	3f
2:
	mov	r4,(r5)+
	jsr	r5,enter
	cmp	symbol+10,$40
	ble	1b
3:
	jsr	r5,relsym
	mov	(r4),r0
	mov	symbol+10,10(r0)
	mov	symbol+12,12(r0)
	br	1b
1:
	tst	libflg		/ load anyway if not library
	beq	1f
	tst	ndef		/ load library if any definitions
	bne	1f
	mov	(sp)+,esymp	/ rip out symbols
2:
	cmp	r5,$local	/ see if end of entered symbols
	blos	2f
	clr	*-(r5)		/ rip out hash entry
	br	2b
1:
	tst	(sp)+
	tst	entry
	beq	1f
	add	txtorg,entry
	tst	exorig
	beq	5f
	jsr	r5,mesg; mulent
5:
	mov	entry,exorig
1:
	add	locsymsiz,symsiz	/ total of local symbs
	mov	rlistp,r0
	cmp	r0,$rliste
	blo	1f
	jsr	r5,mesg; toomany
	jmp	done
1:
	mov	reopened,r1
	bne	1f
	mov	*argp,r1
	cmp	r1,$libfil
	bne	1f
	movb	wlib,r1		/ library, just remember letter name
1:
	mov	r1,(r0)+
	mov	libflg,(r0)+	/ remember start of routine
	beq	3f		/ not library
	bis	$1,reopened
3:
	mov	r0,rlistp
	jsr	r5,addin
2:
	mov	(sp)+,r5
	rts	r5

load2:
	mov	txtsiz,txtorg
	mov	fdatorg,r0
	add	datsiz,r0
	sub	ctxtsiz,r0
	mov	r0,datorg
	mov	fbssorg,r0
	add	bsssiz,r0
	sub	ctxtsiz,r0
	sub	cdatsiz,r0
	mov	r0,bssorg
	mov	r5,-(sp)
	jsr	r5,attach; txtp; csymorg
	mov	$local,r5
	mov	$-1,-(sp)	/ local symbol index
	mov	filnam,r1
2:
	tstb	(r1)+
	bne	2b
2:
	cmp	r1,filnam
	blos	2f
	cmpb	-(r1),$'/
	bne	2b
	tstb	(r1)+
2:
	mov	$symbol,r0
2:
	movb	(r1)+,(r0)+
	bne	3f
	tstb	-(r1)
3:
	cmp	r0,$symbol+8.
	blo	2b
	mov	$37,symbol+10
	mov	txtorg,symbol+12
	tst	sqflg
	bne	1f
	jsr	r5,wrlocsym
1:
	jsr	r5,getsym
	bvs	1f
	jsr	r5,relsym
	inc	(sp)
	cmp	symbol+10,$40
	blo	5f
	jsr	r5,lookup
	tst	(r4)
	bne	6f
	jsr	r5,mesg; snotfound
	br	1b
6:
	cmp	symbol+10,$40
	bgt	2f
	beq	4f
5:
	tst	xtflg
	bne	1b		/ skip locals
	jsr	r5,wrlocsym	/ write local symbol
	br	1b
2:
	mov	(r4),r0
	cmp	10(r0),symbol+10
	bne	2f
	cmp	12(r0),symbol+12
	beq	4f
2:
	jsr	r5,smesg; multi
4:
	cmp	r5,$elocal
	blo	3f
	jsr	r5,mesg; locovflo
	jmp	done
3:
	mov	(sp),(r5)+	/ save local index
	mov	(r4),(r5)+	/ save symbol location
	br	1b
1:
	mov	r5,locp
	tst	(sp)+
	jsr	r5,attach; txtp; ctxtorg
	jsr	r5,attach; relp; ctrelorg
	mov	txtsiz,relbas
1:
	jsr	r5,txtlod
		br 1f
	tst	relflg
	bne	2f
	jsr	r5,putwrd; otrelp
2:
	mov	r3,r0
	jsr	r5,putwrd; otxtp
	br	1b
1:
	jsr	r5,attach; txtp; cdatorg
	jsr	r5,attach; relp; cdrelorg
	mov	datorg,r0
	mov	r0,relbas
	mov	r5,locp
	mov	(sp)+,r5
1:
	jsr	r5,txtlod
		br 1f
	tst	relflg
	bne	2f
	jsr	r5,putwrd; odrelp
2:
	mov	r3,r0
	jsr	r5,putwrd; odatp
	br	1b
1:
	jsr	r5,addin
	rts	r5

txtlod:
	jsr	r5,getwrd; txtp
	bvs	1f
	tst	(r5)+
	mov	r0,r3
	jsr	r5,getwrd; relp
	bvc	2f
	jsr	r5,mesg; relerr
	mov	$4,r0
	sys	exit
2:
	mov	r0,r2
	bic	$1,r2
	bic	$!17,r0
	cmp	r0,$7
	blos	3f
/ external symbol reference
	clc
	ror	r2
	asr	r2
	asr	r2
	asr	r2		/ get symbol number
	jsr	r5,lookloc	/ get symbol
	cmp	10(r4),$40
	bgt	2f
/ still undefined
	sub	$symtab,r4
	add	locsymsiz,r4
	mov	r5,-(sp)
	mov	r4,r5
	clr	r4
	dvd	$14,r4
	als	$4,r4
	bis	r4,r0
	mov	(sp)+,r5
	br	relrel
2:
/ symbol now defined
	add	12(r4),r3	/ symbol value
	mov	10(r4),r2
	sub	$41,r2
	bic	$!1,r0
	bne	4f
	tst	r2
	beq	5f		/ not relative & relocatable
	add	dotdot,r3
	br	5f
4:
	tst	r2
	bne	5f		/ relative & absolute
	sub	dotdot,r3
5:
	asl	r2
	bis	r2,r0
	br	relrel
/ absolute, text, data, or bss symbol
3:
	add	*reltab(r2),r3
relrel:
	bit	$1,r0
	beq	1f
	sub	relbas,r3	/ relative relocation
1:
	rts	r5

wrlocsym:
	mov	$symbol,r1
	mov	$6,-(sp)
3:
	mov	(r1)+,r0
	jsr	r5,putwrd; osymp / write out local symbol
	dec	(sp)
	bne	3b
	tst	(sp)+
	rts	r5

