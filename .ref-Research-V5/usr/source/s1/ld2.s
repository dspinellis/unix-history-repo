/ ld2 -- link editor

attach:
	mov	(r5)+,r1	/ buffer
	mov	(r5)+,r2	/ file origin, size
	mov	(r2),0f		/ seek ptr
	mov	fin,r0
	sys	seek; 0:..; 0
	mov	(r2),r0
	bis	$777,r0
	inc	r0		/ start of next block
	add	$10,r1
	mov	r1,-(sp)
	add	$1000,(sp)	/ buf max
	mov	r0,-(r1)	/ next seek ptr
	mov	(sp),-(r1)
	sub	(r2)+,r0	/  left in buffer
	sub	r0,(sp)		/ next buffer word
	mov	(sp),-(r1)
	mov	(sp)+,0f	/ buffer loc
	cmp	(r2),r0
	bge	1f
	mov	(r2),r0
1:
	mov	r0,0f+2		/ number to read
	mov	(r2),-(r1)	/ left in file
	jsr	r5,rdchk; 0:..; ..
	rts	r5

rdchk:
	mov	(r5)+,0f
	mov	(r5)+,0f+2
	mov	fin,r0
	sys	read; 0:..; ..
	bes	1f
	cmp	r0,0b+2
	bne	1f
	rts	r5
1:
	jsr	r5,mesg; premeof
	jmp	sintr

getwrd:
	mov	(r5)+,r1
	sub	$2,(r1)+	/ left in file
	bge	1f
	sev			/ end of file
	rts	r5
1:
	mov	(r1)+,r2	/ word ptr
	cmp	r2,(r1)+	/ eob ptr
	bhis	1f		/ end of buffer
	mov	(r2)+,r0
	mov	r2,-4(r1)
	rts	r5
1:
	mov	(r1),0f
	mov	fin,r0
	sys	seek; 0:..; 0
	add	$1000,(r1)+	/ new seek ptr
	mov	r1,0f
	mov	-10(r1),r0	/ left in file
	add	$2,r0
	cmp	r0,$1000
	ble	1f
	mov	$1000,r0	/ read 1000 at most
1:
	mov	r0,0f+2
	jsr	r5,rdchk; 0:..; ..
	mov	(r1)+,r0
	mov	r1,-10(r1)	/ new next word
	rts	r5

oattach:
	mov	(r5)+,r1
	mov	(r5)+,r2
	mov	oattpt,r0
	add	(r2),r0
	mov	r0,oattpt
	mov	r0,r2
	bic	$!777,r0
	add	r1,r0
	add	$6,r0
	mov	r0,(r1)+	/ next slot
	mov	r1,r0
	add	$1004,r0
	mov	r0,(r1)+	/ buf max
	mov	r2,(r1)+	/ seek addr
	rts	r5

putwrd:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	(r5)+,r2
	mov	(r2)+,r1	/ slot
	cmp	r1,(r2)		/ buf max
	bhis	1f
	mov	r0,(r1)+
	mov	r1,-(r2)
	br	2f
1:
	tst	(r2)+
	mov	r0,-(sp)
	jsr	r5,flush1
	mov	(sp)+,*(r2)+
	add	$2,-(r2)
2:
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5

flush:
	mov	(r5)+,r2
	cmp	(r2)+,(r2)+
flush1:
	mov	(r2)+,r1
	mov	r1,0f		/ seek address
	mov	fout,r0
	sys	seek; 0:..; 0
	bic	$!777,r1
	add	r2,r1		/ write address
	mov	r1,0f
	mov	r2,r0
	bis	$777,-(r2)
	inc	(r2)		/ new seek addr
	cmp	-(r2),-(r2)
	sub	(r2),r1
	neg	r1
	mov	r1,0f+2		/ count
	mov	r0,(r2)		/ new next slot
	mov	fout,r0
	sys	write; 0:..; ..
	rts	r5

lookup:
	mov	$symbol,r1
	mov	(r1)+,r0
	add	(r1)+,r0
	add	(r1)+,r0
	add	(r1)+,r0
	mov	r0,r1
	clr	r0
	dvd	$hshsiz,r0
	mov	r1,r4
	asl	r4
	add	$hshtab,r4
1:
	mov	(r4)+,r0
	beq	3f		/ not found
	mov	$symbol,r1
	cmp	(r1)+,(r0)+
	bne	2f
	cmp	(r1)+,(r0)+
	bne	2f
	cmp	(r1)+,(r0)+
	bne	2f
	cmp	(r1)+,(r0)+
	bne	2f
3:
	tst	-(r4)
	rts	r5
2:
	cmp	r4,$2*hshsiz+hshtab
	blo	1b
	mov	$hshtab,r4
	br	1b

enter:
	mov	esymp,r0
	add	$14,r0
	cmp	r0,0f
	blo	1f
	add	$500.,r0
	mov	r0,0f
	sys	break; 0:symtab
1:
	mov	esymp,r0
	mov	r0,(r4)
	mov	$symbol,r1
	mov	$6,-(sp)
1:
	mov	(r1)+,(r0)+
	dec	(sp)
	bne	1b
	mov	r0,esymp
	tst	(sp)+
	rts	r5

smesg:
	mov	r1,-(sp)
	mov	r0,-(sp)
	jsr	pc,1f
	tst	filnam
	beq	2f
	mov	$1,r0
	sys	write; qsemi; 1
2:
	mov	(sp)+,r1
	mov	$8.,-(sp)
3:
	movb	(r1)+,ch
	beq	4f
	mov	$1,r0
	sys	write; ch; 1
4:
	dec	(sp)
	bne	3b
	tst	(sp)+
	mov	(sp)+,r1
	br	2f

mesg:
	jsr	pc,1f
	mov	$1,r0
2:
	sys	write; qnl; 1
	rts	r5

1:
	mov	$2,errcnt
	mov	$666,outmod		/ make a.out nonexecutable
	mov	r1,-(sp)
	mov	(r5)+,r1
1:
	movb	(r1)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	mov	filnam,r1
	beq	9f
1:
	movb	(r1)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
9:
	mov	(sp)+,r1
	rts	pc

getsym:
	mov	$6,-(sp)
	mov	$symbol,r4
1:
	jsr	r5,getwrd; txtp
	bvs	2f
	mov	r0,(r4)+
	dec	(sp)
	bne	1b
	tst	(sp)+
	rts	r5
2:
	tst	(sp)+
	sev
	rts	r5

nxtarg2:
	mov	rlistp,r1
	add	$4,rlistp
	mov	(r1)+,r0
	beq	1f		/ end of args
	cmp	r0,$177		/ see if system library
	bhi	2f
	cmp	r0,$1		/ see if archive
	beq	3f
	movb	r0,wlib
	mov	$libfil,r0
2:
	jsr	r5,aopen
		br nxtarg2
3:
	mov	(r1),libflg
	beq	2f
	sub	$20,(r1)
	mov	(r1),0f
	mov	fin,r0
	sys	seek; 0:..; 0
2:
	mov	fin,r0
	sys	read; arcmag; 40
	bes	bform1
	cmp	r0,$20
	blt	bform1
	mov	$arcmag,r4
	tst	(r1)+
	beq	filstrt
	cmp	r0,$40
	bne	bform1
	br	libstrt
1:
	rts	r5

bform1:
	jmp	bform

nxtarg:
	add	libnxt,libflg	/ see if library
	beq	advarg		/ no
	mov	libflg,0f
	mov	fin,r0
	sys	seek; 0:..; 0
	mov	fin,r0
	sys	read; arcmag+2; 40	/ get arc header, seg hdr
	mov	$arcmag+2,r4
	add	$20,libflg
	cmp	r0,$40
	beq	libstrt		/ not end of libr
advarg:
	dec	argc
	bge	1f
	rts	r5
1:
	mov	argp,r1
	tst	(r1)+
	mov	r1,argp
	cmpb	*(r1),$'-
	bne	opnarg
	jsr	r5,specarg
		br advarg
opnarg:
	clr	libflg
	clr	libnxt
	mov	*argp,r0
	clr	reopened
	jsr	r5,aopen
		br advarg
	mov	fin,r0
	sys	read; arcmag; 42 / read arc header if any, file hdr
	bes	bform
	mov	r0,r3
	mov	$arcmag,r4
	add	r4,r3
	cmp	r3,$arcmag+20
	blo	bform
	cmp	(r4),arcmagic
	bne	filstrt
	cmp	r3,$arcmag+42
	blo	bform
	tst	(r4)+
	mov	$22,libflg
libstrt:
	mov	16(r4),r0	/ next library start
	inc	r0
	bic	$1,r0
	mov	r0,libnxt
	clr	14(r4)		/ end of name
	mov	r4,filnam	/ archive entry name
	add	$20,r4		/ point to file start
filstrt:
	cmp	(r4)+,magic	/ see if object file
	bne	bform
	mov	$ctxtorg,r1
	mov	libflg,r2	/ text origin in file
	add	$20,r2		/ skip header
	mov	(r4)+,r0
	mov	r2,(r1)+	/ text origin
	mov	r0,(r1)+	/ text size
	add	r0,r2
	mov	(r4)+,r0
	mov	r2,(r1)+	/ data origin
	mov	r0,(r1)+	/ data size
	add	r0,r2
	mov	(r4)+,(r1)+	/ bss size
	mov	r2,(r1)+	/ text reloc origin
	mov	ctxtsiz,(r1)	/ text reloc size
	add	(r1)+,r2
	mov	r2,(r1)+	/ data reloc origin
	mov	cdatsiz,(r1)	/ data size
	add	(r1)+,r2
	mov	(r4)+,r0
	mov	r2,(r1)+	/ symbol table origin
	mov	r0,(r1)+	/ symbol table size
	mov	(r4)+,r0	/ stack size
	cmp	r0,stksiz
	blo	1f
	mov	r0,stksiz
1:
	mov	(r4)+,entry	/ entry
	tst	(r4)+		/ relocation suppressed?
	beq	1f
	jsr	r5,mesg; norel
	rts	r5
1:
	tst	(r5)+
	rts	r5

bform:
	jsr	r5,mesg; format
	jmp	sintr

specarg:
	mov	(r1),r0
	movb	1(r0),r0
	cmpb	r0,$'u
	beq	use
	cmpb	r0,$'l
	beq	libarg
	cmpb	r0,$'x
	beq	xtsym
	cmpb	r0,$'e
	beq	entarg
	cmpb	r0,$'r
	beq	savrel
	cmpb	r0,$'s
	beq	squash
	cmpb	r0,$'n
	beq	new
	cmpb	r0,$'d
	beq	sdcom
	rts	r5

squash:
	inc	sqflg
	inc	xtflg
	rts	r5

savrel:
	clr	relflg
	inc	clrelflg
	rts	r5

xtsym:
	inc	xtflg
	rts	r5

libarg:
	movb	$'a,wlib
	mov	(r1),r1
	movb	2(r1),r0
	beq	1f
	movb	r0,wlib
1:
	mov	$libfil,*argp
	tst	(r5)+
	rts	r5

entarg:
	clr	r4
	jsr	r5,use
	mov	(r4),entptr
	rts	r5

use:
	dec	argc
	blt	2f
	add	$2,argp
	mov	*argp,r0
	mov	$symbol,r1
	mov	$8.,-(sp)
1:
	movb	(r0)+,(r1)+
	beq	1f
	dec	(sp)
	bgt	1b
1:
	dec	(sp)
	ble	1f
	clrb	(r1)+
	br	1b
1:
	tst	(sp)+
	mov	$40,(r1)+
	clr	(r1)+
	jsr	r5,lookup
	tst	(r4)
	bne	2f
	jsr	r5,enter
2:
	rts	r5

new:
	inc	nflg
	rts	r5

sdcom:
	inc	dcom
	rts	r5

relsym:
	mov	symbol+10,r0
	bic	$!37,r0
	beq	1f
	cmp	r0,$5
	bhis	1f
	asl	r0
	add	*reltab-2(r0),symbol+12
1:
	rts	r5

lookloc:
	mov	$local,r4
1:
	cmp	r4,locp
	bhis	1f
	cmp	(r4)+,r2
	beq	2f
	tst	(r4)+
	br	1b
1:
	jsr	r5,mesg; snotfound
	jmp	sintr
2:
	mov	(r4),r4
	rts	r5

aopen:
	clr	reopened
	mov	r0,0f
	mov	r0,filnam
	mov	fin,r0
	beq	1f
	sys	close
1:
	sys	open; 0:..; 0
	bec	1f
	jsr	r5,mesg; fnotfound
	mov	$4,r0
	sys	exit
1:
	mov	r0,fin
	tst	(r5)+
	rts	r5

addin:
	add	ctxtsiz,txtsiz
	add	cdatsiz,datsiz
	add	cbsssiz,bsssiz
	rts	r5

issymbol:
	mov	r1,-(sp)
	mov	(r0)+,r1
	cmp	(r1)+,(r5)
	bne	1f
	cmp	(r1)+,2(r5)
	bne	1f
	cmp	(r1)+,4(r5)
	bne	1f
	cmp	(r1)+,6(r5)
	beq	2f
1:
	tst	(r0)+
2:
	mov	(sp)+,r1
	rts	r0
