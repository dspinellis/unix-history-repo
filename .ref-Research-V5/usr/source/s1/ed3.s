/
/ copyright 1972 bell telephone laboratories inc.
/

/ ed3 -- text editor

compile:
	mov	r1,compt
	mov	r1,prect
	mov	$exprbuf,r3
	jsr	r5,switch; prect
	jsr	r5,cop; 2
	br	cadv

.data
prect:
	..;	ceof1
	'^;	cadv1
prect1:
	'*;	cerr
	0
.text

cadv1:
	jsr	r5,switch; prect1	/ ^* case

cadv:
	jsr	r5,switch; compt
	jsr	r5,cop; 4
	jsr	r5,getc
	movb	r1,(r3)+
	cmp	r3,$eexprbuf
	blo	cadv
	br	cerr

.data
compt:
	..;	ceof
	'\\;	cesc
	'\n;	cerr
	'.;	cdot
	'*;	cast
	'$;	cdol
	'[;	cccl
	0
.text

ceof:
	clrb	(r3)+
	cmp	r3,$eexprbuf-1
	bhis	cerr
	movb	$1,(r3)+
	rts	r5

ceof1:
	tst	(r3)
	beq	cerr
	rts	r5

cesc:
	jsr	r5,cop; 4
	jsr	r5,getc
	cmp	r1,$'\n
	beq	cerr
	movb	r1,(r3)+
	cmp	r3,$eexprbuf
	blo	cadv

cerr:
	clr	exprbuf
	jmp	9b

cdot:
	jsr	r5,cop; 8.
	br	cadv

cdol:
	jsr	r5,getc
	mov	r1,peekc
	cmp	r1,compt
	beq	1f
	jsr	r5,cop; 4
	movb	$'$,(r3)+
	cmp	r3,$eexprbuf
	blo	cadv
	br	cerr
1:
	jsr	r5,cop; 20.
	br	cadv

cccl:
	jsr	r5,cop; 12.
	jsr	r5,getc
	cmp	r1,$'^
	bne	1f
	movb	*f,r1
	add	$4,r1
	movb	r1,*f
	jsr	r5,getc
1:
	cmp	r1,$'\n
	beq	cerr
	movb	r1,(r3)+
	cmp	r3,$eexprbuf-1
	bhis	cerr
	jsr	r5,getc
	cmp	r1,$']
	bne	1b
	clrb	(r3)+
	br	cadv

cast:
	bisb	$2,*f
	br	cadv

cop:
	mov	r3,f
	movb	(r5)+,(r3)+
	cmp	r3,$eexprbuf
	bhis	cerr
	tstb	(r5)+
	rts	r5

gexecute:
	cmpb	exprbuf,$2
	beq	1f
	rts	r5		/ ^ in global substitute
1:
	mov	$linebuf,r3
	mov	$subbuf,r4
1:
	movb	(r4)+,(r3)
	cmpb	(r3)+,$'\n
	bne	1b
	mov	loc2,r4
	add	$linebuf-subbuf,r4
	mov	r4,locs
	br	1f

execute:
	jsr	r5,getline
	mov	$linebuf,r4
	clr	locs
1:
	mov	r4,loc1
	mov	$exprbuf,r3

eadv:
	movb	(r3)+,r1
	jmp	*1f(r1)
1:
	eeof
	ecmf
	echr
	echrs
	edot
	edots
	eccl
	eccls
	enccl
	enccls
	edol

eeof:
	mov	r4,loc2

eeof1:
	tst	(r5)+
efail:
	rts	r5

ecmf:
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	r5,eadv
		br 1f
	mov	(sp)+,loc1
	tst	(sp)+
	br	eeof1
1:
	mov	(sp)+,r4
	mov	(sp)+,r3
	cmpb	(r4)+,$'\n
	bne	ecmf
	br	efail

echr:
	cmpb	(r3)+,(r4)+
	beq	eadv
	br	efail

echrs:
	movb	(r3)+,r1
	mov	r4,-(sp)
1:
	cmpb	(r4)+,r1
	beq	1b
	br	east

edot:
	cmpb	(r4)+,$'\n
	bne	eadv
	br	efail

edots:
	mov	r4,-(sp)
1:
	cmpb	(r4)+,$'\n
	bne	1b
	br	east

eccl:
	jsr	r5,cclas; 0
		br efail
	br	eadv

enccl:
	jsr	r5,cclas; 1
		br efail
	br	eadv

enccls:
	mov	pc,0f
	br	1f

eccls:
	clr	0f
1:
	mov	r4,-(sp)
	mov	r3,-(sp)
	jmp	2f
.data
2:
	jsr	r5,cclas; 0:0
		br 1f
	mov	(sp),r3
	br	2b
1:
	tst	(sp)+
	jmp	east
.text

edol:
	cmpb	(r4),$'\n
	beq	eadv
	br	efail

east:
	dec	r4
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	r5,eadv
		br 1f
	cmp	r4,locs
	bne	2f
	mov	r4,4(sp)
	br	1f
2:
	add	$6,sp
	br	eeof1
1:
	mov	(sp)+,r4
	mov	(sp)+,r3
	cmp	r4,(sp)
	bhi	east
	tst	(sp)+
	br	efail

cclas:
	movb	(r4)+,r1
	cmp	r1,$'\n
	beq	2f
1:
	cmpb	r1,(r3)+
	beq	1f
	tstb	(r3)
	bne	1b
	tst	(r5)+
	beq	3f
2:
	tst	(r5)+
	br	3f
1:
	tst	(r5)+
	beq	2b
3:
	tstb	(r3)+
	bne	3b
	rts	r5

8:	jmp	advanc
9:	jmp	error

dosub:
	mov	$linebuf,r1
	mov	$subbuf,r2
	mov	$rhsbuf,r3
1:
	cmp	r1,loc1
	beq	1f
	movb	(r1)+,(r2)+
	br	1b
1:
	movb	(r3)+,r0
	beq	1f
	cmp	r0,$'&
	bne	3f
	mov	r1,r4
2:
	cmp	r4,loc2
	beq	1b
	movb	(r4)+,(r2)+
	cmp	r2,$esubbuf
	blo	2b
	br	9b
3:
	bic	$200,r0
	movb	r0,(r2)+
	cmp	r2,$esubbuf
	blo	1b
	br	9b
1:
	mov	r4,r1
	mov	r2,loc2
1:
	movb	(r1)+,r0
	movb	r0,(r2)+
	cmp	r0,$'\n
	beq	1f
	cmp	r2,$esubbuf
	blo	1b
	br	9b
1:
	mov	addr1,dot
	rts	r5

switch:
	jsr	r5,getc
	mov	(r5)+,r4
1:
	tst	(r4)
	beq	1f
	cmp	r1,(r4)+
	beq	2f
	tst	(r4)+
	br	1b
1:
	mov	r1,peekc
	rts	r5
2:
	mov	(sp)+,r5
	jmp	*(r4)

getc:
	mov	peekc,r1
	bne	1f
	tst	gflag
	beq	2f
	movb	*gbufp,r1
	beq	8b
	inc	gbufp
	br	1f
2:
	clr	r0
	sys	read; ch; 1
	bes	2f
	tst	r0
	beq	2f
	mov	ch,r1
	beq	getc
1:
	clr	peekc
	rts	r5
2:
	jmp	cq1

print:
	mov	(r5),r0
	jsr	r5,size
	mov	r4,0f
	mov	(r5)+,0f-2
	mov	$1,r0
	sys	0; 7f
.data
7:
	sys	write; ..; 0:..
.text
	rts	r5

size:
	clr	r4
1:
	inc	r4
	cmpb	(r0)+,$'\n
	bne	1b
	rts	r5

qex:	<!\n>
qbadf:	<tmp file\n>
qerr:	<?\n>
qed:	<Editing System\n>
prompt:	<*>
.data
qetmp:	</tmp/etma\0>
.even
peekc:	0
eflag:	0
gbufp:	gbuf-1
kp:	kname

.bss
intrflg:.=.+2
argc:	.=.+2
nonum:	.=.+2
ibuf:	.=.+512.
obuf:	.=.+512.
kname:	.=.+20.
ekname:	.=.+2
nl:	.=.+2
kaddr:	.=.+20.
ekaddr:	.=.+2
oblkp:	.=.+2
zero:	.=.+2
dol:	.=.+2
dot:	.=.+2
gsp:	.=.+2
shflg:	.=.+2
exprbuf:.=.+128.; eexprbuf:
fin:	.=.+2
fout:	.=.+2
ch:	.=.+2
fch:	.=.+2
pflag:	.=.+2
gflag:	.=.+2
addr:	.=.+2
addr1:	.=.+2
addr2:	.=.+2
gsubf:	.=.+2
loc1:	.=.+2
loc2:	.=.+2
locs:	.=.+2
adrflg:	.=.+2
f:	.=.+2
minflg:	.=.+2
count:	.=.+2
filec:	.=.+2
filep:	.=.+2
linebuf:.=.+512.; elinbuf:
fbuf:subbuf:.=.+512.; efbuf: esubbuf:
rhsbuf:	.=.+256.; erhsbuf:
filsiz = 64.
filebuf:.=.+filsiz
filsav:	.=.+filsiz+2
gz: .=.+2
gbuf:	.=.+256.; egbuf:
buffer: ebuffer = buffer+8004.

