/
/

/ fx4 -- get symbol

.globl	getsym
.globl	getid
.globl	lookid
.globl	chrtab

.globl	lookup
.globl	error
.globl	geti
.globl	holround
/ getsym returns the next basic symbol

/ 0	name	(symbol table entry in r3)
/ 2	number	(type in r3)
/ 4	**
/ 6	/
/ 8	*
/ 10	-
/ 12	+
/ 14	.lt.
/ 16	.le.
/ 18	.eq.
/ 20	.ne.
/ 22	.gt.
/ 24	.ge.
/ 26	.not.
/ 28	.and.
/ 30	.or.
/ 32	(
/ 34	)
/ 36	,
/ 38	=
/ 40	=|
/
getsym:
	mov	r2,-(sp)
	mov	r1,r2
	jsr	r5,lookup; bastab
		br 1f
	mov	r2,r1
	cmp	r0,$4
	bhis	2f
	asr	r0
	add	$'0,r0
	movb	r0,symbuf
	movb	$12,symbuf+1
	clrb	symbuf+2
	mov	$logcon,r3		/ logical*2
	mov	$2,r0
2:
	cmp	r0,$32.
	bne	2f

/ check for possible complex constant

	mov	r1,-(sp)
	movb	-2(r1),r0
	movb	chrtab(r0),r0
	beq	4f
	cmp	r0,$4
	blos	3f
4:
	jsr	r5,srconst
		br 3f
	mov	r3,r2
	cmpb	(r1)+,$',
	bne	3f
	jsr	r5,srconst
		br 3f
	cmp	r3,r2
	bhis	4f
	mov	r2,r3
4:
	cmpb	(r1)+,$')
	bne	3f
	mov	(sp)+,r1
	mov	$symbuf,r2
4:
	movb	(r1)+,(r2)
	cmpb	(r2)+,$')
	bne	4b
	clrb	-(r2)
	mov	$2,r0
	br	2f
3:
	mov	(sp)+,r1
	mov	$32.,r0
2:
	mov	(sp)+,r2
	rts	r5
1:
	clr	lstchr
	cmp	r1,$line
	blos	1f
	movb	-1(r1),lstchr
1:
	mov	$symbuf,r2
	movb	(r1)+,r0
	movb	r0,(r2)+
	bic	$!177,r0
	movb	chrtab(r0),r0
	jmp	*1f(r0)
1:
	eos
	let
	num
	per

eos:
	mov	$40.,r0
	tstb	-(r1)
	beq	2b
	jsr	r5,error; 8.
	br	2b

let:
	dec	r1
	jsr	r5,getid
		br .+2		/ cannot happen
	jsr	r5,lookid; symbuf
	mov	(sp)+,r2
	clr	r0
	rts	r5

num:
	mov	$intcon,r3	/ integer*4
	jsr	r5,numst
	cmpb	(r1),$'.
	bne	2f
	mov	r2,-(sp)
	mov	r1,r2
	jsr	r5,lookup; bastab
		br 1f
	mov	(sp)+,r2
	br	3f
1:
	mov	(sp)+,r2
	movb	(r1)+,(r2)+
	br	1f
2:
	cmpb	(r1),$'h		/ hollerith const?
	bne	2f
	mov	lstchr,r0
	cmpb	chrtab(r0),$2		/ letter?
	beq	2f			/ not h, then
	cmp	r0,$'*
	beq	2f			/ e.g. real*4 h...
	clrb	(r2)
	jsr	r5,geti
	mov	$symbuf,r2
	inc	r1
	mov	holround,-(sp)
	dec	(sp)
	clr	-(sp)
4:
	movb	(r1)+,(r2)+
	bne	5f
	jsr	r5,error; 55.
	br	6f
5:
	inc	(sp)
	dec	r0
	bgt	4b
6:
	bit	(sp),2(sp)
	beq	6f
	movb	$' ,(r2)+
	inc	(sp)
	br	6b
6:
	mov	(sp)+,r3
	tst	(sp)+
	swab	r3
	clrb	r3
	bis	$5,r3
	mov	$2,r0
	mov	(sp)+,r2
	rts	r5

.bss
lstchr:	.=.+2
.text

per:
	jsr	r5,isnum
		br eos
1:
	mov	$realcon,r3	/ real*4
	jsr	r5,numst
2:
	jsr	r5,expon
3:
	clrb	(r2)
	mov	$2,r0
	mov	(sp)+,r2
	rts	r5

isnum:
	movb	(r1),r0
	cmpb	chrtab(r0),$4
	bne	1f
	tst	(r5)+
1:
	rts	r5

numst:
	jsr	r5,isnum
		br 1b
	inc	r1
	movb	r0,(r2)+
	br	numst

expon:
	cmpb	(r1)+,$'e
	beq	1f
	cmpb	-1(r1),$'d
	beq	1f
2:
	dec	r1
	rts	r5
1:
	cmpb	(r1),$'+
	beq	1f
	cmpb	(r1),$'-
	beq	1f
	jsr	r5,isnum
		br 2b
1:
	mov	$realcon,r3	/ real*4
	cmpb	-(r1),$'e
	beq	1f
	mov	$dblcon,r3	/ real*8
1:
	movb	(r1)+,(r2)+
	movb	(r1)+,(r2)+
	jsr	r5,numst
	rts	r5

getid:
	mov	r0,-(sp)
	mov	r2,-(sp)
	movb	(r1),r0
	cmpb	chrtab(r0),$2
	bne	3f
	tst	(r5)+
	mov	$symbuf,r2
1:
	movb	(r1)+,r0
	movb	r0,(r2)+
	movb	chrtab(r0),r0
	cmp	r0,$2
	beq	1b
	cmp	r0,$4
	beq	1b
	dec	r1
	clrb	(r2)
	movb	$12,-(r2)
3:
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	r5

lookid:
	mov	r0,-(sp)
	mov	r2,-(sp)
2:
	mov	(r5),r2
	jsr	r5,lookup; namebuf
		br 1f
	asl	r0
	asl	r0
	mov	r0,r3
	mov	(sp)+,r2
	mov	(sp)+,r0
	tst	(r5)+
	rts	r5
1:
	mov	namep,r0
	add	$8.,symtp
1:
	movb	(r2)+,(r0)+
	bne	1b
	mov	r0,namep
	cmp	r0,$enamebuf
	bhis	1f
	mov	symtp,r0
	add	$symtab,r0
	cmp	r0,esymp
	blo	2b
1:
	mov	$1,r0
	sys	write; ovfl; eovfl-ovfl
	clr	r0
	sys	seek; 0; 2
	mov	$-1,r0
	sys	exit

ovfl:
	<Symbol table overflow\n>
eovfl:
.even

srconst:
	cmpb	(r1)+,$'+
	beq	1f
	cmpb	-(r1),$'-
	bne	1f
	inc	r1
1:
	jsr	r5,getsym
	cmp	r0,$2
	bne	1f
	clrb	r3
	add	r3,r3
	bisb	$cplxcon,r3
	tst	(r5)+
1:
	rts	r5

chrtab:
	.byte	0,0,0,0,0,0,0,0
	.byte	0,0,0,0,0,0,0,0
	.byte	0,0,0,0,0,0,0,0
	.byte	0,0,0,0,0,0,0,0
	.byte	0,0,0,0,0,0,0,0
	.byte	0,0,0,0,0,0,6,0
	.byte	4,4,4,4,4,4,4,4
	.byte	4,4,0,0,0,0,0,0
	.byte	0,2,2,2,2,2,2,2
	.byte	2,2,2,2,2,2,2,2
	.byte	2,2,2,2,2,2,2,2
	.byte	2,2,2,0,0,0,0,0
	.byte	0,2,2,2,2,2,2,2
	.byte	2,2,2,2,2,2,2,2
	.byte	2,2,2,2,2,2,2,2
	.byte	2,2,2,0,0,0,0,0

bastab:
	<.false.\0>
	<.true.\0>
	<**\0>
	</\0>
	<*\0>
	<-\0>
	<+\0>
	<.lt.\0>
	<.le.\0>
	<.eq.\0>
	<.ne.\0>
	<.gt.\0>
	<.ge.\0>
	<.not.\0>
	<.and.\0>
	<.or.\0>
	<(\0>
	<)\0>
	<,\0>
	<=\0>
	<\0>

