/
/

/ fx8 -- read source line

.globl	getline
.globl	isagn
.globl	levzer

.globl	error
.globl	getc
.globl	chrtab

getline:
	mov	r3,-(sp)
	clr	holquo
	clr	r3		/ line number
	clr	line
	mov	$line,r1
1:
	jsr	pc,get
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	mpy	$10.,r3
	add	r0,r3
	br	1b
1:
	add	$'0,r0
	movb	r0,ch
	mov	r3,efno
1:
	mov	lino, ifno
	inc	ifno
	jsr	pc,get
	cmp	r0,$'\n		/ nl
	beq	1f
	cmp	r0,$''
	beq	2f
	cmp	r0,$'"
	bne	3f
2:
	mov	r0,holquo
	add	$4,r1		/ room for h count
	mov	r1,-(sp)
2:
	jsr	pc,get
	cmp	r0,$'\n
	beq	4f
	cmp	r0,holquo
	beq	2f
	bis	$200,r0
	movb	r0,(r1)+
	cmp	r1,$eline-1
	blo	2b
4:
	tst	(sp)+
	br	err1
2:
	mov	(sp)+,r3
	mov	r1,-(sp)
	sub	r3,r1
	movb	$'h,-(r3)
	clr	r0
	div	$10.,r0
	add	$'0,r1
	movb	r1,-(r3)
	mov	r0,r1
	clr	r0
	div	$10.,r0
	add	$'0,r1
	movb	r1,-(r3)
	add	$'0,r0
	movb	r0,-(r3)
	mov	(sp)+,r1
	clr	holquo
	br	1b
3:
	cmp	r0,$'h
	bne	2f
	clr	r3		/ quote count
	mov	r1,-(sp)
3:
	cmp	r1,$line
	blos	4f
	movb	-(r1),r0
	cmpb	chrtab(r0),$4	/ digit?
	beq	3b
	cmpb	chrtab(r0),$2	/ test letter
	beq	3f
	cmp	r0,$'*
	beq	3f
	inc	r1
4:
	cmp	r1,(sp)
	bhis	4f
	movb	(r1)+,r0
	sub	$'0,r0
	mpy	$10.,r3
	add	r0,r3
	br	4b
4:
	mov	r3,r0
	beq	3f
	mov	r0,holquo
	mov	(sp)+,r1
	movb	$'h,(r1)+
4:
	jsr	pc,get
	cmp	r0,$'\n
	bne	5f
	mov	r0,ch
	mov	$' ,r0
5:
	bis	$200,r0
	movb	r0,(r1)+
	cmp	r1,$eline-1
	bhis	err1
	dec	holquo
	bne	4b
	br	1b
3:
	mov	(sp)+,r1
	mov	$'h,r0
2:
	movb	r0,(r1)+
	cmp	r1,$eline-1
	blo	1b
err1:
	jsr	r5,error; 1.
1:
	clrb	(r1)+
	mov	(sp)+,r3
	tstb	line
	bne	1f
	jmp	getline
1:
	rts	r5

isagn:
	jsr	r5,levzer; '=
		br 1f
	br	3f
1:
	mov	r0,-(sp)
	jsr	r5,levzer; ',
		br 4f
	jsr	r5,levzer; '(
		br 1f
	tst	(sp)+
2:
	rts	r5
1:
	cmp	(sp)+,r0
	blt	2b
	mov	r1,-(sp)
	mov	r0,r1
	inc	r1
	jsr	r5,levzer; ')
		br 1f
1:
	mov	(sp)+,r1
	cmpb	1(r0),$'=
	bne	3f
	rts	r5

levzer:
	mov	r1,r0
	clr	-(sp)
1:
	tst	(sp)
	bne	2f
	cmpb	(r0),(r5)
	beq	1f
2:
	cmpb	(r0),$'(
	bne	2f
	inc	(sp)
2:
	cmpb	(r0),$')
	bne	2f
	dec	(sp)
	blt	5f
2:
	tstb	(r0)+
	bne	1b
5:
	tst	(r5)+
1:
4:
	tst	(sp)+
3:
	tst	(r5)+
	rts	r5

get:
	movb	ch,r0
	beq	1f
	clrb	ch
	rts	pc
1:
	jsr	pc,get1
	cmp	r0,$'\n
	bne	2f
	jsr	pc,get1
	cmp	r0,$'&
	beq	1b
	movb	r0,ch1
	mov	$'\n,r0
2:
	tst	holquo
	bne	1f
	cmp	$' ,r0
	beq	1b
	cmp	$'\t,r0
	beq	1b
1:
	cmp	r0,$4		/ test EOT
	bne	1f
	mov	$1,r0
	sys	write; mes; emes-mes
	mov	$1,r0		/ syntax errors detected
	sys	exit
1:
	rts	pc

get1:
	movb	ch1,r0
	beq	1f
	clrb	ch1
	br	2f
1:
	jsr	r5,getc; ibuf
	bcs	1f
	bic	$!177,r0
	beq	1b
	cmp	r0,$'\n
	bne	2f
	inc	lino
2:
	tst	nlflg
	beq	2f
	clr	nlflg
	cmp	r0,$'c
	bne	2f
3:
	jsr	pc,get1
	cmp	r0,$'\n
	beq	1b
	cmp	r0,$4
	bne	3b
2:
	cmp	r0,$'\n
	bne	2f
	inc	nlflg
2:
	rts	pc
1:
	mov	$4,r0
	rts	pc

.data
nlflg:	1
.text
mes:
	<EOF on input\n\0>
emes:
.bss
lino:	.=.+2
