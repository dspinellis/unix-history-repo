/
/

/ f47 -- analysis of data statements

.globl	cdata
.globl	dodata
.globl	onedata
.globl	compare

.globl	code
.globl	getcon
.globl	error
.globl	getsym
.globl	consub
.globl	size
.globl	geti
.globl	setln
.globl	getln
.globl	declimpl
.globl	evalcon
.globl	dattab
.globl	contab
.globl	efno
.globl	perror
.globl	qsort
.globl	negflg
.globl	repfact
.globl	geti
.globl	holround

cdata:
	mov	r5,-(sp)
	jsr	r5,setln
	mov	$dattab,r4
	mov	$contab,r5
dloop:
	jsr	r5,getln
		br 2f
	br	1f
2:
	mov	r4,-(sp)
	mov	$dattab,r1
	mov	r4,r2
	mov	$8.,r3
	jsr	pc,qsort
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	r5
1:
	cmp	r0,$'d
	bne	dloop
	mov	$line+4,r1
/ loop per specification-set
1:
	clr	repfact
	mov	r1,r2
2:
	jsr	r5,getsym
	cmp	r0,$40.		/ =|
	bne	3f
8:
	jmp	8f
9:
	jmp	9f
3:
	cmp	r0,$6		/ /
	bne	2b
	mov	r1,-(sp)
	mov	r2,r1
	mov	(sp)+,r2
/ loop per specification
2:
	cmp	r4,r5
	blo	3f
	jmp	7f
3:
	jsr	r5,getsym
	tst	r0
	bne	8b
	bit	$70,symtab(r3)		/ test classed
	bne	3f
	jsr	r5,declimpl
3:
	mov	symtab(r3),r0
	mov	r0,holquo		/ temp storage
	incb	holquo+1		/ round size
	bicb	$1,holquo+1
	bit	$200,r0			/ test parameter
	bne	9b
	bic	$!70,r0
	cmp	r0,$10			/ simple
	beq	3f
	cmp	r0,$20			/ array
	bne	9b
3:
	bit	$100,symtab(r3)		/ test common
	beq	3f
	cmp	progt,$6		/ test block data
	bne	9b
	mov	symtab+4(r3),(r4)+	/ common block
	br	4f
3:
	cmp	progt,$6		/ test block data
	beq	9b
	clr	(r4)+
	tst	symtab+6(r3)		/ test allocated
	bne	4f
	mov	nxtaloc,symtab+6(r3)
	jsr	r5,size
	add	r0,nxtaloc
4:
	clr	(r4)			/ offset slot
	cmpb	(r1),$'(		/ test subscript
	bne	3f
	inc	r1
	jsr	r5,consub
	bic	$70,holquo
	bis	$10,holquo	/ array -> scalar
	mov	r0,(r4)
3:
	movb	symtab+1(r3),r0	/ width of item
	inc	r0
	bic	$1,r0
	mov	r0,holround
	add	symtab+6(r3),(r4)+
	tst	repfact
	beq	3f
	dec	repfact
	movb	efno+1,r3
	mov	r3,(r4)+
	mov	r5,(r4)+
	br	4f
3:
	mov	r1,-(sp)
	mov	r2,r1
	mov	(sp)+,r2
5:
	jsr	r5,getsym
	cmp	r0,$2			/ constant
	beq	3f
	cmp	r0,$10.			/ -
	bne	8f
	inc	negflg
	jsr	r5,getsym
	cmp	r0,$2			/ constant
	bne	8f
3:
	cmpb	(r1)+,$'*
	bne	3f
	cmp	r3,$intcon
	bne	8f
	jsr	r5,geti
	dec	r0
	bmi	8f
	mov	r0,repfact
	br	5b
3:
	dec	r1
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,efno
	clrb	r3
	swab	r3
	mov	r3,(r4)+
	sub	r3,r5
	mov	r5,(r4)+		/ ptr to const
	mov	r5,r3
	mov	$symbuf,r1
	jsr	r5,evalcon
	clr	negflg
	mov	(sp)+,r1
	mov	(sp)+,r2		/ note r1 <=> r2
4:
	cmpb	efno+1,holquo+1		/ compare sizes
	blt	9f			/ constant too small
	beq	3f
	bicb	$!77,holquo
	cmp	holquo,$int2con+10	/ simple int*2?
	bne	4f
	sub	$2,-4(r4)		/ reduce const length
	cmpb	efno,$5
	beq	3f			/ hollerith, OK
	add	$2,-2(r4)		/ get least sig.
	br	3f
4:
	bit	$20,holquo		/ test array
	beq	9f
	cmpb	efno,$5			/ test hollerith
	bne	9f
3:
	cmpb	(r1),$'/
	beq	3f
	cmpb	(r1)+,$',
	bne	8f
	tst	repfact
	bne	4f
	cmpb	(r2)+,$',
	bne	8f
4:
	jmp	2b
3:
	cmpb	(r2)+,$'/
	bne	8f
	tstb	(r2)
	beq	1f
	cmpb	(r2),$',
	bne	3f
	inc	r2
3:
	mov	r2,r1
	tst	repfact
	bne	8f
	jmp	1b
7:
	jsr	r5,error; 28.		/ data table overflow
	br	2f
8:
	jsr	r5,error; 21.		/ data syntax error
	br	2f
9:
	jsr	r5,error; 22.		/ data semantic error
2:
	mov	$dattab,r4		/ reset ptrs
	mov	$contab,r5
	jsr	r5,perror
1:
	jmp	dloop

dodata:
	cmp	progt,$6
	beq	1f			/ block data
	mov	$dattab,r3
	cmp	r3,r4
	bne	2f			/ is data
	mov	nxtaloc,r0
	jsr	r5,code
		<.=.+%d.\n.text\n\0>; .even
		r0
	rts	r5
2:
	jsr	r5,onedata
	sub	nxtaloc,r1
	neg	r1
	blt	9b
	jsr	r5,code
		<.=.+%d.\n.text\n\0>; .even
		r1
1:
	rts	r5

onedata:
	clr	r1
2:
	mov	2(r3),r0
	sub	r1,r0
	bmi	9f
	beq	3f
	jsr	r5,code
		<.=.+%d.\n\0>; .even
		r0
	add	r0,r1
3:
	mov	4(r3),r0
	add	r0,r1
	asr	r0
	mov	r0,-(sp)
	mov	6(r3),r2
3:
	mov	(r2)+,r0
	jsr	r5,code
		<%o\n\0>; .even
		r0
	dec	(sp)
	bne	3b
	tst	(sp)+
	add	$8.,r3
	cmp	r3,r4
	bhis	1f
	cmp	(r3),-8(r3)		/ any more in this block
	beq	2b
1:
	rts	r5
9:
	clr	line
	jsr	r5,error; 32.		/ overlapping data init
	jsr	r5,perror
	rts	r5

/ comparison routine for qsort

compare:
	cmp	(r0),(r4)
	bne	1f
	cmp	2(r0),2(r4)
1:
	rts	pc

