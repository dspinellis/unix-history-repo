/
/

/ f44 -- formats

.globl	formts
.globl	strout

.globl	setln
.globl	getln
.globl	code
.globl	error
.globl	perror
.globl	putc
.globl	xbuf

formts:
	jsr	r5,setln
	mov	$2,r3
	clr	xbuf+518.		/ pick up zero efn formats
1:
	jsr	r5,getln
		br 1f
	cmp	r0,$'s
	beq	2f
	cmp	r0,$'f
	bne	1b
	mov	efno,r0
	jsr	r5,code
		<.%d:	<\0>; .even
		r0
	mov	$line+6,r1
	jsr	r5,strout
	mov	efno,r0
	neg	r0
	br	3f
2:
	mov	efno,r0
3:
	clr	r2
2:
	cmp	r2,r3
	bhis	2f
	cmp	r0,xbuf+518.(r2)
	beq	3f
	neg	r0
	cmp	r0,xbuf+518.(r2)
	beq	3f
	add	$2,r2
	neg	r0
	br	2b
3:
	jsr	r5,error; 51.
	jsr	r5,perror
2:
	mov	r0,xbuf+518.(r3)
	add	$2,r3
	br	1b
1:
	jsr	r5,code
		<	.even\n>; .even
	jsr	r5,setln
1:
	jsr	r5,getln
		br 1f
	cmp	r0,$'r
	beq	2f
	cmp	r0,$'i
	bne	1b
	mov	efno,r0
	neg	r0
	br	3f
2:
	mov	efno,r0
3:
	mov	$2,r2			/ not zero
2:
	cmp	r2,r3
	bhis	2f
	cmp	r0,xbuf+518.(r2)
	beq	1b
	add	$2,r2
	br	2b
2:
	jsr	r5,error; 52.
	jsr	r5,perror
	br	1b
1:
	rts	r5

strout:
	movb	(r1)+,r0
	beq	3f
	cmp	r0,$'>
	beq	4f
	cmp	r0,$'\\
	bne	5f
4:
	mov	r0,-(sp)
	mov	$'\\,r0
	jsr	r5,putc; obuf
	mov	(sp)+,r0
5:
	jsr	r5,putc; obuf
	br	strout
3:
	jsr	r5,code
		<\\0\>\n\0>; .even
	rts	r5

