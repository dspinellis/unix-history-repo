/
/

/ fx9 -- code

/	jsr	r5,code; <string\0>; .even
/		arg1
/		arg2
/		...
/
/ args specified by %<c> in string.
/   %s:  string
/   %d:  decimal number
/   %o:  octal number
/   %c:  character
/
/ args are addresses
/ or registers r0-r4

.globl	code

.globl	putc
.globl	getname

code:
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	mov	r5,r4
1:
	tstb	(r5)+
	bne	1b
	inc	r5
	bic	$1,r5
1:
	movb	(r4)+,r0
	beq	1f
	cmp	r0,$'%
	beq	2f
	jsr	r5,putc; obuf
	br	1b
2:
	movb	(r4)+,r0
	beq	1f
	cmp	r0,$'s
	beq	cstr
	cmp	r0,$'c
	beq	cchr
	cmp	r0,$'d
	beq	cdec
	cmp	r0,$'o
	beq	coct
	cmp	r0,$'n
	beq	cnam
	jsr	r5,putc; obuf
	br	1b
cnam:
	jsr	pc,cget
	mov	r1,r3
	jsr	r5,getname
	mov	$symbuf,r1
	br	2f
cstr:
	jsr	pc,cget
2:
	movb	(r1)+,r0
	beq	1b
	cmp	r0,$'\n
	beq	2b
	jsr	r5,putc; obuf
	br	2b
cchr:
	jsr	pc,cget
	mov	r1,r0
	jsr	r5,putc; obuf
	mov	r1,r0
	clrb	r0
	swab	r0
	beq	1b
	jsr	r5,putc; obuf
	br	1b
coct:
	jsr	pc,cget
	mov	$8.,r2
	br	2f
cdec:
	jsr	pc,cget
	mov	$10.,r2
2:
	jsr	pc,2f
	br	1b
2:
	clr	r0
	dvd	r2,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	2f
	jsr	pc,2b
2:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	r5,putc; obuf
	rts	pc
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	r5

cget:
	mov	(r5)+,r1
	cmp	r1,$4
	blos	1f
	rts	pc
1:
	asl	r1
	add	$2,r1
	add	sp,r1
	mov	(r1),r1
	rts	pc

