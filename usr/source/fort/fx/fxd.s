/
/

/ xd -- tmp file 2 handl

.globl	setln
.globl	getln

.globl	tfil2
.globl	tfildiag
.globl	fopen
.globl	getc
.globl	getw
.globl	xbuf

setln:
	mov	$tfil2,r0
	jsr	r5,fopen; xbuf
	bcc	1f
	jmp	tfildiag
1:
	rts	r5

getln:
	jsr	r5,getc; xbuf
	bcs	3f
	mov	r0,-(sp)
	jsr	r5,getw; xbuf
	mov	r0,efno
	jsr	r5, getw; xbuf
	mov	r0,ifno
	mov	$line,r1
1:
	jsr	r5,getc; xbuf
	bcs	1f
	tst	r0
	beq	1f
	bic	$200,r0
	movb	r0,(r1)+
	br	1b
1:
	clrb	(r1)+
	mov	(sp)+,r0
	tst	(r5)+
	rts	r5
3:
	mov	xbuf,r0
	sys	close
	rts	r5

