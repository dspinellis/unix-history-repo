/
/

/ fx2 -- error setting and printing

.globl	error
.globl	perror

.globl	tput
.globl	tdecml

error:
	inc	nerror
	mov	r0,-(sp)
	mov	r2,-(sp)
	mov	(r5)+,r0
	mov	$errb,r2
1:
	cmp	r2,errp
	bhis	1f
	cmp	r0,(r2)+
	beq	2f
	tst	(r2)+
	br	1b
1:
	cmp	r2,$eerrb
	bhis	2f
	mov	r0,(r2)+
	mov	r1,(r2)
	dec	(r2)+		/ r1 is often 1 too far
	mov	r2,errp
2:
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	r5

perror:
	cmp	errp,$errb
	beq	1f
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	ifno,r0
	jsr	r5,tdecml
	mov	$'\t,r0
	jsr	r5,tput
	mov	$line,r1
2:
	movb	(r1),r0
	beq	2f
	jsr	r5,tput
	mov	$errb+2,r2
3:
	cmp	r1,(r2)+
	bne	4f
	mov	$1,r0
	sys	write; mes2; 2	/ bs, _
	br	3f
4:
	cmp	r2,errp
	blo	3b
3:
	inc	r1
	br	2b
2:
	mov	$errb,r1
2:
	mov	$1,r0
	sys	write; mes1; 4
3:
	sys	open; errfil; 0
	bec	3f
	mov	(r1)+,r0
	tst	(r1)+
	jsr	r5,tdecml
	cmp	r1,errp
	bhis	2f
	mov	$',,r0
	jsr	r5,tput
	br	3b
3:
	mov	r0,-(sp)
	mov	(r1)+,r2
	tst	(r1)+
3:
	dec	r2
	ble	3f
4:
	mov	(sp),r0
	sys	read; ich; 1
	bes	4f
	tst	r0
	beq	4f
	cmpb	ich,$'\n
	bne	4b
	br	3b
3:
	mov	(sp),r0
	sys	read; ich; 1
	bes	4f
	tst	r0
	beq	4f
	movb	ich,r0
	cmp	r0,$'\n
	beq	3f
	jsr	r5,tput
	br	3b
4:
	mov	$1,r0
	sys	write; mes3; 18.
3:
	mov	(sp)+,r0
	sys	close
	cmp	r1,$errp
	blo	2b
2:
	mov	$'\n,r0
	jsr	r5,tput
	mov	$errb,errp
	mov	(sp)+,r2
	mov	(sp)+,r1
1:
	rts	r5

mes1:
	<\n** >
mes2:
	.byte 010, '_
mes3:
	<Unknown diagnostic>
errfil:
	</usr/fort/errors\0>
.even
.bss
ich:	.=.+2
