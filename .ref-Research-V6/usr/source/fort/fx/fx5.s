/
/

/ fx5 -- declare implicit

.globl	declimpl
.globl	getname

declimpl:
	bit	$7,symtab(r3)
	bne	1f		/ already declared
	jsr	r5,getname
	movb	symbuf,r0
	cmp	r0,$'a
	blo	2f
	sub	$6,r0		/ map 'a -> 'Z+1
2:
	asl	r0
	bis	imptab-[2*'A](r0),symtab(r3)
1:
	bit	$70,symtab(r3)		/ class
	bne	1f
	bis	$10,symtab(r3)		/ simple
1:
	rts	r5

getname:
	mov	r3,-(sp)
	clr	r3
	mov	$namebuf,r0
1:
	cmp	r3,(sp)
	bhis	1f
2:
	tstb	(r0)+
	bne	2b
	add	$8,r3
	br	1b
1:
	mov	$symbuf,r3
2:
	movb	(r0)+,(r3)+
	bne	2b
	mov	(sp)+,r3
	rts	r5

