/
/

/ f43 -- output globals and bdata bss's

.globl	globls

.globl	code
.globl	dattab
.globl	onedata
.globl	error
.globl	perror

globls:
	mov	progt,r0
	jmp	*1f(r0)
1:
	1f
	2f
	3f
	4f
1:
	jsr	r5,code
		<.globl	main\n\0>; .even
	rts	r5
2:
3:
	jsr	r5,code
		<.globl	%n.\n\0>; .even
		8
	rts	r5
4:
1:
	clr	r3
1:
	cmp	r3,symtp
	blo	2f
	rts	r5
2:
	mov	symtab(r3),r0
	bic	$!270,r0
	cmp	r0,$40			/ common block
	bne	2f
	mov	symtab+6(r3),r2
	beq	2f
	mov	$dattab,r1
3:
	cmp	r1,r4
	bhis	3f
	cmp	(r1),r3
	beq	4f
	add	$8.,r1
	br	3b
3:
	jsr	r5,code
		<.bss\n.globl	%n\n%n:	.=.+%d.\n.text\n\0>; .even
		r3
		r3
		r2
	br	2f
4:
	jsr	r5,code
		<.data\n.globl	%n\n%n:\n\0>; .even
		r3
		r3
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r1,r3
	jsr	r5,onedata
	mov	(sp)+,r3
	mov	(sp)+,r2
	sub	r1,r2
	bge	9f
	jsr	r5,error; 22.		/ data overrun
	jsr	r5,perror
9:
	jsr	r5,code
		<.=.+%d.\n.text\n\0>; .even
	r2
2:
	add	$8,r3
	br	1b

