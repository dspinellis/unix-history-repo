/
/

/ f16 -- extrn, equiv, data

.globl	sextr
.globl	sequi

.globl	getsym
.globl	putc
.globl	error
.globl	ptemp

sextr:
	jsr	r5,getsym
	tst	r0
	bne	1f
	bit	$70,symtab(r3)
	beq	2f
	jsr	r5,error; 14.
2:
	bis	$30,symtab(r3)
	jsr	r5,getsym
	cmp	r0,$36.		/ ,
	beq	sextr
	cmp	r0,$40.		/ eos
	beq	2f
1:
	jsr	r5,error; 15.
2:
	rts	r5

sequi:
	jsr	r5,ptemp; 'e; efno; line
	rts	r5


