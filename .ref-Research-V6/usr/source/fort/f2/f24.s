/
/

/ f24 -- allocate storage for non-common variables
/ called after common and equivalence have been done

.globl	salloc

.globl	eqvtab
.globl	error
.globl	declimpl
.globl	size
.globl	perror

/ destroys all registers

salloc:
	mov	r5,-(sp)
	clr	r3			/ loop over symbol table
	br	2f
1:
	add	$8.,r3			/ next variable
2:

	cmp	r3,symtp
	blo	2f
	mov	(sp)+,r5
	mov	$line,r1
	jsr	r5,perror		/ flush errors
	rts	r5
2:
	bit	$70,symtab(r3)
	beq	1b			/ unclassed
	jsr	r5,declimpl		/ just in case
	tst	eqvtab(r3)		/ test for already allocated
	bne	1b			/ yes
	mov	symtab(r3),r0
	bic	$!70,r0
	cmp	r0,$10			/ test class=simple
	beq	2f
	cmp	r0,$20			/ test array
	bne	1b			/ no, not a variable
2:
	bit	$200,symtab(r3)		/ test parameter
	bne	1b
	tst	eqvtab+2(r3)		/ test for equivalence
	bne	2f			/ yes
	bit	$100,symtab(r3)		/ test common
	bne	1b			/ yes, nothing to do
	mov	nxtaloc,symtab+6(r3)	/ offset
	jsr	r5,size			/ get byte count
	add	r0,nxtaloc
	inc	eqvtab(r3)		/ mark allocated
	br	1b
2:
	clr	r4			/ common variable of group
	mov	$77777,r1		/ infinity to smallest offset
	mov	r3,r5
2:
	cmp	eqvtab+4(r3),r1
	bgt	3f
	mov	eqvtab+4(r3),r1		/ replace smallest offset
3:
	bit	$100,symtab(r3)		/ test common
	beq	3f
	mov	r3,r4			/ yes
3:
	mov	eqvtab+2(r3),r3		/ next group member
	cmp	r3,r5
	bne	2b
	tst	r4
	bne	2f			/ *there was a common in group
				/ equivalence group w/o common
	sub	nxtaloc,r1		/ get -(group offset)
3:
	inc	eqvtab(r3)		/ mark allocated
	mov	eqvtab+4(r3),r2
	sub	r1,r2			/ compute offset
	mov	r2,symtab+6(r3)		/ enter offset
	jsr	r5,size
	add	r0,r2			/ highest loc of variable
	cmp	r2,r4
	ble	4f
	mov	r2,r4			/ extends storage
4:
	mov	eqvtab+2(r3),r3		/ next of group
	cmp	r3,r5
	bne	3b
	mov	r4,nxtaloc		/ account for space
	br	1b			/ done!
2:				/ equivalence group w/ common
	mov	symtab+6(r4),r1		/ actual common offset
	sub	eqvtab+4(r4),r1		/ virtual common offset
2:
	inc	eqvtab(r3)		/ mark allocated
	bit	$100,symtab(r3)		/ is variable already in common
	beq	3f			/ *no
	cmp	symtab+4(r4),symtab+4(r3)
	beq	4f
	jsr	r5,error; 25.		/ different blocks equiv.
4:
	mov	r1,r0
	add	eqvtab+4(r3),r0
	cmp	r0,symtab+6(r3)
	beq	4f			/ ok
	jsr	r5,error; 27.		/ same variable, different offsets
	br	4f
3:
	bis	$100,symtab(r3)		/ mark common now
	mov	symtab+4(r4),symtab+4(r3)/ get right common block
	mov	r1,r0
	add	eqvtab+4(r3),r0
	bge	3f
	jsr	r5,error; 26.		/ block extended leftward
	clr	r0
3:
	mov	r0,symtab+6(r3)		/ get proper offset
	mov	r0,-(sp)
	jsr	r5,size			/ see if size is extended
	add	(sp)+,r0
	mov	symtab+4(r3),r2		/ common block
	cmp	symtab+6(r2),r0
	bge	4f			/ ok
	mov	r0,symtab+6(r2)		/ extend size
4:
	mov	eqvtab+2(r3),r3
	cmp	r3,r5
	bne	2b
	jmp	1b

