/ switch -- switch on contents of r0
/
/
/ calling sequence --
/
/   jsr r5,switch; swtab
/
/ r0 is looked up in swtab and if found
/ control is returned to the corresponding place
/ if r0 is not found, the next inline instruction is 
/ executed
/
/ swtab format --
/
/ swtab:
/    val1; ret1
/   val2; ret2
/   ...
/   valn; retn
/   ..; 0
/

	.globl	switch
switch:
	mov	r1,-(sp)
	mov	(r5)+,r1
1:
	cmp	r0,(r1)+
	beq	1f
	tst	(r1)+
	bne	1b
2:
	mov	(sp)+,r1
	rts	r5
1:
	mov	(r1)+,r1
	beq	2b
	mov	r1,r5
	br	2b

