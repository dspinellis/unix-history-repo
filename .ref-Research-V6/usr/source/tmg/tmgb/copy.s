f = r5
i = r3
.globl x,k
.globl ktab,ktat
.globl putcall,obuild
.globl errcom,succ,generate
.globl rewcstr,getcstr
.globl scopy

scopy:
	mov	$2f+1,r0
	jsr	pc,putcall
	jsr	pc,rewcstr
	mov	k(f),r2
	neg	r2
	add	$2,r2
1:
	jsr	pc,getcstr
	tst	r0
	beq	1f
	movb	r0,ktab(r2)
	inc	r2
	cmp	r2,$ktat
	blt	1b
	jsr	r0,errcom
	<translation overflow\0>;.even
1:
	clrb	ktab(r2)
	bic	$1,r2
	neg	r2
	mov	r2,k(f)
	jmp	succ
2:
	mov	i,r0
	jsr	pc,obuild
	jmp	generate
