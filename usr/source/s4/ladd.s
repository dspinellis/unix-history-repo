/ C routine-- long integer subtract and add

/ ladd/lsub (a, b, c);
/	int a[2], b[2], c[2];
/	a = b +- c;

.globl	_lsub
.globl	_ladd
.globl	csv
.globl	cret

_lsub:
	jsr	r5,csv
	mov	6(r5),r2
	mov	(r2)+,r0
	mov	(r2),r1
	mov	10(r5),r2
	sub	(r2)+,r0
	sub	(r2),r1
	sbc	r0
	mov	4(r5),r2
	mov	r0,(r2)+
	mov	r1,(r2)
	jmp	cret

_ladd:
	jsr	r5,csv
	mov	6(r5),r2
	mov	(r2)+,r0
	mov	(r2),r1
	mov	10(r5),r2
	add	(r2)+,r0
	add	(r2),r1
	adc	r0
	mov	4(r5),r2
	mov	r0,(r2)+
	mov	r1,(r2)
	jmp	cret
