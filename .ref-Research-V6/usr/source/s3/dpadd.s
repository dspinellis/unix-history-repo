/ C library -- signed dp add

/ dpadd(l, i)
/	int l[2];
/ l =+ i;

.globl	_dpadd

_dpadd:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	tst	6(r5)
	sxt	r1
	add	r1,(r0)+
	add	6(r5),(r0)
	adc	-(r0)
	mov	(sp)+,r5
	rts	pc
