/ C library-- fake floating output

.globl	pfloat
.globl	pscien
.globl	pgen

pfloat:
pscien:
pgen:
	add	$8,r4
	movb	$'?,(r3)+
	rts	pc
