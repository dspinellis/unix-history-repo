/
/

/ rh -- simple array l/r values

achk	= 1 / 1 to check array bounds
.globl	gal1
.globl	gal2
.globl	gal4
.globl	gal8
.globl	gal16
.globl	gar1
.globl	gar2
.globl	gar4
.globl	gar8
.globl	gar16

.globl	rerr

gal1:
	tst	(sp)+
	dec	(sp)
	br	1f

gal2:
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	br	1f

gal4:
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	asl	(sp)
	br	1f

gal8:
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	asl	(sp)
	asl	(sp)
	br	1f

gal16:
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	asl	(sp)
	asl	(sp)
	asl	(sp)

1:
	cmp	(sp),(r4)+
.if achk
	bhis	9f
.endif
	add	(r4)+,(sp)
	jmp	*(r4)+

9:
	jsr	r5,rerr; 15.
	4

gar1:
	tst	(sp)+
	dec	(sp)
	cmp	(sp),(r4)+
.if achk
	bhis	9b
.endif
	add	(r4)+,(sp)
	movb	*(sp)+,r0
	mov	r0,-(sp)
	jmp	*(r4)+

gar2:
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	cmp	(sp),(r4)+
.if achk
	bhis	9b
.endif
	add	(r4)+,(sp)
	mov	*(sp)+,-(sp)
	jmp	*(r4)+

gar4:
	tst	(sp)+
	dec	(sp)
	mov	(sp)+,r0
	asl	r0
	asl	r0
	cmp	r0,(r4)+
.if achk
	bhis	9b
.endif
	add	(r4)+,r0
	cmp	(r0)+,(r0)+
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	jmp	*(r4)+

gar8:
	setd
	tst	(sp)+
	dec	(sp)
	asl	(sp)
	asl	(sp)
	asl	(sp)
	cmp	(sp),(r4)+
.if achk
	bhis	9b
.endif
	add	(r4)+,(sp)
	movf	*(sp)+,fr0
	movf	fr0,-(sp)
	jmp	*(r4)+

gar16:
	setd
	tst	(sp)+
	dec	(sp)
	mov	(sp)+,r0
	asl	r0
	asl	r0
	asl	r0
	asl	r0
	cmp	r0,(r4)+
.if achk
	bhis	9b
.endif
	add	(r4)+,r0
	movf	(r0)+,fr0
	movf	(r0)+,fr1
	movf	fr1,-(sp)
	movf	fr0,-(sp)
	jmp	*(r4)+

