/
/

/ rg -- assignments

.globl	gmv1
.globl	gmv2
.globl	gmv4
.globl	gmv8
.globl	gmv16
.globl	stst

gmv1:
	movb	(sp)+,*(r4)+
	jmp	*(r4)+

gmv2:
stst:
	mov	(sp)+,*(r4)+
	jmp	*(r4)+

gmv4:
	mov	(r4)+,r0
	mov	(sp)+,(r0)+
	mov	(sp)+,(r0)+
	jmp	*(r4)+

gmv8:
	setd
	movf	(sp)+,fr0
	movf	fr0,*(r4)+
	jmp	*(r4)+

gmv16:
	setd
	mov	(r4)+,r0
	movf	(sp)+,fr0
	movf	fr0,(r0)+
	movf	(sp)+,fr0
	movf	fr0,(r0)+
	jmp	*(r4)+

.globl	gmv1p
.globl	gmv2p
.globl	gmv4p
.globl	gmv8p
.globl	gmv16p

gmv1p:
	mov	(r4)+,r0
	add	r3,r0
	movb	(sp)+,*(r0)+
	jmp	*(r4)+

gmv2p:
	mov	(r4)+,r0
	add	r3,r0
	mov	(sp)+,*(r0)+
	jmp	*(r4)+

gmv4p:
	mov	(r4)+,r0
	add	r3,r0
	mov	(r0),r0
	mov	(sp)+,(r0)+
	mov	(sp)+,(r0)+
	jmp	*(r4)+

gmv8p:
	setd
	mov	(r4)+,r0
	add	r3,r0
	movf	(sp)+,fr0
	movf	fr0,*(r0)+
	jmp	*(r4)+

gmv16p:
	setd
	mov	(r4)+,r0
	add	r3,r0
	mov	(r0),r0
	movf	(sp)+,fr0
	movf	fr0,(r0)+
	movf	(sp)+,fr0
	movf	fr0,(r0)+
	jmp	*(r4)+

