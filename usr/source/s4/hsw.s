/ C library -- latest version of hash switch
.globl	hsw

hsw:
	mov	r0,r1
	clr	r0
	div	(r2)+,r0
	asl	r1
	add	(r2)+,r1
	mov	r0,*(r1)+
	mov	(r1)+,r1
1:
	cmp	r0,-(r1)
	bne	1b
	rts	r2

/	jsr	r2,nhswitch; magicdiv; tbase
/	jmp	X-L0(r1)
/
/.data
/tbase:
/	L0
/	L1
/	L2
/	X
/
/L0:	..
/	V00
/	V01
/	V02
/	V03
/L1:	..
/	V10
/	V11
/L2:	..
/	V20
/	V21
/	V22
/L3:	..
/X:
/	L00
/	L01
/	L02
/	L03
/	Ldef
/	L10
/	L11
/	Ldef
/	L20
/	L21
/	L22
/	Ldef
