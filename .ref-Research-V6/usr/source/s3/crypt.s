/ crypt -- password incoding

/	mov	$key,r0
/	jsr	pc,crypt

.globl	crypt, _crypt
.globl	savr5

_crypt:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	mov	4(r5),r0
	jsr	pc,crypt
	clr	savr5
	mov	(sp)+,r5
	rts	pc

crypt:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	mov	r5,-(sp)

	mov	r0,r1
	mov	$key,r0
	movb	$004,(r0)+
	movb	$034,(r0)+
1:
	cmp	r0,$key+64.
	bhis	1f
	movb	(r1)+,(r0)+
	bne	1b
1:
	dec	r0
/
/
/	fill out key space with clever junk
/
	mov	$key,r1
1:
	movb	-1(r0),r2
	movb	(r1)+,r3
	xor	r3,r2
	movb	r2,(r0)+
	cmp	r0,$key+128.
	blo	1b
/
/
/	establish wheel codes and cage codes
/
	mov	$wheelcode,r4
	mov	$cagecode,r5
	mov	$256.,-(sp)
2:
	clr	r2
	clr	(r4)
	mov	$wheeldiv,r3
3:
	clr	r0
	mov	(sp),r1
	div	(r3)+,r0
	add	r1,r2
	bic	$40,r2
	bis	shift(r2),(r4)
	cmp	r3,$wheeldiv+6.
	bhis	4f
	bis	shift+4(r2),(r5)
4:
	cmp	r3,$wheeldiv+10.
	blo	3b
	sub	$2,(sp)
	tst	(r4)+
	tst	(r5)+
	cmp	r4,$wheelcode+256.
	blo	2b
	tst	(sp)+
/
.data
shift:	1;2;4;10;20;40;100;200;400;1000;2000;4000;10000;20000;40000;100000
	1;2
wheeldiv: 32.; 18.; 10.; 6.; 4.
.bss
cagecode: .=.+256.
wheelcode: .=.+256.
.text
/
/
/	make the internal settings of the machine
/	both the lugs on the 128 cage bars and the lugs
/	on the 16 wheels are set from the expanded key
/
	mov	$key,r0
	mov	$cage,r2
	mov	$wheel,r3
1:
	movb	(r0)+,r1
	bic	$!177,r1
	asl	r1
	mov	cagecode(r1),(r2)+
	mov	wheelcode(r1),(r3)+
	cmp	r0,$key+128.
	blo	1b
/
/
/	now spin the cage against the wheel to produce output.
/
	mov	$word,r4
	mov	$wheel+128.,r3
3:
	mov	-(r3),r2
	mov	$cage,r0
	clr	r5
1:
	bit	r2,(r0)+
	beq	2f
	incb	r5
2:
	cmp	r0,$cage+256.
	blo	1b
/
/	we have a piece of output from current wheel
/	it needs to be folded to remove lingering hopes of
/	inverting the function
/
	mov	r4,-(sp)
	clr	r4
	div	$26.+26.+10.,r4
	add	$'0,r5
	cmp	r5,$'9
	blos	1f
	add	$'A-'9-1,r5
	cmp	r5,$'Z
	blos	1f
	add	$'a-'Z-1,r5
1:
	mov	(sp)+,r4
	movb	r5,(r4)+
	cmp	r4,$word+8.
	blo	3b
/

	mov	(sp)+,r5
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	$word,r0
	rts	pc
.bss
key:	.=.+128.
word:	.=.+32.
cage:	.=.+256.
wheel:	.=.+256.
