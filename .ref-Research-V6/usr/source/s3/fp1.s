/ fp1 -- floating point simulator

rti	= 2
bpt	= 3

m.ext = 200		/ long mode bit
m.lngi = 100		/ long integer mode

.globl	fptrap
.globl	ac0, ac1, ac2, ac3

fptrap:
	dec	reenter
	bge	1f
	4		/ reentered!
1:
	mov	(sp)+,spc
	mov	(sp)+,sps
	mov	r0,sr0
	mov	$sr1,r0
	mov	r1,(r0)+
	mov	r2,(r0)+
	mov	r3,(r0)+
	mov	r4,(r0)+
	mov	r5,(r0)+
	mov	sp,(r0)+
	sub	$8,sp		/ room for double push
	mov	(r0),r5		/ pc
	clr	trapins
	mov	-(r5),r5	/ trapped instruction
	mov	r5,r4
	bic	$7777,r4
	cmp	r4,$170000
	beq	1f
	jmp	badins
1:
	bic	$100000,fpsr	/ clear fp error
	bic	$170000,r5
	mov	r5,r4
	bit	$7000,r4
	bne	class3
	bit	$700,r4
	bne	class2
	cmp	r4,$12
	blos	1f
	jmp	badins
1:
	asl	r4
	jmp	*1f(r4)

	.data
1:
	i.cfcc		/ 170000
	i.setf		/ 170001
	i.seti		/ 170002
	badins
	badins
	badins
	badins
	badins
	badins
	i.setd		/ 170011
	i.setl		/ 170012
	.text

class2:
	cmp	r5,$400
	bge	1f
	jsr	r1,fsrc; mod0rx; mod242
	br	2f
1:
	jsr	r1,fsrc; mod0f; mod24f
2:
	mov	r3,r5
	asl	r4
	asl	r4
	clrb	r4
	swab	r4
	asl	r4
	jsr	pc,*1f(r4)
	jmp	sret

	.data
1:
	badins		/ 1700xx
	i.ldfps		/ 1701xx
	i.stfps		/ 1702xx
	badins		/ 1703xx - stst
	i.clrx		/ 1704xx
	i.tstx		/ 1705xx
	i.absx		/ 1706xx
	i.negx		/ 1707xx
	.text

class3:
	cmp	r5,$5000
	blt	1f
	mov	r5,r2
	clrb	r2
	cmp	r2,$6400
	blt	2f
	sub	$1400,r2
2:
	cmp	r2,$5000
	bne	2f
	jsr	r1,fsrc; mod0rx; mod242
	br	3f
2:
	cmp	r2,$5400
	bne	2f
	jsr	r1,fsrc; mod0ra; mod24i
	br	3f
2:
	jsr	r1,fsrc; mod0f; mod24d
	br	3f
1:
	jsr	r1,fsrc; mod0f; mod24f
3:
	jsr	pc,freg
	mov	r2,r5
	clrb	r4
	swab	r4
	asl	r4
	jsr	pc,*1f(r4)
	br	sret

	.data
1:
	badins		/ 1700xx
	badins		/ 1704xx
	i.mulx		/ 1710xx
	i.modx		/ 1714xx
	i.addx		/ 1720xx
	i.ldx		/ 1724xx
	i.subx		/ 1730xx
	i.cmpx		/ 1734xx
	i.stx		/ 1740xx
	i.divx		/ 1744xx
	i.stexp		/ 1750xx
	i.stcxj		/ 1754xx
	i.stcxy		/ 1760xx
	i.ldexp		/ 1764xx
	i.ldcjx		/ 1770xx
	i.ldcyx		/ 1774xx
	.text

i.cfcc:
	mov	fpsr,r0
	bic	$!17,r0
	mov	r0,sps
	br	ret

i.setf:
	bic	$m.ext,fpsr
	br	ret

i.setd:
	bis	$m.ext,fpsr
	br	ret

i.seti:
	bic	$m.lngi,fpsr
	br	ret

i.setl:
	bis	$m.lngi,fpsr
	br	ret

badins:
	inc	trapins
	br	ret

sret:
	mov	$fpsr,r0
	bic	$17,(r0)
	tst	(r5)
	bpl	1f
	bis	$10,(r0)
	br	ret
1:
	bne	ret
	bis	$4,(r0)

ret:
	mov	$sr1,r0
	mov	(r0)+,r1
	mov	(r0)+,r2
	mov	(r0)+,r3
	mov	(r0)+,r4
	mov	(r0)+,r5
	mov	(r0)+,sp
	mov	sr0,r0
	mov	sps,-(sp)
	mov	spc,-(sp)
	tst	trapins
	bne	1f
	inc	reenter
	rti
1:
	bpt

freg:
	mov	r5,r2
	bic	$!300,r2
	asr	r2
	asr	r2
	asr	r2
	add	$ac0,r2
	rts	pc

fsrc:
	mov	r5,r3
	bic	$!7,r3			/ register
	asl	r3
	add	$sr0,r3
	mov	r5,r0
	bic	$!70,r0			/ mode
	asr	r0
	asr	r0
	jmp	*1f(r0)

	.data
1:
	mod0
	mod1
	mod2
	mod3
	mod4
	mod5
	mod6
	mod7
	.text

mod24f:
	mov	$4,r0
	bit	$m.ext,fpsr
	beq	1f
	add	$4,r0
1:
	rts	pc

mod24d:
	mov	$8,r0
	bit	$m.ext,fpsr
	beq	1f
	sub	$4,r0
1:
	rts	pc

mod242:
	mov	$2,r0
	rts	pc

mod24i:
	mov	$2,r0
	bit	$m.lngi,fpsr
	beq	1f
	add	$2,r0
1:
	rts	pc

mod0:
	jmp	*(r1)+

mod0f:
	sub	$sr0,r3			/ get fp ac
	cmp	r3,$6*2
	bhis	badi1
	asl	r3
	asl	r3
	add	$ac0,r3
	tst	(r1)+
	rts	r1

mod0ra:
	bit	$m.lngi,fpsr
	bne	badi1

mod0r:
	cmp	r3,$ssp
	bhis	badi1
mod0rx:
	tst	(r1)+
	rts	r1

mod1:
	cmp	r3,$spc
	beq	badi1
	mov	(r3),r3
	br	check

mod2:
	mov	(r3),-(sp)
	jsr	pc,*2(r1)
	cmp	r3,$spc
	bne	1f
	mov	$2,r0
	mov	*(r3),pctmp
	mov	$pctmp,(sp)
1:
	add	r0,(r3)
	mov	(sp)+,r3
	br	check

mod3:
	mov	*(r3),-(sp)
	add	$2,(r3)
	mov	(sp)+,r3
	br	check

mod4:
	cmp	r3,$spc		/ test pc
	beq	badi1
	jsr	pc,*2(r1)
	sub	r0,(r3)
	mov	(r3),r3
	br	check

mod5:
	cmp	r3,$spc
	beq	badi1
	sub	$2,(r3)
	mov	*(r3),r3
	br	check

mod6:
	mov	*spc,-(sp)
	add	$2,spc
	add	(r3),(sp)
	mov	(sp)+,r3
	br	check

mod7:
	jsr	r1,mod6; ..; ..
	mov	(r3),r3
	br	check

badi1:
	jmp	badins

check:
	bit	$1,r3
	bne	badi1
	cmp	(r1)+,(r1)+
	rts	r1

setab:
	mov	$asign,r0
	jsr	pc,seta
	mov	r3,r2
	mov	$bsign,r0

seta:
	clr	(r0)
	mov	(r2)+,r1
	mov	r1,-(sp)
	beq	1f
	blt	2f
	inc	(r0)+
	br	3f
2:
	dec	(r0)+
3:
	bic	$!177,r1
	bis	$200,r1
	br	2f
1:
	clr	(r0)+
2:
	mov	r1,(r0)+
	mov	(r2)+,(r0)+
	bit	$m.ext,fpsr
	beq	2f
	mov	(r2)+,(r0)+
	mov	(r2)+,(r0)+
	br	3f
2:
	clr	(r0)+
	clr	(r0)+
3:
	mov	(sp)+,r1
	asl	r1
	clrb	r1
	swab	r1
	sub	$200,r1
	mov	r1,(r0)+	/ exp
	rts	pc

norm:
	mov	$areg,r0
	mov	(r0)+,r1
	mov	r1,-(sp)
	mov	(r0)+,r2
	bis	r2,(sp)
	mov	(r0)+,r3
	bis	r3,(sp)
	mov	(r0)+,r4
	bis	r4,(sp)+
	bne	1f
	clr	asign
	rts	pc
1:
	bit	$!377,r1
	beq	1f
	clc
	ror	r1
	ror	r2
	ror	r3
	ror	r4
	inc	(r0)
	br	1b
1:
	bit	$200,r1
	bne	1f
	asl	r4
	rol	r3
	rol	r2
	rol	r1
	dec	(r0)
	br	1b
1:
	mov	r4,-(r0)
	mov	r3,-(r0)
	mov	r2,-(r0)
	mov	r1,-(r0)
	rts	pc

