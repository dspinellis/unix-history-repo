f = r5
g = r4
.globl k
.globl errcom
.globl ktab,ktat
.globl putcall,kput

putcall:
	jsr	pc,kput
	mov k(f),(g)+
	rts	pc

kput:
	sub	$2,k(f)
	mov	k(f),r1
	neg	r1
	mov	r0,ktab(r1)
	cmp	r1,$ktat
	bhis	1f
	rts	pc
1:
	jsr	r0,errcom
		<translation overflow\0>;.even
