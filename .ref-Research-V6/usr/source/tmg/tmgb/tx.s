i = r3
.globl generate
.globl obuild
.globl .tx,.txs

.txs:
	mov	i,r0
	tst	(i)+
	br	1f
.tx:
	mov	(i)+,r0
1:
	jsr	pc,obuild
	jmp	generate
