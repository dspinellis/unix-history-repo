f = r5
i = r3
.globl ek,ep,ek.fs,ep.fs,si,x,fs
.globl generate
.globl .tq,gpar

.tq:
	mov	(i)+,r0
	mov	i,si(f)
	mov	ep(f),r1
	mov	ep(r1),ep.fs(f)
	mov	ek(r1),ek.fs(f)
	mov	si(r1),r1
	asl	r0
	sub	r0,r1
	mov	(r1),i
	add	$fs,f
	clr	x(f)
	jsr	pc,generate
	jmp	generate

gpar:
	mov	(i)+,r0
	mov	ep(f),r1
	asl	r0
	add	r0,si(r1)
	jmp	generate
.globl gq;gq=.tq
