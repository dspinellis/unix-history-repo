/
/

/ fx7 -- passes advancement

.globl	signon
.globl	signoff


.globl	pass2
.globl	pass3
.globl	pass4

signon:
	tst	(r5)+
	rts	r5

signoff:
	mov	(r5)+,r0
	tst	(sp)+
	dec	r0
	asl	r0
	jmp	*passtab(r0)

passtab:
	pass2
	pass3
	pass4

.data




