/
/

/ rd -- short list I/O do loop

.globl	slist1
.globl	slist2
.globl	slist3

.globl	temp

slist1:
	mov	(r4)+,r0
	mov	(r0)+,r2
	mov	$1,r1
1:
	mpy	(r0)+,r1
	sob	r2,1b
	mov	$temp,r2
	mov	r1,(r2)+
	clr	(r2)+
	mov	(r0)+,(r2)+
	jmp	*(r4)+

slist2:
	mov	(r4)+,r0
	dec	temp
	ble	1f
	mov	r0,r4
1:
	jmp	*(r4)+

slist3:
	mov	$temp+2,r0
	add	(r0)+,(sp)
	add	(r0),-(r0)
	jmp	*(r4)+

