/
/

/ fx6 -- teletype

.globl	tput
.globl	tdecml

tdecml:
	mov	r1,-(sp)
	jsr	r5,td
	mov	(sp)+,r1
	rts	r5

td:
	mov	r0,r1
	clr	r0
	dvd	$10.,r0
	mov	r1,-(sp)
	tst	r0
	beq	1f
	jsr	r5,td
1:
	mov	(sp)+,r0
	add	$'0,r0

tput:
	movb	r0,ch+1
	mov	$1,r0
	sys	write; ch+1; 1
	rts	r5
