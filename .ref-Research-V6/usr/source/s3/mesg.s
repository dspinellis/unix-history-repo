.globl	mesg

/ usage:
/   jsr r5,mesg
/       <string ending in \0>
/      .even
/   ...
/
/ string is output onto $1
/

mesg:
	mov	r0,-(sp)
	mov	r5,r0
	mov	r5,0f
1:
	tstb	(r5)+
	bne	1b
	sub	r5,r0
	com	r0
	mov	r0,0f+2
	mov	$1,r0
	sys	0; 9f
.data
9:
	sys	write; 0:..; ..
.text
	inc	r5
	bic	$1,r5
	mov	(sp)+,r0
	rts	r5

