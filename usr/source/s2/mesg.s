/ mesg -- set current tty to accept or forbid messages

.globl	ttyn

	clr	r0
	jsr	pc,ttyn
	movb	r0,ttyno
	sys	stat; ttyx; stbuf
	bes	error
	cmp	(sp)+,$1
	beq	flip
	tst	(sp)+
	mov	(sp)+,r0
	cmpb	(r0),$'n
	beq	setno
	br	setyes

flip:
	bit	$2,stbuf+4
	beq	setyes

setno:
	sys	chmod; ttyx; 600
	bes	error
	br	say

setyes:
	sys	chmod; ttyx; 622
	bes	error

say:
	bit	$2,stbuf+4
	beq	wasno

wasyes:
	movb	$'y,nmes+4

wasno:
	mov	$1,r0
	sys	write; nmes; 6
	sys	exit

nmes:
	<was n\n>
error:
	mov	$1,r0
	sys	write; 1f; 2
	sys	exit
1:	<?\n>

ttyx:	</dev/tty0\0>
ttyno = .-2
	.even

stbuf:	.=.+40.
