/ list on kl-11 followon ascii file

	mov	$ascii,r1
1:
	movb	(r1)+,r0
	beq	1f
	jsr	pc,(r5)
	br	1b
1:
	rts	pc

ascii:
	<dldr\n>
	<dtf\n>
	<list\n>
	<mboot\n>
	<mcopy\n>
	<rkf\n>
	<tboot\n>
	<uboot\n>
	<\0>
