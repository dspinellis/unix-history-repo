	sys	open; aout; 0
	bes	error
	mov	r0,r1
	sys	creat; u; 666
	bes	error
	mov	r0,r2
	mov	r1,r0
	sys	read; buf; 020
	mov	buf+2,r3
	add	buf+4,r3
1:
	tst	r3
	beq	done
	cmp	r3,$512.
	bhis	2f
	mov	r3,0f
2:
	mov	r1,r0
	sys	read; buf; 0:512.
	cmp	r0,0b
	bne	error
	sub	r0,r3
	mov	r0,0f
	mov	r2,r0
	sys	write; buf; 0:..
	br	1b

error:
	mov	$1,r0
	sys	write; msg; emsg-msg

done:
	sys	exit

msg:
	<error\n>
emsg:
aout:	<a.out\0>
u:	<u\0>
.bss
buf:	.=.+512.
