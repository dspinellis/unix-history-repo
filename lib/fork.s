/ C library -- fork

/ pid = fork();
/
/ pid == 0 in child process; pid == -1 means error return

	.globl	_fork

.data
_fork:
	1f
.text
1:
	sys	fork
		br 1f
	bes	2f
	rts	pc
2:
	mov	$-1,r0
	rts	pc
1:
	clr	r0
	rts	pc

