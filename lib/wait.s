/ C library -- wait

/ pid = wait();
/   or,
/ pid = wait(&status);
/
/ pid == -1 if error
/ status idicates fate of process, if given

	.globl	_wait

.data
_wait:
	1f
.text
1:
	clr	mq
	sys	wait
	bec	1f
	mov	$-1,r0
	rts	pc
1:
	cmp	*(sp),tstins	/ arg count
	bne	1f
	mov	mq,*2(sp)	/ status return
1:
	rts	pc

tstins:	tst	(sp)+		/ stack pop for 1 arg

