/ lpd -- Line Printer daemon

.globl	fopen, getc
.globl	fcreat, putc, flush
.globl	switch

	sys	signal; 1; 1
	sys	signal; 2; 1
	sys	signal; 3; 1	/ ignore quit,intr,hup
	sys	stat; lock; buf
	bec	2f
	sys	creat; lock; 0
	bes	2f
	sys	open; lpd; 0
	bes	3f
	mov	r0,r1
1:
	mov	r1,r0
	sys	read; dbuf; 16.
	bes	3f
	tst	r0
	beq	3f
	tst	dbuf
	beq	1b
	cmp	dbuf+2,$"df
	bne	1b
	sys	fork
		br  retry
	sys	exit
3:
	sys	unlink; lock
2:
	sys	exit

/ get line printer
retry:
	jsr	r5,aclose
	mov	$lpr,r0
	jsr	r5,fcreat; print
	bes	error
	sys	chdir; lpd
	bes	error
	sys	open; lpd; 0
	bes	error
	mov	r0,dfi
	br	loop

done:
	sys	unlink; lock
	sys	exit

error:
	jsr	r5,aclose
	mov	$10.,r0
	sys	sleep
	sys	stat; lock; buf
	bes	done			/ die if lock is gone
	br	retry

/ look in directory for work
loop:
	mov	dfi,r0
	sys	seek; 0; 0

1:
	mov	dfi,r0
	sys	read; dbuf; 16.
	bes	error
	tst	r0
	beq	done		/ only non-error return
	tst	dbuf
	beq	1b
	mov	$dbuf+2,r0
	cmp	(r0),$"df		/ look for daemon file
	bne	1b

/ found prototype file
	jsr	r5,fopen; proto
	bes	1b

/ copy a line into buf
/ only work is expansion of tabs
loop1:
	mov	$buf,r1
1:
	jsr	r5,getc; proto
	bes	eloop1
	movb	r0,(r1)
	cmp	r0,$'\t
	beq	2f
	cmpb	(r1)+,$'\n
	bne	1b
	br	1f
2:
	movb	$' ,(r1)+
	cmp	r1,$buf+8.
	beq	1b
	cmp	r1,$buf+16.
	bhis	1b
	br	2b
1:
	movb	buf,r0
	jsr	r5,switch; sptab

/ done with a prototype file
/ look for U's in second pass
eloop1:
	mov	proto,r0
	sys	seek; 0; 0
	br	2f
1:
	jsr	r5,getc; proto
	bes	1f
3:
	cmp	r0,$'\n
	bne	1b
2:
	jsr	r5,getc; proto
	bes	1f
	cmp	r0,$'U
	bne	3b
	mov	$buf,r1
3:
	jsr	r5,getc; proto
	bes	1f
	movb	r0,(r1)+
	cmp	r0,$'\n
	bne	3b
	clrb	-(r1)
	sys	unlink; buf
	br	2b
1:
	mov	proto,r0
	sys	close
	sys	unlink; dbuf+2
	br	loop

/ list of special characters
/ switchout
sptab:
	'L; literal
	'B; binary
	'F; form
	'U; loop1		/ unlink on second pass
	 0; 0


literal:
	jmp	loop1

form:
	clrb	-(r1)
	mov	$buf+1,r0
	jsr	r5,fopen; insert
	bes	loop1
	mov	$14,r0
	jsr	r5,putc; print
	br	1f

binary:
	clrb	-(r1)
	mov	$buf+1,r0
	jsr	r5,fopen; insert
	bes	loop1

1:
	jsr	r5,getc; insert
	bes	1f
	jsr	r5,putc; print
	br	1b
1:
	jsr	r5,flush; print
	mov	insert,r0
	sys	close
	br	loop1

aclose:
	mov	$9.,r1
1:
	mov	r1,r0
	sys	close
	dec	r1
	bge	1b
	rts	r5

lpr:
	</dev/lp\0>
lock:
	</usr/lpd/lock\0>
lpd:
	</usr/lpd\0>
	.even
.bss

ch:	.=.+2
dfi:	.=.+2
dbuf:	.=.+18.
print:	.=.+518.
proto:	.=.+518.
insert:	.=.+518.
buf:	.=.+400.
sleep = 35.
signal = 48.
