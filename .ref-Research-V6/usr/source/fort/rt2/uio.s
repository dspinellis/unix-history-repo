/ fortran to UNIX IO
/
.globl	read.
.globl	write.
.globl	seek.
.globl	seek0.
.globl	open.
.globl	creat.
.globl	close.
.globl	errno.

indir	= 0

/	integer function read(f, a, n)
/	integer f, n
/	logical*1 a(n)
/		same as C

read.:
	temp
	rval4p; 2	/ f
	i4i2
	lvalp; 4	/ a
	rval4p; 6	/ n
	i4i2
	.+2
	mov	(sp)+,9f+4
	mov	(sp)+,9f+2
	mov	(sp)+,r0
	sys	indir; 9f
	br	ret

.data
9:	sys	read; ..; ..
.text

/	integer function read(f, a, n)
/	integer f, n
/	logical*1 a(n)
/		same as C

write.:
	temp
	rval4p; 2	/ f
	i4i2
	lvalp; 4	/ a
	rval4p; 6	/ n
	i4i2
	.+2
	mov	(sp)+,9f+4
	mov	(sp)+,9f+2
	mov	(sp)+,r0
	sys	indir; 9f
	br	ret

.data
9:	sys	write; ..; ..
.text

/	integer function seek(f, o, b)
/	integer f, o, b
/		same as C

seek.:
	temp
	rval4p; 2	/ f
	i4i2
	rval4p; 4	/ o
	i4i2
	rval4p; 6	/ b
	i4i2
	.+2
	mov	(sp)+,9f+4
	mov	(sp)+,9f+2
	mov	(sp)+,r0
	sys	indir; 9f
	br	ret

/	integer function seek0(f, n)
/	integer f, n
/		same as seek(f, n, 0)
/		but with 24 bit `n'

seek0.:
	temp
	rval4p; 2	/ f
	i4i2
	rval4p; 4	/ n
	.+2
	mov	(sp)+,r0
	mov	(sp),r1
	bic	$!777,(sp)
	ashc	$-9.,r0
	mov	r1,9f+2
	mov	$3,9f+4
	mov	2(sp),r0
	sys	indir; 9f	/ to block
	mov	(sp)+,9f+2
	mov	$1,9f+4
	mov	(sp)+,r0
	sys	indir; 9f	/ to byte within block
	br	ret

.data
9:	sys	seek; ..; ..
.text

/	integer function open(a, m)
/	logical*1 a(n)
/	integer m
/		same as C, name is ` ' terminated

open.:
	temp
	lvalp; 2	/ a
	rval4p; 4	/ m
	i4i2
	.+2
	mov	(sp)+,9f+4
	mov	(sp)+,r0
	mov	r0,9f+2
1:
	cmpb	(r0)+,$' /
	bne	1b
	clrb	-(r0)
	sys	indir; 9f
	br	ret

.data
9:	sys	open; ..; ..
.text

/	integer function creat(a, m)
/	logical*1 a(n)
/	integer m
/		same as C, name is ` ' terminated

creat.:
	temp
	lvalp; 2	/ a
	rval4p; 4	/ m
	i4i2
	.+2
	mov	(sp)+,9f+4
	mov	(sp)+,r0
	mov	r0,9f+2
1:
	cmpb	(r0)+,$' /
	bne	1b
	clrb	-(r0)
	sys	indir; 9f
	br	ret

.data
9:	sys	creat; ..; ..
.text

/	integer function close(f)
/	integer f
/		same as C

close.:
	temp
	rval4p; 2	/ f
	i4i2
	.+2
	mov	(sp)+,r0
	sys	close

ret:
	bec	1f
	mov	r0,error
	mov	$-1,r0
1:
	mov	r0,temp+2
	sxt	temp
	jmp	retrn

/	error = errno
/		returns last error number

errno.:
	temp
	.+2
	mov	error,r0
	br	ret

.globl	temp
.globl	retrn
.globl	rval4p
.globl	lvalp
.globl	i4i2
.bss
error:	.=.+2
