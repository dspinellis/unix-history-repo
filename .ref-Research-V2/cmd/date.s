/ get date -- date
/ set date -- date mmddhhmm

.globl	ptime

	cmp	(sp)+,$2
	blt	prdate
	tst	(sp)+
	mov	(sp),r0
	setd
	seti
	jsr	r5,tmul; 0
	jsr	r5,tmul; 10.
	movfi	fr1,r1		/ months in r2
	asl	r1
	jsr	r5,tmul; 0
	jsr	r5,tmul; 10.
	movfi	fr1,r3		/ days in r3
	dec	r3
	add	mtab-2(r1),r3
	cmp	r1,$6		/ march or later?
	blt	1f
	add	yr1972,r3	/ leap year correction
1:
	mpy	$24.,r3		/ days to hours
	jsr	r5,tmul; 0
	jsr	r5,tmul; 10.
	movif	r3,fr0
	addf	fr0,fr1		/ total hours
	jsr	r5,tmul; 6.
	jsr	r5,tmul; 10.
	movif	$3600.,fr0
	mulf	fr0,fr1
	tstb	(r0)
	bne	error
	setl
	tst	yr1972
	beq	1f
	movif	yrtime,fr0
	addf	fr0,fr1
1:
	movif	two,fr0
	divf	fr0,fr1
	movfi	fr1,-(sp)
	mov	(sp)+,r0
	mov	(sp)+,r1
	alsc	$1,r0
	sys	stime
	br	1f

prdate:
	sys	time
1:
	alsc	$-8,r0
	bic	$!377,r0
	dvd	$20250.,r0
	add	$0.,r0
	mov	r0,r1
	clr	r0
	dvd	$7.,r0
	mpy	$5.,r1
	add	$days,r1
	mov	r1,0f
	mov	$1,r0
	sys	write; 0:..; 5
	sys	time
	mov	$1,r2
	jsr	pc,ptime
	mov	$1,r0
	sys	write; nl; 1
	sys	exit

error:
	mov	$1,r0
	sys	write; 1f; 2
	sys	exit
1:	<?>
nl:	<\n>

tmul:
	movif	(r5)+,fr2
	mulf	fr2,fr1
	movb	(r0)+,r2
	beq	error
	sub	$'0,r2
	cmp	r2,$10.
	bhis	error
	movif	r2,fr2
	addf	fr2,fr1
	rts	r5

yrtime:
	28872.; 4608.

mtab:
	0
	31.
	59.
	90.
	120.
	151.
	181.
	212.
	243.
	273.
	304.
	334.
0

yr1972:	1
two:	0; 2
days:	<Sun \0>
	<Mon \0>
	<Tue \0>
	<Wed \0>
	<Thur >
	<Fri \0>
	<Sat \0>
	.even
r1,0f
	mov	$1,r0
	sys	write; 0:..; 5
	sys	time
	mov	$1,r2
	jsr	pc,ptime
	mov	$1,r0
	sys	write; nl; 1
	sys	exit

error:
	mov	$1,r0
	sys	write; 1f; 2
	sys	exit
1: