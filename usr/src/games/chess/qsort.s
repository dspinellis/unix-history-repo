/ qsort interfact to c

/	qsort(from, to)

.globl	_qsort
_qsort:
	mov	2(sp),r1
	mov	4(sp),r2
	jsr	pc,qsort
	rts	pc


qsort:
	mov	r2,r3
	sub	r1,r3
	cmp	r3,$4
	ble	done
	asr	r3
	bic	$3,r3
	add	r1,r3
	mov	r1,-(sp)
	mov	r2,-(sp)

loop:
	cmp	r1,r3
	bhis	loop1
	cmp	(r1),(r3)
	bgt	loop1
	add	$4,r1
	br	loop

loop1:
	cmp	r2,r3
	blos	1f
	sub	$4,r2
	mov	r2,r0
	cmp	(r0),(r3)
	bge	loop1

	mov	(r1),r0
	mov	(r2),(r1)+
	mov	r0,(r2)+
	mov	(r1),r0
	mov	(r2),(r1)
	mov	r0,(r2)
	cmp	-(r1),-(r2)
	cmp	r1,r3
	bne	loop
	mov	r2,r3
	br	loop

1:
	cmp	r1,r3
	beq	1f
	mov	(r1),r0
	mov	(r2),(r1)+
	mov	r0,(r2)+
	mov	(r1),r0
	mov	(r2),(r1)
	mov	r0,(r2)
	cmp	-(r1),-(r2)
	mov	r1,r3
	br	loop1

1:
	mov	(sp)+,r2
	mov	r3,-(sp)
	mov	r3,r1
	add	$4,r1
	jsr	pc,qsort
	mov	(sp)+,r2
	mov	(sp)+,r1
	br	qsort

done:
	rts	pc

	rti = 2

/	itinit()

.globl	_itinit
.globl	_intrp, _term
signal = 48.
time = 13.

_itinit:
	sys	signal; 1; 1
	bit	$1,r0
	bne	1f
	sys	signal; 1; _onhup
1:
	sys	signal; 2; 1
	bit	$1,r0
	bne	1f
	sys	signal; 2; onint
1:
	sys	signal; 3; 1
	rts	pc

.globl	_onhup
_onhup:
	sys	signal; 1; 1
	sys	signal; 2; 1
	sys	signal; 3; 1
	jmp	_term

onint:
	mov	r0,-(sp)
	sys	signal; 2; onint
	inc	_intrp
	mov	(sp)+,r0
	rti

/	t = clock()

.globl	_clock
_clock:
	sys	time
	mov	r0,-(sp)
	mov	r1,-(sp)
	sub	t+2,r1
	sbc	r0
	sub	t,r0
	mov	r1,r0
	mov	(sp)+,t+2
	mov	(sp)+,t
	rts	pc

.bss
t:	.=.+4
