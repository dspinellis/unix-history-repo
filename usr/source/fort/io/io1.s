/
/

/ io1 --  I/O operators


.globl	rerr
.globl	endio
.globl	rio4
.globl	rio8
.globl	iio2
.globl	iio4
.globl	lio2
.globl	lio1
.globl	cio8
.globl	cio16
.globl	ecvt
.globl	fcvt
.globl	_ndigit

endio:
	mov	(sp)+,r5
	rts	r5

cio8:
	tst	slcnt
	bne	2f
	inc	slcnt
	tst	-(r4)
	br	rio4
2:
	clr	slcnt
	mov	ilval,-(sp)
	add	$4,(sp)
	br	rio4

cio16:
	tst	slcnt
	bne	2f
	inc	slcnt
	tst	-(r4)
	br	rio8
2:
	clr	slcnt
	mov	ilval,-(sp)
	add	$8,(sp)
	br	rio8

rio8:
	mov	$8.\<8+'r,r0
	br	1f

rio4:
	mov	$4\<8+'r,r0
	br	1f

iio4:
	mov	$4\<8+'i,r0
	br	1f

iio2:
	mov	$2\<8+'i,r0
	br	1f

lio2:
	mov	$2\<8+'l,r0
	br	1f

lio1:
	mov	$1\<8+'l,r0

1:
	mov	r0,itype
	mov	(sp)+,ilval
	mov	(sp)+,r5
	tst	(r5)+
	rts	r5

