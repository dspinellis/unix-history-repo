/
/

/ Fortran binary I/O

.globl	iowu
.globl	ioru
.globl	rewi
.globl	enfl

iowu:
	tst	(sp)+
	mov	(sp)+,r1		/ unit number
	jsr	r5,setio; 42
1:
	jsr	r5,getitm
		br 1f
	movb	ilen,r1
	mov	ilval,r2
2:
	movb	(r2)+,r0
	jsr	r5,fputc
	sob	r1,2b
	br	1b
1:
	jsr	r5,fflush
	jmp	*(r4)+

ioru:
	tst	(sp)+
	mov	(sp)+,r1		/ unit number
	jsr	r5,setio; 41
	clr	nlflg
	mov	pc,binflg
1:
	jsr	r5,getitm
		br 1f
	movb	ilen,r1
	mov	ilval,r2
2:
	jsr	r5,fgetc
	movb	r0,(r2)+
	sob	r1,2b
	br	1b
1:
	jmp	*(r4)+

rewi:
enfl:
	tst	(sp)+
	mov	(sp)+,r1		/ unit number
	jsr	r5,chkunit
	clrb	utable(r1)
	asl	r1
	mov	*btable(r1),r0
	cmp	r0,$1
	bhi	1f
	sys	seek; 0; 0
	jmp	*(r4)+
1:
	sys	close
	jmp	*(r4)+
