/ rmdir -- unlink directory

	mov	(sp)+,r5
	tst	(sp)+

loop:
	dec	r5
	ble	done
	mov	(sp)+,r1
	mov	$name,r2
	clr	r0
1:
	inc	r0
	movb	(r1)+,(r2)+
	bne	1b
	dec	r2
	dec	r0
	mov	r0,size
	cmp	r2,$name
	beq	error
	sys	stat; name; stbuf
	bes	error
	bic	$!60000,stbuf+4
	cmp	$40000,stbuf+4
	bne	error
	cmpb	-1(r2),$'.
	bne	1f
	cmp	r2,$name+1
	beq	error
	cmpb	-2(r2),$'/
	beq	error
	cmpb	-2(r2),$'.
	bne	1f
	cmp	r2,$name+2
	beq	error
	cmpb	-3(r2),$'/
	beq	error
1:
	sys	open; name; 0
	bes	error
	mov	r0,r1
1:
	mov	r1,r0
	sys	read; stbuf; 16.
	bes	1f
	tst	r0
	beq	1f
	tst	stbuf
	beq	1b
	cmpb	stbuf+2,$'.
	bne	error1
	tstb	stbuf+3
	beq	1b
	cmpb	stbuf+3,$'.
	bne	error1
	tstb	stbuf+4
	beq	1b

error1:
	jsr	pc,prname
	mov	r1,r0
	sys	close
	mov	$1,r0
	sys	write; mes1; emes1-mes1
	br	loop

1:
	mov	r1,r0
	sys	close
	movb	$'/,(r2)+
	movb	$'.,(r2)+
	movb	$'.,(r2)+
	clrb	(r2)
	sys	unlink; name
	clrb	-(r2)
	sys	unlink; name
	clrb	-2(r2)
	sys	unlink; name
	br	loop

error:
	jsr	pc,prname
	mov	$1,r0
	sys	write; mes2; emes2-mes2
	br	loop

prname:
	mov	$1,r0
	sys	write; name; size:..
	rts	pc

done:
	sys	exit

mes1:
	< -- directory not empty\n>
emes1:
mes2:
	< ?\n>
emes2:
	.even

.bss
name:	.=.+40.
stbuf:	.=.+40.

