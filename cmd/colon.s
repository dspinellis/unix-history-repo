/ colon -- do nothing

	sys	exit

; 9:
	mov	r1,frlist(r0)
	mov	exp2(r0),r0
	add	$strbuf,r0
	mov	r0,l(r1)
	mov	$frlist-2,r1
1:	mov	(r1),r1
	tst	r1
	beq	1f
	mov	$strbuf,a(r1)
	mov	$strbuf,l(r1)
	br	1b
1:
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	pc
/
/
	.bss
stats:	.=.+16.
useful:	.=.+2
hdrptr:	.=.+2	/do not move me
frlist:	.=hdrptr+32.
frend:
headers:.=hdrptr+512.
headend:
strbuf:	.=.+4000
strend:
end:
v	frlist-2,r1
	mov	(r1),frlist-2
	clr	w(r1)
	mov	$strbuf,r0
	mov	r0,a(r1)
	mov	$strend-strbuf,r0
	jsr	pc,log2
	asl	r