rti = 2
.globl rm
.globl ck
.globl loop
.globl oct
.globl argc
.globl arg
.globl tfiget
.globl asmem
.globl qflag
.globl vflag
.globl getspq
.globl q
.globl ask
.globl getword
.globl r
.globl w
.globl	output
.globl buffer
.globl bufend
.globl fv
.globl	getsp
.globl release
.globl nothere
.globl getn
.globl getchar
.globl rewind
.globl iflag
.globl inter
/	output buffer
/	r2 = last char in buffer + 1
/	r2 returned at beginning of buffer
/
output:
	sub	$buffer,r2
	blos	1f
	mov	r2,0f
	mov	$1,r0
	sys	write; buffer; 0:..
	mov	$buffer,r2
1:
	rts	pc
/
/
/	routine to find the string pointed to by r0
/	in either memory or curly
/	r1 = where to look
/
/	returns error set if string not there
/	returns r1 = header of string if there
getsp:
	mov	r0,-(sp)
	mov	asmem,r1
	jsr	pc,rewind	/memory
3:
	jsr	pc,getword
	bes	1f
	mov	r0,r1
	jsr	pc,rewind
	mov	(sp),r2
2:
	jsr	pc,getchar
	bes	2f
	tstb	(r2)
	beq	4f
	cmpb	r0,(r2)+
	beq	2b
4:
	mov	asmem,r1
	add	$2,r(r1)
	br	3b
2:
	tstb	(r2)
	bne	4b
	mov	r1,r2
	mov	asmem,r1
	jsr	pc,getword
	tst	(sp)+
	rts	pc
1:
	mov	(sp)+,r0
	sec
	rts	pc
/
/	finds header of string with name same as that pointed to by r0
/	taking into account ? and * 
getspq:
	mov	r0,-(sp)
	mov	asmem,r1
	tst	qflag
	bgt	1f
	jsr	pc,rewind
1:
	jsr	pc,getword
	bes	1f
	mov	r0,r1
	jsr	pc,rewind
	mov	(sp),r2
2:
	cmpb	(r2),$'*
	bne	3f
	clr	strflg
	mov	$2,qflag
	inc	r2
	tstb	(r2)
	beq	6f
	mov	r2,ch
3:
	jsr	pc,getchar
	bes	2f
	tstb	(r2)
	bne	3f
	cmp	$2,qflag
	bne	4f
	mov	ch,r2
3:
	cmpb	(r2),$'?
	bne	3f
	inc	r2
	tst	qflag
	bne	2b
	mov	$1,qflag
	br	2b
3:
	cmpb	r0,(r2)+
	bne	5f
	inc	strflg
	br	2b
5:
	cmp	$2,qflag
	bne	4f
	tst	strflg
	bne	4f
	dec	r2
	br	2b
4:
	mov	asmem,r1
	add	$2,r(r1)
	br	1b
2:
	tstb	(r2)
	bne	4b
6:
	mov	r1,r2
	mov	asmem,r1
	jsr	pc,getword
	tst	(sp)+
	rts	pc
1:
	mov	$-1.,qflag
	mov	(sp)+,r0
	sec
	rts	pc
/
/	tfiget
/	jsr	r5,tfiget
/
tfiget:
	clr	r0
	sys	read; ch; 1
	bes	1f
	tst	iflag
	beq 9f; jmp loop; 9:
	tst	r0
	beq	1f
	movb	ch,r0
	rts	pc
1:	jmp	q
/
/	routine to print file name and error message
/	r2 = next free byte in buffer
/
nothere:
	mov	$buffer,r2
1:
	tstb	(r1)
	beq	1f
	movb	(r1)+,(r2)+
	br	1b
1:
	sub	$buffer,r2
	mov	r2,0f
	mov	$1,r0
	sys	write; buffer; 0:..
	mov	$1,r0
	sys	write; err4; 16.
	rts	pc
/
/
/	routine to put characters from string in buffer
/	r1 = header of string
/	on return r2 = next free byte in buffer
/
getn:
	mov	$buffer,r2
	jsr	pc,rewind
1:
	jsr	pc,getchar
	bes	1f
	movb	r0,(r2)+
	cmp	r2,$bufend
	bhis	1f
	br	1b
1:	rts	pc
/
ask:
	mov	r2,-(sp)
	mov	r0,r1
	jsr	pc,getn
	movb	$' ,(r2)+
	jsr	pc,output
	jsr	pc,tfiget
	cmpb	$'\n,r0
	bne	1f
2:
	mov	asmem,r1
	add	$2,r(r1)
	mov	(sp)+,r2
	sec
	rts	pc
1:
	cmpb	r0,$'y
	beq	4f
	cmpb	r0,$'q
	beq	3f
1:
	tst	iflag
	bne	2b
	jsr	pc,tfiget
	cmpb	$'\n,r0
	bne	1b
	br	2b
4:
	tst	iflag
	bne	2b
	jsr	pc,tfiget
	cmpb	$'\n,r0
	bne	1b
	mov	asmem,r1
	mov	(sp)+,r2
	clc
	rts	pc
3:
	tst	iflag
	bne	2b
	jsr	pc,tfiget
	cmpb	$'\n,r0
	bne	1b
	mov	(sp)+,r2
	jmp	loop
inter:
	inc	iflag
	rti
/	to remove an entry from asmem - r points to name ptr
/
rm:
	mov	r3,-(sp)
	mov	asmem,r1
	mov	r(r1),r3
	sub	$4,r(r1)
	mov	r(r1),r2
	mov	r2,-(sp)
1:
	mov	r3,r(r1)
	jsr	pc,getword
	bes	1f
	mov	r(r1),r3
	mov	r2,r(r1)
	jsr	pc,alterword
	mov	r(r1),r2
	br	1b
1:
	mov	r2,w(r1)
	mov	(sp)+,r(r1)
	mov	(sp)+,r3
	rts	pc
/
/	check that header pointed to by r2 released
/
ck:
	mov	r2,-(sp)
	mov	l(r2),r0
	sub	a(r2),r0
	sub	$hblk,r2
	jsr	pc,log2
	asl	r0
	mov	r0,ch
	mov	frlist(r0),r0
1:
	beq	1f
	cmp	r2,r0
	beq	2f
	add	$hblk,r0
	mov	(r0),r0
	br	1b
2:
	mov	ch,r0
	inc	freeb(r0)
	mov	(sp)+,r2
	rts	pc
1:
	mov	(sp)+,r2
	sec
	rts	pc
oct:
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	div	$8.,r0
	mov	r1,-(sp)
	add	$'0,(sp)
	tst	r0
	beq	1f
	jsr	r5,oct
1:
	movb	(sp)+,ch
	mov	$1,r0
	sys	write; ch; 1
	mov	(sp)+,r1
	rts	r5

decml:
	mov	r1,-(sp)
	mov	r0,r1
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	add	$'0,(sp)
	tst	r0
	beq	1f
	jsr	r5,decml
1:
	movb	(sp)+,ch
	mov	$1,r0
	sys	write; ch; 1
	mov	(sp)+,r1
	rts	r5
/
/
iflag:	0
qflag:	0
vflag:	0
	.data
err4:	< not in memory.\n>
err1:	<Cannot open file >
end1:	.even
	.bss
freeh:	.=.+2
freeb:	.=.+32.
freen:
ch:	.=.+2
strflg:	.=.+2
buffer:	.=.+512.
bufend:	.=.+2
argc = buffer + 20.
arg = buffer + 256.
fv:	.=.+2
	.text
