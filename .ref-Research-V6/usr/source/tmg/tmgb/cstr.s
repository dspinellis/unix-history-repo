.globl jget,iget
.globl succ
.globl create,rewind,putchar,getchar,allocate
.globl smark
.globl rewcstr,putcstr,getcstr,size

/ currnet string manipulations
/ keep a initial fragment handy for quick access
/go to allocator for the rest

smark:
	jsr	pc,jget
	clr	cstrw
	jmp	succ

rewcstr:
	clr	cstrr
	rts	pc

putcstr:
	mov	cstrw,r1
	inc	cstrw
	cmp	r1,$cstrt	/is it quick access?
	bge	1f
	movb	r0,cstrb(r1)	/yes, stash the char
	rts	pc
1:
	bne	1f
	mov	r0,-(sp)	/first char to allocator
	mov	symp,r1
	bne	2f
	mov	$64,r0
	jsr	pc,allocate
	mov	r1,symp
2:
	jsr	pc,create
	mov	(sp)+,r0
1:
	mov	symp,r1
	jsr	pc,putchar
	rts	pc

getcstr:
	mov	cstrr,r1
	cmp	r1,cstrw
	blt	1f
	clr	r0	/end of string
	rts	pc
1:
	inc	cstrr
	cmp	r1,$cstrt
	bge	1f
	movb	cstrb(r1),r0
	rts	pc
1:
	bne	1f
	mov	symp,r1
	jsr	pc,rewind
1:
	mov	symp,r1
	jsr	pc,getchar
	rts	pc

size:
	jsr	pc,iget
	mov	cstrw,(r0)
	jmp	succ

cstrt = 16		/top of quick access current string
.data
symp:	0	/pointer to dynamicallly allocated current string
cstrw:	0	/current string write pointer
.bss
cstrr:	.=.+2	/read pointer
cstrb:	.=.+cstrt	/base of quick access fragment
