/
/
/	here to allocate a new block
/
/	mov	...,r0
/	jsr	pc,allocate
/	mov	r1,...
/
/	requested size in bytes in r0
/	pointer to header of allocated block returned in r1
/	r0 is preserved
/
/	convert to words, adjust for header, round up
/	to a power of two
/
/	each block has a four-word header
/		W - write ptr (also used as link ptr in frlist)
/		R - read ptr
/		A - pointer to head of data
/		L - ptr to (end+1) of data
w=0
r=2
a=4
l=6
/
allocate:
	clr	garbage
	mov	r0,-(sp)
	mov	r2,-(sp)
	tst	stats
	bne	1f
	jsr	pc,init
1:
	inc	stats
	bne 9f; inc stats; 9:
	cmp	r0,$strend-strbuf
	blos 9f; 4; 9:
1:
	cmp	$8.,r0
	blo	2f
	mov	$3.,r0
	br	1f
2:
	sub	$1,r0
	bmi	1f
	jsr	pc,log2
	add	$1,r0
1:	asl	r0		/bite to word
	mov	r0,-(sp)
	add	$2,r0
	cmp	r0,$frend-frlist+2
	blo	zzz
	jmp	err
/
/	look on free list for block of required size
/
zzz:
	mov	(sp),r0
	tst	frlist(r0)
	beq	xxx
/
/	found it, allocate and return
/
	mov	frlist(r0),r1
	mov	(r1),frlist(r0)
	mov	a(r1),r0
	mov	r0,w(r1)		/W
	mov	r0,r(r1)		/R
	tst	(sp)+
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	pc
/
/	no block of required size
/	look for larger block
/
xxx:
	tst	hdrptr
	bne	1f
	mov	r0,-(sp)
	jsr	pc,morehd
	tst	r0
	bne	out
	mov	(sp)+,r0
1:
	tst	(r0)+
	cmp	r0,$frend-frlist
	bhis	www
	tst	frlist(r0)
	bne	yyy
	br	xxx
/
/	there are no larger blocks;  must garbage collect
/
www:
	jsr	pc,collect
	tst	r0
	bne	zzz
	jsr	pc,moresp
	tst	r0
	beq	zzz
/
/	out of space
/
out:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	jmp	reset
1:	<Out of space.\n>
2:	.even
/
/	split larger block into two smaller pieces and
/	link together as smaller blocks in the free list.
/
yyy:
	mov	frlist(r0),r1
	mov	(r1),frlist(r0)
	mov	hdrptr,r2
	bne	1f
	mov	r0,-(sp)
	jsr	pc,morehd
	tst	r0
	bne	out
	mov	(sp)+,r0
	mov	hdrptr,r2
1:
	mov	(r2),hdrptr
	clr	(r2)
	mov	r2,(r1)
	mov	r1,hdrptr(r0)
	mov	l(r1),l(r2)
	mov	l(r1),r0
	sub	a(r1),r0
	asr	r0
	add	a(r1),r0
	mov	r0,l(r1)
	mov	r0,a(r2)
	br	zzz
/
/
/	here to release a block
/
/	mov	...,r1
/	jsr	pc,release
/
/	pointer to block in r1
/
release:
/
/	discover that this is a plausible pointer
/
	mov	r0,-(sp)
	jsr	pc,preposterous
/
/	find free list index and link block to that entry
/
	inc	stats+2
	mov	frlist(r0),(r1)
	clr	r(r1)
	mov	r1,frlist(r0)
	clr	r1		/self-defense
	mov	(sp)+,r0
	rts	pc
/
/
/	jsr	pc,collect
/
/	coalesce free storage by rejoining paired blocks
/	on the free list.
/	zero is returned in r0 if no paired blocks were found.
/
collect:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	clr	useful
	inc	stats+4.
	clr	r0		/start with smallest blocks
				/r0 contains frlist index
loop1:	mov	$frlist,r1
	add	r0,r1
/
/	try next list member at this level
/
loop2:	mov	(r1),r3
	beq	advance		/list is empty
	tst	*(r1)		/W
	beq	advance		/only one list element
/
/	calculate address of buddy
/
	mov	a(r3),r4
	mov	$block,r2
1:
	cmp	r4,(r2)
	blo	1f
	cmp	r2,lblock
	beq	2f
	add	$2,r2
	br	1b
1:
	sub	$2,r2
2:
	mov	(r2),beg
	sub	beg,r4
	bit	exp2(r0),r4
	beq	2f
	bic	exp2(r0),r4
	br	1f
2:	bis	exp2(r0),r4
1:	add	beg,r4
/
/	and search for him
/
loop3:	tst	0(r3)
	beq	nocoal
	mov	(r3),r2
	cmp	a(r2),r4
	beq	coal
	mov	(r3),r3
	br	loop3
/
/	have found a pair; remove both blocks from list,
/	coalesce them, and put them on next higher list
/
coal:	inc	useful
	mov	(r3),r4
	mov	(r4),(r3)	/remove him from list
	mov	(r1),r2
	mov	(r2),(r1)	/remove the other one
	cmp	a(r2),a(r4)
	bgt	1f
	mov	r2,-(sp)
	mov	r4,r2
	mov	(sp)+,r4
1:	add	exp2(r0),l(r4)
	clr	r(r4)
	mov	frlist+2(r0),(r4)
	mov	r4,frlist+2(r0)
	mov	hdrptr,(r2)
	mov	r2,hdrptr
	clr	r(r2)
	mov	beg,a(r2)
	mov	beg,l(r2)
	br	loop2
/
/	no buddy found, try next block on this list
/
nocoal:
	mov	(r1),r1
	br	loop2
/
/	advance to next free list
/
advance:
	tst	(r0)+
	cmp	r0,$frend-frlist
	blo	loop1
	mov	useful,r0
/
/	do we have enough headers to continue?
/
	cmp	garbage,$2
	blo	1f
	mov	$1,r0
	sys	write; 4f; 5f-4f
	jmp	reset
/
4:	<Out of space - too big a block.\n>
5:	.even
/
/
/	restore registers and return
/
1:
	inc	garbage
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
/
.bss
garbage: .=.+2
.text
/
/	routine to get more space for strings
/
moresp:
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	brk,r1
	mov	$block,r2
	add	nblock,r2
	cmp	r2,$blkend
	bhis	rout
	mov	r1,(r2)
	mov	r1,lblock
	add	$2,nblock
	add	$10000,r1
	mov	r1,9f
	sys	break;9:..
	bes	2f
	mov	hdrptr,r2
	bne	1f
	jsr	pc,morehd
	tst	r0
	beq	2f
	mov	hdrptr,r2
1:
	mov	(r2),hdrptr
	mov	brk,a(r2)
	mov	r1,brk
	mov	r1,l(r2)
	clr	r(r2)
	mov	$10000,r0
	jsr	pc,log2
	asl	r0
	mov	frlist(r0),w(r2)
	mov	r2,frlist(r0)
	clr	r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc
2:
	mov	$1,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc
/
/	routine to get move space for headers
/
morehd:
	mov	r2,-(sp)
	mov	brk,r0
	mov	$hblock,r2
	add	nhdr,r2
	cmp	r2,$hblkend
	bhis	rout
	mov	r0,(r2)
	mov	r0,lhblock
	add	$2,nhdr
	add	$1024.,r0
	mov	r0,9f
	sys	break;9:..
	bes	2f
	mov	brk,r2
	mov	r2,hdrptr
	mov	r0,brk
	sub	$8,r0
1:
	add	$8,r2
	mov	r2,-8(r2)
	cmp	r2,r0
	blos	1b
	clr	-8(r2)
	clr	r0
	mov	(sp)+,r2
	rts	pc
2:
	mov	$1,r0
	mov	(sp)+,r2
	rts	pc
rout:
	mov	$1,r0
	sys	write; 4f; 5f-4f
	jmp	reset
/
4:	<out of space - no more block storage\n>
5:	.even
/
/	routine to find integer part of log2(x)
/
/	jsr	pc,log2
/
/	r0 = log2(r0)
/
log2:
	mov	r0,-(sp)
	bge 9f; 4; 9:
	mov	$15.,r0
1:
	rol	(sp)
	bmi	1f
	sob	r0,1b
1:
	dec	r0
	tst	(sp)+
	rts	pc
/
	0		/Don't move me, I'm exp(-1)
exp2:
	1;2;4;10;20;40;100;200;400;1000;2000;4000;
	10000;20000;40000;100000
/
/	routine to discover whether r1 points to
/	a plausible header - to avoid ruination.
/
/	r1 is preserved and r0 gets a suitable index for frlist
/
/	jsr	pc,preposterous
/
preposterous:
	mov	r2,-(sp)
	mov	$hblock,r2
1:
	cmp	r1,(r2)
	blo	1f
	cmp	(r2),lhblock
	beq	2f
	add	$2,r2
	br	1b
1:
	sub	$2,r2
2:
	mov	(r2),r2
	add	$1024.,r2
	cmp	r1,r2
	blo	9f;4;9:
	mov	$block,r2
1:
	cmp	a(r1),(r2)
	blo	1f
	cmp	(r2),lblock
	beq	2f
	add	$2,r2
	br	1b
1:
	sub	$2,r2
2:
	cmp	l(r1),(r2)
	bhis	9f;4;9:
	mov	(r2),r2
	add	$10000,r2
	cmp	a(r1),r2
	blo	9f;4;9:
	cmp	l(r1),r2
	blos	9f;4;9:
	mov	(sp)+,r2
	mov	l(r1),r0		/L
	sub	a(r1),r0		/A
	mov	r0,-(sp)
	jsr	pc,log2
	asl	r0
	cmp	exp2(r0),(sp)
	beq 9f; 4; 9:
	add	$2,r0
	cmp	r0,$frend-frlist+2
	blo 9f; 4; 9:
	sub	$2,r0
	mov	r0,(sp)
	mov	frlist(r0),r0
1:	beq	1f
	cmp	r0,r1
	bne 9f; 4; 9:
	mov	(r0),r0
	br	1b
1:	mov	(sp)+,r0
	rts	pc
/
/
/	routine to initialize storage area, headers and
/	free list upon first call to allocate a block.
/	The entire storage area is formed into a single block.
/
init:
	mov	r0,-(sp)
	mov	r1,-(sp)
/
/	form all the headers into a single list.
/
	mov	$headers,r0
	mov	r0,hdrptr
1:	add	$8,r0
	mov	r0,-8(r0)
	cmp	r0,$headend-8
	blos	1b
	clr	-8(r0)
	mov	$frlist,r0
1:	clr	(r0)+
	cmp	r0,$frend
	blo	1b
/
	mov	hdrptr,r1
	mov	(r1),hdrptr
	clr	w(r1)
	mov	$strbuf,r0
	mov	r0,a(r1)
	mov	$strend-strbuf,r0
	jsr	pc,log2
	asl	r0
	cmp	r0,$frend-frlist
	blo 9f; 4; 9:
	mov	r1,frlist(r0)
	mov	exp2(r0),r0
	add	$strbuf,r0
	mov	r0,l(r1)
	mov	$hdrptr,r1
1:	mov	(r1),r1
	tst	r1
	beq	1f
	mov	$strbuf,a(r1)
	mov	$strbuf,l(r1)
	br	1b
1:
	mov	$end,brk
	add	$2,nblock
	mov	$strbuf,block
	mov	$strbuf,lblock
	mov	$headers,hblock
	add	$2,nhdr
	mov	$headers,lhblock
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	pc
/
/
.bss
nhdr:	.=.+2
lhblock:	.=.+2
hblock:	.=.+20.
hblkend:
stats:	.=.+16.
useful:	.=.+2
beg:	.=.+2
lblock:	.=.+2
nblock:	.=.+2
block:	.=.+40.
blkend:
brk:	.=.+2
hdrptr:	.=.+2	/do not move me
frlist:	.=hdrptr+32.
frend:
headers:.=hdrptr+1024.
headend:
strbuf:	.=.+10000
strend:
end:
signal = 48.
