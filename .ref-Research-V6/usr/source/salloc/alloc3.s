.globl	flag
.globl	b1, w1, u1, b1s, b1e
/	here to allocate a new block
/
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
hsz=1024.
numb=4.
numb2=2*numb
w=0
r=2
a=4
l=6
/
allocate:
	mov	r0,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	tst	stats
	bne	1f
	jsr	pc,initl
1:
	inc	stats
	dec	r0
	bmi	1f
	jsr	pc,log2
	inc	r0
1:	asl	r0
	mov	r0,-(sp)
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
	add	$hblk,r1
	mov	(r1),frlist(r0)
	mov	a(r1),r0
	mov	r0,w(r1)
	mov	r0,r(r1)
	tst	(sp)+
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r0
/	jsr	pc,whead
	rts	pc
/
/	no block of required size
/	look for larger block
/
xxx:
	tst	(r0)+
	cmp	r0,$frend-frlist
	bhis	www
	tst	frlist(r0)
	bne	yyy
	br	xxx
/
/	there are no larger blocks;  must garbage collect
/
www:	jsr	pc,collect
	tst	r0
	bne	zzz
/
/	out of space
/
	mov	$1,r0
	sys	write; 1f; 2f-1f
	4
1:	<Out of space.\n>
2:	.even
/
/	split larger block into two smaller pieces and
/	link together as smaller blocks in the free list.
/
yyy:
	mov	hblk,r3	/get free header block
	beq	www
	mov	frlist(r0),r1
	add	$hblk,r1
	mov	w(r1),frlist(r0)
	mov	r3,w(r1)
	add	$hblk,r3
	mov	exp2-2(r0),r2
	add	a(r1),r2
	mov	w(r3),hblk
	mov	l(r1),l(r3)
	mov	r2,l(r1)		/L
	mov	r2,a(r3)
	clr	w(r3)			/W'
	mov	r1,r2
	sub	$hblk,r2
	mov	r2,frlist-2(r0)
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
	mov	frlist(r0),w(r1)
	clr	r(r1)
	sub	$hblk,r1
	mov	r1,frlist(r0)
	clr	r1		/self-defense
/	jsr	pc,whead
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
	add	$hblk,r3
	tst	(r3)		/W
	beq	advance		/only one list element
/
/	calculate address of buddy
/
	mov	a(r3),r4
	sub	headsz,r4
	bit	exp2(r0),r4
	beq	2f
	bic	exp2(r0),r4
	br	1f
2:	bis	exp2(r0),r4
1:	add	headsz,r4
/
/	and search for him
/
loop3:
	cmp	a(r3),r4
	beq	coal
	mov	r3,r2
	mov	w(r3),r3
	tst	r3
	beq	nocoal
	add	$hblk,r3
	br	loop3
/
/	have found a pair; remove both blocks from list,
/	coalesce them, and put them on next higher list
/
coal:	inc	useful
	mov	w(r3),w(r2)	/remove him from list
	mov	(r1),r2
	add	$hblk,r2
	mov	r3,r4
	mov	w(r2),w(r1)	/remove other one
	cmp	a(r2),a(r4)
	bgt	1f
	mov	r2,-(sp)
	mov	r4,r2
	mov	(sp)+,r4
1:	mov	hblk,(r2)
	clr	r(r2)
	mov	headsz,a(r2)
	mov	headsz,l(r2)
	sub	$hblk,r2
	mov	r2,hblk
	add	exp2(r0),l(r4)	/L
	clr	r(r4)
	mov	frlist+2(r0),w(r4)
	sub	$hblk,r4
	mov	r4,frlist+2(r0)
	br	loop2
/
/	no buddy found, try next block on this list
/
nocoal:
	mov	(r1),r1
	add	$hblk,r1
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
/	restore registers and return
/
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
/
/	routine to find integer part of log2(x)
/
/	jsr	pc,log2
/
/	r0 = log2(r0)
/
log2:
	mov	$15.,-(sp)
	tst	r0
	beq	2f
1:
	asl	r0
	bcs	2f
	dec	(sp)
	br	1b
2:
	mov	(sp)+,r0
	rts	pc
/
	0
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
plausible:
	cmp	r1,$strbuf
	blo	botch
	cmp	r1,$strend
	bhis	botch
	rts	pc
/
/
botch:
	mov	r0,-(sp)
	mov	$1,r0
	sys	write; 1f; 2f-1f
	4
1:	<Error in allocator.\n>
2:	.even
/
/
preposterous:
	cmp	r1,$strbuf
	blo	botch
	cmp	r1,$strend
	bhis	botch
	cmp	a(r1),headsz		/A
	blo	botch
	cmp	l(r1),datadr		/L
	bhi	botch
	mov	l(r1),r0		/L
	sub	a(r1),r0		/A
	mov	r0,-(sp)
	jsr	pc,log2
	asl	r0
	cmp	exp2(r0),(sp)
	bne	botch
	mov	r0,(sp)
	mov	frlist(r0),r0
1:	beq	1f
	add	$hblk,r0
	cmp	r0,r1
	beq	botch
	mov	(r0),r0
	br	1b
1:	mov	(sp)+,r0
	rts pc
/
/
whead:
	mov	r0,-(sp)
	mov	afout,r0
	sys	seek;0;0	/write pointer to 0
	sys	write;hblk;hsz
	mov	(sp)+,r0
	rts	pc
/
datasz:	16384.
headsz: hsz
nbuf: numb
nbuf2: numb2

b1s:	.=.+numb2
b1e:	.=.+ numb2
w1:	.=.+ numb2
u1:	.=.+ numb2
b1 = .
/
initl:
	mov	r0,-(sp)
	mov	r2,-(sp)
	sys	open;asmem; 1	/open for write
	bec	2f
	sys	creat;asmem; 606
	bes	err2
2:
	mov	r0,afout
1:
	sys	open; asmem; 0	/open for read
	bes	err2
	mov	r0,afi
1:
	br	gargs
/
err2:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	4
1:	<cannot open output file\n>
2:
asmem:
	<alloc.d\0>
	.even
/
gargs:
	mov	$headers,r2
	mov	r2,r0
	sub	$hblk,r0
	mov	r0,hblk
1:
	add	$8,r0
	mov	r0,(r2)
	add	$8,r2
	cmp	r2,$strend-8.
	blo	1b
	clr	-8(r2)
/
	mov	headsz,datadr
	add	datasz,datadr
/
	mov	$frlist,r0
1:	clr	(r0)+
	cmp	r0,$frend
	blo	1b

	mov	hblk,r2
	add	$hblk,r2
	mov	(r2),hblk
	clr	w(r2)
	mov	headsz,a(r2)
	mov	datadr,l(r2)
	mov	datasz,r0
	jsr	pc,log2
	asl	r0
	cmp	r0,$frend-frlist
	blo	9f; 4; 9:
	sub	$hblk,r2
	mov	r2,frlist(r0)
/
/	install plausible pointers to make octal dumps look ok
/
	mov	$hblk,r1
1:	mov	(r1),r1
	beq	1f
	add	$hblk,r1
	mov	headsz,a(r1)
	mov	headsz,l(r1)
	br	1b
/
1:	mov	afout,r0
	sys	write;hblk;hsz
	jsr	pc,reset
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	pc
. = b1 + [512.*numb]
/
.bss

flag:	.=.+2
stats:	.=.+18.
useful:	.=.+2
afi:	.=.+2
afout:	.=.+2
datadr:	.=.+2
hblk: .=.+2	/must remain here - pointer to free header
frlist: .=hblk+34.
frend:
headers:
strbuf:	.=hblk+hsz
strend:
nchar: .=.+2
end:
