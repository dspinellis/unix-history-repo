| Copyright (c) 1988 Regents of the University of California.
| All rights reserved.
|
| %sccs.include.redist.gas%
|
|	@(#)oc_cksum.s	7.2 (Berkeley) %G%
|
|
| oc_cksum: ones complement 16 bit checksum for MC68020.
|
| oc_cksum (buffer, count, strtval)
|
| Do a 16 bit one's complement sum of 'count' bytes from 'buffer'.
| 'strtval' is the starting value of the sum (usually zero).
|
| It simplifies life in in_cksum if strtval can be >= 2^16.
| This routine will work as long as strtval is < 2^31.
|
| Performance
| -----------
| This routine is intended for MC 68020s but should also work
| for 68030s.  It (deliberately) doesn't worry about the alignment
| of the buffer so will only work on a 68010 if the buffer is
| aligned on an even address.  (Also, a routine written to use
| 68010 "loop mode" would almost certainly be faster than this
| code on a 68010).
|
| We don't worry about alignment because this routine is frequently
| called with small counts: 20 bytes for IP header checksums and 40
| bytes for TCP ack checksums.  For these small counts, testing for
| bad alignment adds ~10% to the per-call cost.  Since, by the nature
| of the kernel's allocator, the data we're called with is almost
| always longword aligned, there is no benefit to this added cost
| and we're better off letting the loop take a big performance hit
| in the rare cases where we're handed an unaligned buffer.
|
| Loop unrolling constants of 2, 4, 8, 16, 32 and 64 times were
| tested on random data on four different types of processors (see
| list below -- 64 was the largest unrolling because anything more
| overflows the 68020 Icache).  On all the processors, the
| throughput asymptote was located between 8 and 16 (closer to 8).
| However, 16 was substantially better than 8 for small counts.
| (It's clear why this happens for a count of 40: unroll-8 pays a
| loop branch cost and unroll-16 doesn't.  But the tests also showed
| that 16 was better than 8 for a count of 20.  It's not obvious to
| me why.)  So, since 16 was good for both large and small counts,
| the loop below is unrolled 16 times.
| 
| The processors tested and their average time to checksum 1024 bytes
| of random data were:
| 	Sun 3/50 (15MHz)	190 us/KB
| 	Sun 3/180 (16.6MHz)	175 us/KB
| 	Sun 3/60 (20MHz)	134 us/KB
| 	Sun 3/280 (25MHz)	 95 us/KB
| 
| The cost of calling this routine was typically 10% of the per-
| kilobyte cost.  E.g., checksumming zero bytes on a 3/60 cost 9us
| and each additional byte cost 125ns.  With the high fixed cost,
| it would clearly be a gain to "inline" this routine -- the
| subroutine call adds 400% overhead to an IP header checksum.
| However, in absolute terms, inlining would only gain 10us per
| packet -- a 1% effect for a 1ms ethernet packet.  This is not
| enough gain to be worth the effort.

	.data
	.asciz	"@(#)$Header: oc_cksum.s,v 1.1 90/07/09 16:04:43 mike Exp $"
	.even
	.text

	.globl	_oc_cksum
_oc_cksum:
	movl	sp@(4),a0	| get buffer ptr
	movl	sp@(8),d1	| get byte count
	movl	sp@(12),d0	| get starting value
	movl	d2,sp@-		| free a reg

	| test for possible 1, 2 or 3 bytes of excess at end
	| of buffer.  The usual case is no excess (the usual
	| case is header checksums) so we give that the faster
	| 'not taken' leg of the compare.  (We do the excess
	| first because we're about the trash the low order
	| bits of the count in d1.)

	btst	#0,d1
	jne	L5		| if one or three bytes excess
	btst	#1,d1
	jne	L7		| if two bytes excess
L1:
	movl	d1,d2
	lsrl	#6,d1		| make cnt into # of 64 byte chunks
	andl	#0x3c,d2	| then find fractions of a chunk
	negl	d2
	andb	#0xf,cc		| clear X
	jmp	pc@(L3-.-2:b,d2)
L2:
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
	movl	a0@+,d2
	addxl	d2,d0
L3:
	dbra	d1,L2		| (NB- dbra doesn't affect X)

	movl	d0,d1		| fold 32 bit sum to 16 bits
	swap	d1		| (NB- swap doesn't affect X)
	addxw	d1,d0
	jcc	L4
	addw	#1,d0
L4:
	andl	#0xffff,d0
	movl	sp@+,d2
	rts

L5:	| deal with 1 or 3 excess bytes at the end of the buffer.
	btst	#1,d1
	jeq	L6		| if 1 excess

	| 3 bytes excess
	clrl	d2
	movw	a0@(-3,d1:l),d2	| add in last full word then drop
	addl	d2,d0		|  through to pick up last byte

L6:	| 1 byte excess
	clrl	d2
	movb	a0@(-1,d1:l),d2
	lsll	#8,d2
	addl	d2,d0
	jra	L1

L7:	| 2 bytes excess
	clrl	d2
	movw	a0@(-2,d1:l),d2
	addl	d2,d0
	jra	L1
