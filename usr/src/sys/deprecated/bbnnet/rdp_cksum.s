#define M_OFF	4
#define M_LEN	8
#define IPHLEN 20			/* sizeof(struct ip) */
#define	LGP	2			/* log2(adds in unrolled loop) */
#define ADDLEN	24

 # r0	checksum result				result
 # r1	current longword to add			curlong
 # r2	pointer to data				p
 # r3	byte count in current mbuf		count
 # r4	"odd" byte count			bytenum
 # r5	pointer to mbuf chain			m

.text
.align 1
.globl	_rdp_cksum

_rdp_cksum:
.set MASK, 0x00c0
	.word MASK			# use r6,r7 (in addition to r0-r5)

	movl	4(ap), r5		# m = arg to function
	clrl	r0			# result = 0

	# Assume IP header and RDP header always with in first mbuf.
	# Assume mbuf chain is at least 1 long

	addl3	M_OFF (r5), r5, r2	# p = mtod(m, cast)
	addl2	$IPHLEN, r2		# p += sizeof(struct ip)
	subw3	$IPHLEN, M_LEN (r5), r3 # count = m->m_len - sizeof(struct ip)
	cvtwl	r3, r3

Ldombuf:
	# Determine the number of longwords in this mbuf.  Note that we
	# are depending on the VAX Architecture that allows access to
	# non-aligned data.  (When we cross MBUF boundries an an earlier
	# one was not filled with an 'even' number of bytes for longwords).

	ashl	$-2, r3, r6		# n_longs = n_bytes >> 2
	extzv	$0, $2, r3, r4		# bytenum = n_bytes & 3

	# Now, can add together as many longwords as possible.  We have
	# unrolled the loop for efficiency, so let's calculate the number
	# of times through the loop and the partial pass.

	extzv	$0, $LGP, r6, r7	# r7 = # adds in partial pass
	ashl	$-LGP, r6, r6		# r6 = # whole passes

	mull2	$ADDLEN, r7		# convert adds to bytes of instruc
	subl3	r7, $Lhere, r7
	jmp	(r7)			# and jump into the loop

	#
	# There is VAX order, adding order, and network order to consider
	#
	# VAX order: 80 1 2 3 is the VAX integer 03020180 since the low
	#	bytes come first when treated as an unsigned character array
	#	on the vax.
	#
	# adding order: add so that carries propogate in the same manner that
	#	they would if the machine had its bytes in network order
	#	    80 01 02 03 + 80 01 02 03 = 00020406, since 80 is msb
	#	    00 80 00 00 + 00 80 00 00 = 01000000
	#	This is just essentially getting the bytes into the host's
	#	integer format.  adding order should work for the rotate too.
	#	We MUST add the bytes in adding order so that different
	#	machine architectures get the same result.  We cannot add
	#	in native mode and f(result) because the propogation of
	#	carries in native cannot be made equivalent to the propogation
	#	of carries in adding order
	#
	# network order: The resulting checksum should be transferred in
	#	network order.  The VAX result 01020304 would be converted
	#	to 04030201 for communication with remote host.
	#

Ltop:
#define SUML \
	;movl	(r2)+, r7		/* fetch longword */		\
	;rotl	$-8, r7, r1		/* put it in adding order */	\
	;insv	r1, $16, $8, r1						\
	;movb	-1(r2), r1						\
	;addl2	r1, r0			/* result += ... */		\
	;rotl	$1, r0, r0		/* and rotate it per spec */

	SUML
	SUML
	SUML
	SUML
Lhere:
	sobgeq	r6, Ltop

	# Now, add in remaining bytes, if any

	tstl	r4
	bneq	Leftovers
	movl	(r5), r5		# m = m->m_next
	bneq	Lnextmbuf
Ldone:
	# Convert result from adding order to network order

	pushl	r0
	rotl	$-8,(sp),r0
	insv	r0,$16,$8,r0
	movb	3(sp),r0
	addl2	$4, sp

	ret

Lnextmbuf:
	addl3	M_OFF (r5), r5, r2	# p = mtod(m, cast)
	cvtwl	M_LEN (r5), r3		# count = m->m_len
	brw	Ldombuf			# assume zero length mbufs unusual

	# In adding in the remainder of this mbuf and part of the next one,
	# we're trying to build up a single 32 bit quantity for adding into
	# the checksum.
	#
	# use fact that:
	#	result += curlong = (a<<24) | (b<<16) | (c<<8) | d
	# is the same as
	#	result += a<<24; result += b<<16; result += c<<8; result += d

Leftovers:
	movl	$3, r6
L1:
	movzbl	(r2)+, r1		# r1 = this byte (unsigned char)
	ashl	$3, r6, r7		# r7 = r6 * 8
	ashl	r7, r1, r1		# r1 <<= r7
	addl2	r1, r0			# result += this byte
	decl	r6
	sobgtr	r4, L1			# get next byte in this mbuf

	# Now, grab bytes from next mbuf
L2:
	movl	(r5), r5
	bneq	L3
	rotl	$1, r0, r0		# last mbuf had odd byte count
	brw	Ldone
L3:
	cvtwl	M_LEN (r5), r3		# count = m->m_len
	beql	L2			# if (count == 0) do next mbuf
	addl3	M_OFF (r5), r5, r2	# p = mtod(m, cast)
L4:
	movzbl	(r2)+, r1		# r1 = this byte (unsigned char)
	ashl	$3, r6, r7		# r7 = r6 * 8
	ashl	r7, r1, r1		# r1 <<= r7
	addl2	r1, r0			# result += this byte
	decl	r6
	bgeq	L5			# got last byte in long?
	rotl	$1, r0, r0
	decl	r3
	brw	Ldombuf			# and continue checksumming
L5:
	sobgtr	r3, L4			# grab next byte from this mbuf
	brb	L2			# but go to next if have too
