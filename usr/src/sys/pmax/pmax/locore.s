/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Digital Equipment Corporation and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * Copyright (C) 1989 Digital Equipment Corporation.
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies.
 * Digital Equipment Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/mach/ds3100.md/RCS/loMem.s,
 *	v 1.1 89/07/11 17:55:04 nelson Exp $ SPRITE (DECWRL)
 * from: $Header: /sprite/src/kernel/mach/ds3100.md/RCS/machAsm.s,
 *	v 9.2 90/01/29 18:00:39 shirriff Exp $ SPRITE (DECWRL)
 * from: $Header: /sprite/src/kernel/vm/ds3100.md/vmPmaxAsm.s,
 *	v 1.1 89/07/10 14:27:41 nelson Exp $ SPRITE (DECWRL)
 *
 *	@(#)locore.s	7.15 (Berkeley) %G%
 */

/*
 *	Contains code that is the first executed at boot time plus
 *	assembly language support routines.
 */

#include <sys/errno.h>
#include <sys/syscall.h>

#include <machine/param.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/machAsmDefs.h>
#include <machine/pte.h>

#include "assym.h"

	.set	noreorder

/*
 * Amount to take off of the stack for the benefit of the debugger.
 */
#define START_FRAME	((4 * 4) + 4 + 4)

	.globl	start
start:
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	li	t1, MACH_CACHED_MEMORY_ADDR	# invalid address
	mtc0	t1, MACH_COP_0_TLB_HI		# Mark entry high as invalid
	mtc0	zero, MACH_COP_0_TLB_LOW	# Zero out low entry.
/*
 * Clear the TLB (just to be safe).
 * Align the starting value (t1), the increment (t2) and the upper bound (t3).
 */
	move	t1, zero
	li	t2, 1 << VMMACH_TLB_INDEX_SHIFT
	li	t3, VMMACH_NUM_TLB_ENTRIES << VMMACH_TLB_INDEX_SHIFT
1:
	mtc0	t1, MACH_COP_0_TLB_INDEX	# Set the index register.
	addu	t1, t1, t2			# Increment index.
	bne	t1, t3, 1b			# NB: always executes next
	tlbwi					# Write the TLB entry.

	la	sp, start - START_FRAME
 #	la	gp, _gp
	sw	zero, START_FRAME - 4(sp)	# Zero out old ra for debugger
	jal	mach_init			# mach_init(argc, argv, envp)
	sw	zero, START_FRAME - 8(sp)	# Zero out old fp for debugger

	li	t0, MACH_SR_COP_1_BIT		# Disable interrupts and
	mtc0	t0, MACH_COP_0_STATUS_REG	#   enable the coprocessor
	li	sp, KERNELSTACK - START_FRAME	# switch to standard stack
	mfc0	t0, MACH_COP_0_PRID		# read processor ID register
	cfc1	t1, MACH_FPC_ID			# read FPU ID register
	sw	t0, cpu				# save PRID register
	sw	t1, fpu				# save FPU ID register
	jal	main				# main()
	nop

/* proc[1] == /etc/init now running here; run icode */
	li	v0, PSL_USERSET
	mtc0	v0, MACH_COP_0_STATUS_REG	# switch to user mode
	li	v0, VM_MIN_ADDRESS
	j	v0				# jump to icode
	rfe

/*
 * GCC2 seems to want to call __main in main() for some reason.
 */
LEAF(__main)
	j	ra
	nop
END(__main)

/*
 * This code is copied to user data space as the first program to run.
 * Basically, it just calls execve();
 */
	.globl	icode
icode:
	li	a1, VM_MIN_ADDRESS + (9 * 4)	# address of 'icode_argv'
	addu	a0, a1, (3 * 4)		# address of 'icode_fname'
	move	a2, zero		# no environment
	li	v0, 59			# code for execve system call
	syscall
	li	v0, 1			# code for exit system call
	syscall				# execve failed: call exit()
1:	b	1b			# loop if exit returns
	nop
icode_argv:
	.word	VM_MIN_ADDRESS + (12 * 4)	# address of 'icode_fname'
	.word	VM_MIN_ADDRESS + (15 * 4)	# address of 'icodeEnd'
	.word	0
icode_fname:
	.asciiz	"/sbin/init"		# occupies 3 words
	.align	2
	.globl	icodeEnd
icodeEnd:

	.data
	.align	2
	.globl	szicode
szicode:
	.word	(9 + 3 + 3) * 4		# compute icodeEnd - icode
	.text

/*
 * This code is copied the user's stack for returning from signal handlers
 * (see sendsig() and sigreturn()). We have to compute the address
 * of the sigcontext struct for the sigreturn call.
 */
	.globl	sigcode
sigcode:
	addu	a0, sp, 16		# address of sigcontext
	li	v0, SYS_sigreturn	# sigreturn(scp)
	syscall
	break	0			# just in case sigreturn fails
	.globl	esigcode
esigcode:

/*
 * Primitives
 */

/*
 * This table is indexed by u.u_pcb.pcb_onfault in trap().
 * The reason for using this table rather than storing an address in
 * u.u_pcb.pcb_onfault is simply to make the code faster.
 */
	.globl	onfault_table
	.data
	.align	2
onfault_table:
	.word	0		# invalid index number
#define BADERR		1
	.word	baderr
#define COPYERR		2
	.word	copyerr
#define FSWBERR		3
	.word	fswberr
#define FSWINTRBERR	4
	.word	fswintrberr
#ifdef KADB
#define KADBERR		5
	.word	kadberr
#endif
	.text

/*
 * See if access to addr with a len type instruction causes a machine check.
 * len is length of access (1=byte, 2=short, 4=long)
 *
 * badaddr(addr, len)
 *	char *addr;
 *	int len;
 */
LEAF(badaddr)
	li	v0, BADERR
	bne	a1, 1, 2f
	sw	v0, UADDR+U_PCB_ONFAULT
	b	5f
	lbu	v0, (a0)
2:
	bne	a1, 2, 4f
	nop
	b	5f
	lhu	v0, (a0)
4:
	lw	v0, (a0)
5:
	sw	zero, UADDR+U_PCB_ONFAULT
	j	ra
	move	v0, zero		# made it w/o errors
baderr:
	j	ra
	li	v0, 1			# trap sends us here
END(badaddr)

/*
 * netorder = htonl(hostorder)
 * hostorder = ntohl(netorder)
 */
LEAF(htonl)				# a0 = 0x11223344, return 0x44332211
ALEAF(ntohl)
	srl	v1, a0, 24		# v1 = 0x00000011
	sll	v0, a0, 24		# v0 = 0x44000000
	or	v0, v0, v1
	and	v1, a0, 0xff00
	sll	v1, v1, 8		# v1 = 0x00330000
	or	v0, v0, v1
	srl	v1, a0, 8
	and	v1, v1, 0xff00		# v1 = 0x00002200
	j	ra
	or	v0, v0, v1
END(htonl)

/*
 * netorder = htons(hostorder)
 * hostorder = ntohs(netorder)
 */
LEAF(htons)
ALEAF(ntohs)
	srl	v0, a0, 8
	and	v0, v0, 0xff
	sll	v1, a0, 8
	and	v1, v1, 0xff00
	j	ra
	or	v0, v0, v1
END(htons)

/*
 * bit = ffs(value)
 */
LEAF(ffs)
	beq	a0, zero, 2f
	move	v0, zero
1:
	and	v1, a0, 1		# bit set?
	addu	v0, v0, 1
	beq	v1, zero, 1b		# no, continue
	srl	a0, a0, 1
2:
	j	ra
	nop
END(ffs)

/*
 * strlen(str)
 */
LEAF(strlen)
	addu	v1, a0, 1
1:
	lb	v0, 0(a0)		# get byte from string
	addu	a0, a0, 1		# increment pointer
	bne	v0, zero, 1b		# continue if not end
	nop
	j	ra
	subu	v0, a0, v1		# compute length - 1 for '\0' char
END(strlen)

/*
 * NOTE: this version assumes unsigned chars in order to be "8 bit clean".
 */
LEAF(strcmp)
1:
	lbu	t0, 0(a0)		# get two bytes and compare them
	lbu	t1, 0(a1)
	beq	t0, zero, LessOrEq	# end of first string?
	nop
	bne	t0, t1, NotEq
	nop
	lbu	t0, 1(a0)		# unroll loop
	lbu	t1, 1(a1)
	beq	t0, zero, LessOrEq	# end of first string?
	addu	a0, a0, 2
	beq	t0, t1, 1b
	addu	a1, a1, 2
NotEq:
	j	ra
	subu	v0, t0, t1
LessOrEq:
	j	ra
	subu	v0, zero, t1
END(strcmp)

/*
 * bzero(s1, n)
 */
LEAF(bzero)
ALEAF(blkclr)
	blt	a1, 12, smallclr	# small amount to clear?
	subu	a3, zero, a0		# compute # bytes to word align address
	and	a3, a3, 3
	beq	a3, zero, 1f		# skip if word aligned
	subu	a1, a1, a3		# subtract from remaining count
	swr	zero, 0(a0)		# clear 1, 2, or 3 bytes to align
	addu	a0, a0, a3
1:
	and	v0, a1, 3		# compute number of words left
	subu	a3, a1, v0
	move	a1, v0
	addu	a3, a3, a0		# compute ending address
2:
	addu	a0, a0, 4		# clear words
	bne	a0, a3, 2b		#  unrolling loop does not help
	sw	zero, -4(a0)		#  since we are limited by memory speed
smallclr:
	ble	a1, zero, 2f
	addu	a3, a1, a0		# compute ending address
1:
	addu	a0, a0, 1		# clear bytes
	bne	a0, a3, 1b
	sb	zero, -1(a0)
2:
	j	ra
	nop
END(bzero)

/*
 * bcmp(s1, s2, n)
 */
LEAF(bcmp)
	blt	a2, 16, smallcmp	# is it worth any trouble?
	xor	v0, a0, a1		# compare low two bits of addresses
	and	v0, v0, 3
	subu	a3, zero, a1		# compute # bytes to word align address
	bne	v0, zero, unalignedcmp	# not possible to align addresses
	and	a3, a3, 3

	beq	a3, zero, 1f
	subu	a2, a2, a3		# subtract from remaining count
	move	v0, v1			# init v0,v1 so unmodified bytes match
	lwr	v0, 0(a0)		# read 1, 2, or 3 bytes
	lwr	v1, 0(a1)
	addu	a1, a1, a3
	bne	v0, v1, nomatch
	addu	a0, a0, a3
1:
	and	a3, a2, ~3		# compute number of whole words left
	subu	a2, a2, a3		#   which has to be >= (16-3) & ~3
	addu	a3, a3, a0		# compute ending address
2:
	lw	v0, 0(a0)		# compare words
	lw	v1, 0(a1)
	addu	a0, a0, 4
	bne	v0, v1, nomatch
	addu	a1, a1, 4
	bne	a0, a3, 2b
	nop
	b	smallcmp		# finish remainder
	nop
unalignedcmp:
	beq	a3, zero, 2f
	subu	a2, a2, a3		# subtract from remaining count
	addu	a3, a3, a0		# compute ending address
1:
	lbu	v0, 0(a0)		# compare bytes until a1 word aligned
	lbu	v1, 0(a1)
	addu	a0, a0, 1
	bne	v0, v1, nomatch
	addu	a1, a1, 1
	bne	a0, a3, 1b
	nop
2:
	and	a3, a2, ~3		# compute number of whole words left
	subu	a2, a2, a3		#   which has to be >= (16-3) & ~3
	addu	a3, a3, a0		# compute ending address
3:
	lwr	v0, 0(a0)		# compare words a0 unaligned, a1 aligned
	lwl	v0, 3(a0)
	lw	v1, 0(a1)
	addu	a0, a0, 4
	bne	v0, v1, nomatch
	addu	a1, a1, 4
	bne	a0, a3, 3b
	nop
smallcmp:
	ble	a2, zero, match
	addu	a3, a2, a0		# compute ending address
1:
	lbu	v0, 0(a0)
	lbu	v1, 0(a1)
	addu	a0, a0, 1
	bne	v0, v1, nomatch
	addu	a1, a1, 1
	bne	a0, a3, 1b
	nop
match:
	j	ra
	move	v0, zero
nomatch:
	j	ra
	li	v0, 1
END(bcmp)

/*
 * {ov}bcopy(from, to, len)
 */
LEAF(bcopy)
ALEAF(ovbcopy)
	addu	t0, a0, a2		# t0 = end of s1 region
	sltu	t1, a1, t0
	sltu	t2, a0, a1
	and	t1, t1, t2		# t1 = true if from < to < (from+len)
	beq	t1, zero, forward	# non overlapping, do forward copy
	slt	t2, a2, 12		# check for small copy

	ble	a2, zero, 2f
	addu	t1, a1, a2		# t1 = end of to region
1:
	lb	v0, -1(t0)		# copy bytes backwards,
	subu	t0, t0, 1		#   doesnt happen often so do slow way
	subu	t1, t1, 1
	bne	t0, a0, 1b
	sb	v0, 0(t1)
2:
	j	ra
	nop
forward:
	bne	t2, zero, smallcpy	# do a small bcopy
	xor	v0, a0, a1		# compare low two bits of addresses
	and	v0, v0, 3
	subu	a3, zero, a1		# compute # bytes to word align address
	beq	v0, zero, aligned	# addresses can be word aligned
	and	a3, a3, 3

	beq	a3, zero, 1f
	subu	a2, a2, a3		# subtract from remaining count
	lwr	v0, 0(a0)		# get next 4 bytes (unaligned)
	lwl	v0, 3(a0)
	addu	a0, a0, a3
	swr	v0, 0(a1)		# store 1, 2, or 3 bytes to align a1
	addu	a1, a1, a3
1:
	and	v0, a2, 3		# compute number of words left
	subu	a3, a2, v0
	move	a2, v0
	addu	a3, a3, a0		# compute ending address
2:
	lwr	v0, 0(a0)		# copy words a0 unaligned, a1 aligned
	lwl	v0, 3(a0)
	addu	a0, a0, 4
	addu	a1, a1, 4
	bne	a0, a3, 2b
	sw	v0, -4(a1)
	b	smallcpy
	nop
aligned:
	beq	a3, zero, 1f
	subu	a2, a2, a3		# subtract from remaining count
	lwr	v0, 0(a0)		# copy 1, 2, or 3 bytes to align
	addu	a0, a0, a3
	swr	v0, 0(a1)
	addu	a1, a1, a3
1:
	and	v0, a2, 3		# compute number of whole words left
	subu	a3, a2, v0
	move	a2, v0
	addu	a3, a3, a0		# compute ending address
2:
	lw	v0, 0(a0)		# copy words
	addu	a0, a0, 4
	addu	a1, a1, 4
	bne	a0, a3, 2b
	sw	v0, -4(a1)
smallcpy:
	ble	a2, zero, 2f
	addu	a3, a2, a0		# compute ending address
1:
	lbu	v0, 0(a0)		# copy bytes
	addu	a0, a0, 1
	addu	a1, a1, 1
	bne	a0, a3, 1b
	sb	v0, -1(a1)
2:
	j	ra
	move	v0, zero
END(bcopy)

/*
 * Copy a null terminated string within the kernel address space.
 * Maxlength may be null if count not wanted.
 *	copystr(fromaddr, toaddr, maxlength, &lencopied)
 *		caddr_t fromaddr;
 *		caddr_t toaddr;
 *		u_int maxlength;
 *		u_int *lencopied;
 */
LEAF(copystr)
	move	t2, a2			# Save the number of bytes
1:
	lbu	t0, 0(a0)
	subu	a2, a2, 1
	beq	t0, zero, 2f
	sb	t0, 0(a1)
	addu	a0, a0, 1
	bne	a2, zero, 1b
	addu	a1, a1, 1
2:
	beq	a3, zero, 3f
	subu	a2, t2, a2		# compute length copied
	sw	a2, 0(a3)
3:
	j	ra
	move	v0, zero
END(copystr)

/*
 * Copy a null terminated string from the user address space into
 * the kernel address space.
 *
 *	copyinstr(fromaddr, toaddr, maxlength, &lencopied)
 *		caddr_t fromaddr;
 *		caddr_t toaddr;
 *		u_int maxlength;
 *		u_int *lencopied;
 */
NON_LEAF(copyinstr, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	sw	ra, STAND_RA_OFFSET(sp)
	blt	a0, zero, copyerr	# make sure address is in user space
	li	v0, COPYERR
	jal	copystr
	sw	v0, UADDR+U_PCB_ONFAULT
	lw	ra, STAND_RA_OFFSET(sp)
	sw	zero, UADDR+U_PCB_ONFAULT
	addu	sp, sp, STAND_FRAME_SIZE
	j	ra
	move	v0, zero
END(copyinstr)

/*
 * Copy a null terminated string from the kernel address space into
 * the user address space.
 *
 *	copyoutstr(fromaddr, toaddr, maxlength, &lencopied)
 *		caddr_t fromaddr;
 *		caddr_t toaddr;
 *		u_int maxlength;
 *		u_int *lencopied;
 */
NON_LEAF(copyoutstr, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	sw	ra, STAND_RA_OFFSET(sp)
	blt	a1, zero, copyerr	# make sure address is in user space
	li	v0, COPYERR
	jal	copystr
	sw	v0, UADDR+U_PCB_ONFAULT
	lw	ra, STAND_RA_OFFSET(sp)
	sw	zero, UADDR+U_PCB_ONFAULT
	addu	sp, sp, STAND_FRAME_SIZE
	j	ra
	move	v0, zero
END(copyoutstr)

/*
 * Copy specified amount of data from user space into the kernel
 *	copyin(from, to, len)
 *		caddr_t *from;	(user source address)
 *		caddr_t *to;	(kernel destination address)
 *		unsigned len;
 */
NON_LEAF(copyin, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	sw	ra, STAND_RA_OFFSET(sp)
	blt	a0, zero, copyerr	# make sure address is in user space
	li	v0, COPYERR
	jal	bcopy
	sw	v0, UADDR+U_PCB_ONFAULT
	lw	ra, STAND_RA_OFFSET(sp)
	sw	zero, UADDR+U_PCB_ONFAULT
	addu	sp, sp, STAND_FRAME_SIZE
	j	ra
	move	v0, zero
END(copyin)

/*
 * Copy specified amount of data from kernel to the user space
 *	copyout(from, to, len)
 *		caddr_t *from;	(kernel source address)
 *		caddr_t *to;	(user destination address)
 *		unsigned len;
 */
NON_LEAF(copyout, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	sw	ra, STAND_RA_OFFSET(sp)
	blt	a1, zero, copyerr	# make sure address is in user space
	li	v0, COPYERR
	jal	bcopy
	sw	v0, UADDR+U_PCB_ONFAULT
	lw	ra, STAND_RA_OFFSET(sp)
	sw	zero, UADDR+U_PCB_ONFAULT
	addu	sp, sp, STAND_FRAME_SIZE
	j	ra
	move	v0, zero
END(copyout)

LEAF(copyerr)
	lw	ra, STAND_RA_OFFSET(sp)
	sw	zero, UADDR+U_PCB_ONFAULT
	addu	sp, sp, STAND_FRAME_SIZE
	j	ra
	li	v0, EFAULT		# return error
END(copyerr)

/*
 * Copy data to the DMA buffer.
 * The DMA bufffer can only be written one short at a time
 * (and takes ~14 cycles).
 *
 *	CopyToBuffer(src, dst, length)
 *		u_short *src;	NOTE: must be short aligned
 *		u_short *dst;
 *		int length;
 */
LEAF(CopyToBuffer)
	blez	a2, 2f
	nop
1:
	lhu	t0, 0(a0)		# read 2 bytes of data
	subu	a2, a2, 2
	addu	a0, a0, 2
	addu	a1, a1, 4
	bgtz	a2, 1b
	sh	t0, -4(a1)		# write 2 bytes of data to buffer
2:
	j	ra
	nop
END(CopyToBuffer)

/*
 * Copy data from the DMA buffer.
 * The DMA bufffer can only be read one short at a time
 * (and takes ~12 cycles).
 *
 *	CopyFromBuffer(src, dst, length)
 *		u_short *src;
 *		char *dst;
 *		int length;
 */
LEAF(CopyFromBuffer)
	and	t0, a1, 1		# test for aligned dst
	beq	t0, zero, 3f
	nop
	blt	a2, 2, 7f		# at least 2 bytes to copy?
	nop
1:
	lhu	t0, 0(a0)		# read 2 bytes of data from buffer
	addu	a0, a0, 4		# keep buffer pointer word aligned
	addu	a1, a1, 2
	subu	a2, a2, 2
	sb	t0, -2(a1)
	srl	t0, t0, 8
	bge	a2, 2, 1b
	sb	t0, -1(a1)
3:
	blt	a2, 2, 7f		# at least 2 bytes to copy?
	nop
6:
	lhu	t0, 0(a0)		# read 2 bytes of data from buffer
	addu	a0, a0, 4		# keep buffer pointer word aligned
	addu	a1, a1, 2
	subu	a2, a2, 2
	bge	a2, 2, 6b
	sh	t0, -2(a1)
7:
	ble	a2, zero, 9f		# done?
	nop
	lhu	t0, 0(a0)		# copy one more byte
	nop
	sb	t0, 0(a1)
9:
	j	ra
	nop
END(CopyFromBuffer)

/*
 * Copy the kernel stack to the new process and save the current context so
 * the new process will return nonzero when it is resumed by cpu_swtch().
 *
 *	copykstack(up)
 *		struct user *up;
 */
LEAF(copykstack)
	subu	v0, sp, UADDR		# compute offset into stack
	addu	v0, v0, a0		# v0 = new stack address
	move	v1, sp			# v1 = old stack address
	li	t1, KERNELSTACK
1:
	lw	t0, 0(v1)		# copy stack data
	addu	v1, v1, 4
	sw	t0, 0(v0)
	bne	v1, t1, 1b
	addu	v0, v0, 4
	/* FALLTHROUGH */
/*
 * Save registers and state so we can do a longjmp later.
 * Note: this only works if p != curproc since
 * cpu_swtch() will copy over pcb_context.
 *
 *	savectx(up)
 *		struct user *up;
 */
ALEAF(savectx)
	sw	s0, U_PCB_CONTEXT+0(a0)
	sw	s1, U_PCB_CONTEXT+4(a0)
	sw	s2, U_PCB_CONTEXT+8(a0)
	sw	s3, U_PCB_CONTEXT+12(a0)
	mfc0	v0, MACH_COP_0_STATUS_REG
	sw	s4, U_PCB_CONTEXT+16(a0)
	sw	s5, U_PCB_CONTEXT+20(a0)
	sw	s6, U_PCB_CONTEXT+24(a0)
	sw	s7, U_PCB_CONTEXT+28(a0)
	sw	sp, U_PCB_CONTEXT+32(a0)
	sw	s8, U_PCB_CONTEXT+36(a0)
	sw	ra, U_PCB_CONTEXT+40(a0)
	sw	v0, U_PCB_CONTEXT+44(a0)
	j	ra
	move	v0, zero
END(copykstack)

/*
 * _whichqs tells which of the 32 queues _qs
 * have processes in them.  Setrq puts processes into queues, Remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

/*
 * setrq(p)
 *	proc *p;
 *
 * Call should be made at splclock(), and p->p_stat should be SRUN.
 */
NON_LEAF(setrq, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	lw	t0, P_RLINK(a0)		## firewall: p->p_rlink must be 0
	sw	ra, STAND_RA_OFFSET(sp)	##
	beq	t0, zero, 1f		##
	lbu	t0, P_PRI(a0)		# put on queue which is p->p_pri / 4
	PANIC("setrq")			##
1:
	li	t1, 1			# compute corresponding bit
	srl	t0, t0, 2		# compute index into 'whichqs'
	sll	t1, t1, t0
	lw	t2, whichqs		# set corresponding bit
	nop
	or	t2, t2, t1
	sw	t2, whichqs
	sll	t0, t0, 3		# compute index into 'qs'
	la	t1, qs
	addu	t0, t0, t1		# t0 = qp = &qs[pri >> 2]
	lw	t1, P_RLINK(t0)		# t1 = qp->ph_rlink
	sw	t0, P_LINK(a0)		# p->p_link = qp
	sw	t1, P_RLINK(a0)		# p->p_rlink = qp->ph_rlink
	sw	a0, P_LINK(t1)		# p->p_rlink->p_link = p;
	sw	a0, P_RLINK(t0)		# qp->ph_rlink = p
	j	ra
	addu	sp, sp, STAND_FRAME_SIZE
END(setrq)

/*
 * Remrq(p)
 *
 * Call should be made at splclock().
 */
NON_LEAF(remrq, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	lbu	t0, P_PRI(a0)		# get from queue which is p->p_pri / 4
	li	t1, 1			# compute corresponding bit
	srl	t0, t0, 2		# compute index into 'whichqs'
	lw	t2, whichqs		# check corresponding bit
	sll	t1, t1, t0
	and	v0, t2, t1
	sw	ra, STAND_RA_OFFSET(sp)	##
	bne	v0, zero, 1f		##
	lw	v0, P_RLINK(a0)		# v0 = p->p_rlink
	PANIC("remrq")			## it wasnt recorded to be on its q
1:
	lw	v1, P_LINK(a0)		# v1 = p->p_link
	nop
	sw	v1, P_LINK(v0)		# p->p_rlink->p_link = p->p_link;
	sw	v0, P_RLINK(v1)		# p->p_link->p_rlink = p->r_rlink
	sll	t0, t0, 3		# compute index into 'qs'
	la	v0, qs
	addu	t0, t0, v0		# t0 = qp = &qs[pri >> 2]
	lw	v0, P_LINK(t0)		# check if queue empty
	nop
	bne	v0, t0, 2f		# No. qp->ph_link != qp
	nop
	xor	t2, t2, t1		# clear corresponding bit in 'whichqs'
	sw	t2, whichqs
2:
	sw	zero, P_RLINK(a0)	## for firewall checking
	j	ra
	addu	sp, sp, STAND_FRAME_SIZE
END(remrq)

/*
 * swtch_exit()
 *
 * At exit of a process, do a cpu_swtch for the last time.
 * The mapping of the pcb at p->p_addr has already been deleted,
 * and the memory for the pcb+stack has been freed.
 * All interrupts should be blocked at this point.
 */
LEAF(swtch_exit)
	la	v1, nullproc			# save state into garbage proc
	lw	t0, P_UPTE+0(v1)		# t0 = first u. pte
	lw	t1, P_UPTE+4(v1)		# t1 = 2nd u. pte
	li	v0, UADDR			# v0 = first HI entry
	mtc0	zero, MACH_COP_0_TLB_INDEX	# set the index register
	mtc0	v0, MACH_COP_0_TLB_HI		# init high entry
	mtc0	t0, MACH_COP_0_TLB_LOW		# init low entry
	li	t0, 1 << VMMACH_TLB_INDEX_SHIFT
	tlbwi					# Write the TLB entry.
	addu	v0, v0, NBPG			# 2nd HI entry
	mtc0	t0, MACH_COP_0_TLB_INDEX	# set the index register
	mtc0	v0, MACH_COP_0_TLB_HI		# init high entry
	mtc0	t1, MACH_COP_0_TLB_LOW		# init low entry
	sw	zero, curproc
	tlbwi					# Write the TLB entry.
	b	cpu_swtch
	li	sp, KERNELSTACK - START_FRAME	# switch to standard stack
END(swtch_exit)

/*
 * When no processes are on the runq, cpu_swtch branches to idle
 * to wait for something to come ready.
 * Note: this is really a part of cpu_swtch() but defined here for kernel
 * profiling.
 */
LEAF(idle)
	li	t0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
	mtc0	t0, MACH_COP_0_STATUS_REG	# enable all interrupts
	sw	zero, curproc			# set curproc NULL for stats
1:
	lw	t0, whichqs			# look for non-empty queue
	nop
	beq	t0, zero, 1b
	nop
	b	sw1
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable all interrupts
END(idle)

/*
 * cpu_swtch()
 * Find the highest priority process and resume it.
 */
NON_LEAF(cpu_swtch, STAND_FRAME_SIZE, ra)
	sw	sp, UADDR+U_PCB_CONTEXT+32	# save old sp
	subu	sp, sp, STAND_FRAME_SIZE
	sw	ra, STAND_RA_OFFSET(sp)
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	lw	t2, cnt+V_SWTCH			# for statistics
	lw	t1, whichqs			# look for non-empty queue
	sw	s0, UADDR+U_PCB_CONTEXT+0	# do a 'savectx()'
	sw	s1, UADDR+U_PCB_CONTEXT+4
	sw	s2, UADDR+U_PCB_CONTEXT+8
	sw	s3, UADDR+U_PCB_CONTEXT+12
	mfc0	t0, MACH_COP_0_STATUS_REG	# t0 = saved status register
	sw	s4, UADDR+U_PCB_CONTEXT+16
	sw	s5, UADDR+U_PCB_CONTEXT+20
	sw	s6, UADDR+U_PCB_CONTEXT+24
	sw	s7, UADDR+U_PCB_CONTEXT+28
	sw	s8, UADDR+U_PCB_CONTEXT+36
	sw	ra, UADDR+U_PCB_CONTEXT+40	# save return address
	sw	t0, UADDR+U_PCB_CONTEXT+44	# save status register
	addu	t2, t2, 1
	sw	t2, cnt+V_SWTCH
	beq	t1, zero, idle			# if none, idle
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable all interrupts
sw1:
	nop					# wait for intrs disabled
	nop
	lw	t0, whichqs			# look for non-empty queue
	li	t2, -1				# t2 = lowest bit set
	beq	t0, zero, idle			# if none, idle
	move	t3, t0				# t3 = saved whichqs
1:
	addu	t2, t2, 1
	and	t1, t0, 1			# bit set?
	beq	t1, zero, 1b
	srl	t0, t0, 1			# try next bit
/*
 * Remove process from queue.
 */
	sll	t0, t2, 3
	la	t1, qs
	addu	t0, t0, t1			# t0 = qp = &qs[highbit]
	lw	a0, P_LINK(t0)			# a0 = p = highest pri process
	nop
	lw	v0, P_LINK(a0)			# v0 = p->p_link
	bne	t0, a0, 2f			# make sure something in queue
	sw	v0, P_LINK(t0)			# qp->ph_link = p->p_link;
	PANIC("cpu_swtch")			# nothing in queue
2:
	sw	t0, P_RLINK(v0)			# p->p_link->p_rlink = qp
	bne	v0, t0, 3f			# queue still not empty
	sw	zero, P_RLINK(a0)		## for firewall checking
	li	v1, 1				# compute bit in 'whichqs'
	sll	v1, v1, t2
	xor	t3, t3, v1			# clear bit in 'whichqs'
	sw	t3, whichqs
3:
/*
 * Switch to new context.
 */
	sw	zero, want_resched
	jal	pmap_alloc_tlbpid		# v0 = TLB PID
	move	s0, a0				# save p
	move	a0, s0				# restore p
	sw	a0, curproc			# set curproc
	sll	v0, v0, VMMACH_TLB_PID_SHIFT	# v0 = aligned PID
	lw	t0, P_UPTE+0(a0)		# t0 = first u. pte
	lw	t1, P_UPTE+4(a0)		# t1 = 2nd u. pte
	or	v0, v0, UADDR			# v0 = first HI entry
/*
 * Resume process indicated by the pte's for its u struct
 * NOTE: This is hard coded to UPAGES == 2.
 * Also, there should be no TLB faults at this point.
 */
	mtc0	zero, MACH_COP_0_TLB_INDEX	# set the index register
	mtc0	v0, MACH_COP_0_TLB_HI		# init high entry
	mtc0	t0, MACH_COP_0_TLB_LOW		# init low entry
	li	t0, 1 << VMMACH_TLB_INDEX_SHIFT
	tlbwi					# Write the TLB entry.
	addu	v0, v0, NBPG			# 2nd HI entry
	mtc0	t0, MACH_COP_0_TLB_INDEX	# set the index register
	mtc0	v0, MACH_COP_0_TLB_HI		# init high entry
	mtc0	t1, MACH_COP_0_TLB_LOW		# init low entry
	nop
	tlbwi					# Write the TLB entry.
/*
 * Now running on new u struct.
 * Restore registers and return.
 */
	lw	v0, UADDR+U_PCB_CONTEXT+44	# restore kernel context
	lw	ra, UADDR+U_PCB_CONTEXT+40
	lw	s0, UADDR+U_PCB_CONTEXT+0
	lw	s1, UADDR+U_PCB_CONTEXT+4
	lw	s2, UADDR+U_PCB_CONTEXT+8
	lw	s3, UADDR+U_PCB_CONTEXT+12
	lw	s4, UADDR+U_PCB_CONTEXT+16
	lw	s5, UADDR+U_PCB_CONTEXT+20
	lw	s6, UADDR+U_PCB_CONTEXT+24
	lw	s7, UADDR+U_PCB_CONTEXT+28
	lw	sp, UADDR+U_PCB_CONTEXT+32
	lw	s8, UADDR+U_PCB_CONTEXT+36
	mtc0	v0, MACH_COP_0_STATUS_REG
	j	ra
	li	v0, 1				# possible return to 'savectx()'
END(cpu_swtch)

/*
 * {fu,su},{ibyte,isword,iword}, fetch or store a byte, short or word to
 * user text space.
 * {fu,su},{byte,sword,word}, fetch or store a byte, short or word to
 * user data space.
 */
LEAF(fuword)
ALEAF(fuiword)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	lw	v0, 0(a0)		# fetch word
	j	ra
	sw	zero, UADDR+U_PCB_ONFAULT
END(fuword)

LEAF(fusword)
ALEAF(fuisword)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	lhu	v0, 0(a0)		# fetch short
	j	ra
	sw	zero, UADDR+U_PCB_ONFAULT
END(fusword)

LEAF(fubyte)
ALEAF(fuibyte)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	lbu	v0, 0(a0)		# fetch byte
	j	ra
	sw	zero, UADDR+U_PCB_ONFAULT
END(fubyte)

LEAF(suword)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	sw	a1, 0(a0)		# store word
	sw	zero, UADDR+U_PCB_ONFAULT
	j	ra
	move	v0, zero
END(suword)

/*
 * Have to flush instruction cache afterwards.
 */
LEAF(suiword)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	sw	a1, 0(a0)		# store word
	sw	zero, UADDR+U_PCB_ONFAULT
	move	v0, zero
	b	MachFlushICache		# NOTE: this should not clobber v0!
	li	a1, 4			# size of word
END(suiword)

/*
 * Will have to flush the instruction cache if byte merging is done in hardware.
 */
LEAF(susword)
ALEAF(suisword)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	sh	a1, 0(a0)		# store short
	sw	zero, UADDR+U_PCB_ONFAULT
	j	ra
	move	v0, zero
END(susword)

LEAF(subyte)
ALEAF(suibyte)
	blt	a0, zero, fswberr	# make sure address is in user space
	li	v0, FSWBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	sb	a1, 0(a0)		# store byte
	sw	zero, UADDR+U_PCB_ONFAULT
	j	ra
	move	v0, zero
END(subyte)

LEAF(fswberr)
	j	ra
	li	v0, -1
END(fswberr)

/*
 * fuswintr and suswintr are just like fusword and susword except that if
 * the page is not in memory or would cause a trap, then we return an error.
 * The important thing is to prevent sleep() and swtch().
 */
LEAF(fuswintr)
	blt	a0, zero, fswintrberr	# make sure address is in user space
	li	v0, FSWINTRBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	lhu	v0, 0(a0)		# fetch short
	j	ra
	sw	zero, UADDR+U_PCB_ONFAULT
END(fuswintr)

LEAF(suswintr)
	blt	a0, zero, fswintrberr	# make sure address is in user space
	li	v0, FSWINTRBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	sh	a1, 0(a0)		# store short
	sw	zero, UADDR+U_PCB_ONFAULT
	j	ra
	move	v0, zero
END(suswintr)

LEAF(fswintrberr)
	j	ra
	li	v0, -1
END(fswintrberr)

/*
 * Insert 'p' after 'q'.
 *	_insque(p, q)
 *		caddr_t p, q;
 */
LEAF(_insque)
	lw	v0, 0(a1)		# v0 = q->next
	sw	a1, 4(a0)		# p->prev = q
	sw	v0, 0(a0)		# p->next = q->next
	sw	a0, 4(v0)		# q->next->prev = p
	j	ra
	sw	a0, 0(a1)		# q->next = p
END(_insque)

/*
 * Remove item 'p' from queue.
 *	_remque(p)
 *		caddr_t p;
 */
LEAF(_remque)
	lw	v0, 0(a0)		# v0 = p->next
	lw	v1, 4(a0)		# v1 = p->prev
	nop
	sw	v0, 0(v1)		# p->prev->next = p->next
	j	ra
	sw	v1, 4(v0)		# p->next->prev = p->prev
END(_remque)

/*
 * This code is copied to the UTLB exception vector address to
 * handle user level TLB translation misses.
 * NOTE: This code must be relocatable!!!
 */
	.globl	MachUTLBMiss
MachUTLBMiss:
	.set	noat
	mfc0	k0, MACH_COP_0_BAD_VADDR	# get the virtual address
	lw	k1, UADDR+U_PCB_SEGTAB		# get the current segment table
	bltz	k0, 1f				# R3000 chip bug
	srl	k0, k0, SEGSHIFT		# compute segment table index
	sll	k0, k0, 2
	addu	k1, k1, k0
	mfc0	k0, MACH_COP_0_BAD_VADDR	# get the virtual address
	lw	k1, 0(k1)			# get pointer to segment map
	srl	k0, k0, PGSHIFT - 2		# compute segment map index
	andi	k0, k0, (NPTEPG - 1) << 2
	beq	k1, zero, 2f			# invalid segment map
	addu	k1, k1, k0			# index into segment map
	lw	k0, 0(k1)			# get page PTE
	nop
	beq	k0, zero, 2f			# dont load invalid entries
	mtc0	k0, MACH_COP_0_TLB_LOW
	mfc0	k1, MACH_COP_0_EXC_PC		# get return address
	tlbwr					# update TLB
	j	k1
	rfe
1:
	mfc0	k1, MACH_COP_0_EXC_PC		# get return address
	nop
	j	k1
	rfe
2:
	j	SlowFault			# handle the rest
	nop
	.set	at
	.globl	MachUTLBMissEnd
MachUTLBMissEnd:

/*
 * This code is copied to the general exception vector address to
 * handle all execptions except RESET and UTLBMiss.
 * NOTE: This code must be relocatable!!!
 */
	.globl	MachException
MachException:
/*
 * Find out what mode we came from and jump to the proper handler.
 */
	.set	noat
	mfc0	k0, MACH_COP_0_STATUS_REG	# Get the status register
	mfc0	k1, MACH_COP_0_CAUSE_REG	# Get the cause register value.
	and	k0, k0, MACH_SR_KU_PREV		# test for user mode
	sll	k0, k0, 3			# shift user bit for cause index
	and	k1, k1, MACH_CR_EXC_CODE	# Mask out the cause bits.
	or	k1, k1, k0			# change index to user table
1:
	la	k0, machExceptionTable		# get base of the jump table
	addu	k0, k0, k1			# Get the address of the
						#  function entry.  Note that
						#  the cause is already
						#  shifted left by 2 bits so
						#  we dont have to shift.
	lw	k0, 0(k0)			# Get the function address
	nop
	j	k0				# Jump to the function.
	nop
	.set	at
	.globl	MachExceptionEnd
MachExceptionEnd:

/*
 * We couldn't find a TLB entry.
 * Find out what mode we came from and call the appropriate handler.
 */
SlowFault:
	.set	noat
	mfc0	k0, MACH_COP_0_STATUS_REG
	nop
	and	k0, k0, MACH_SR_KU_PREV
	bne	k0, zero, MachUserGenException
	nop
	.set	at
/*
 * Fall though ...
 */

/*----------------------------------------------------------------------------
 *
 * MachKernGenException --
 *
 *	Handle an exception from kernel mode.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */

/*
 * The kernel exception stack contains 18 saved general registers,
 * the status register and the multiply lo and high registers.
 * In addition, we set this up for linkage conventions.
 */
#define KERN_REG_SIZE		(18 * 4)
#define KERN_REG_OFFSET		(STAND_FRAME_SIZE)
#define KERN_SR_OFFSET		(STAND_FRAME_SIZE + KERN_REG_SIZE)
#define KERN_MULT_LO_OFFSET	(STAND_FRAME_SIZE + KERN_REG_SIZE + 4)
#define KERN_MULT_HI_OFFSET	(STAND_FRAME_SIZE + KERN_REG_SIZE + 8)
#define	KERN_EXC_FRAME_SIZE	(STAND_FRAME_SIZE + KERN_REG_SIZE + 12)

NNON_LEAF(MachKernGenException, KERN_EXC_FRAME_SIZE, ra)
	.set	noat
#ifdef KADB
	la	k0, kdbpcb			# save registers for kadb
	sw	s0, (S0 * 4)(k0)
	sw	s1, (S1 * 4)(k0)
	sw	s2, (S2 * 4)(k0)
	sw	s3, (S3 * 4)(k0)
	sw	s4, (S4 * 4)(k0)
	sw	s5, (S5 * 4)(k0)
	sw	s6, (S6 * 4)(k0)
	sw	s7, (S7 * 4)(k0)
	sw	s8, (S8 * 4)(k0)
	sw	gp, (GP * 4)(k0)
	sw	sp, (SP * 4)(k0)
#endif
	subu	sp, sp, KERN_EXC_FRAME_SIZE
	.mask	0x80000000, (STAND_RA_OFFSET - KERN_EXC_FRAME_SIZE)
/*
 * Save the relevant kernel registers onto the stack.
 * We don't need to save s0 - s8, sp and gp because
 * the compiler does it for us.
 */
	sw	AT, KERN_REG_OFFSET + 0(sp)
	sw	v0, KERN_REG_OFFSET + 4(sp)
	sw	v1, KERN_REG_OFFSET + 8(sp)
	sw	a0, KERN_REG_OFFSET + 12(sp)
	mflo	v0
	mfhi	v1
	sw	a1, KERN_REG_OFFSET + 16(sp)
	sw	a2, KERN_REG_OFFSET + 20(sp)
	sw	a3, KERN_REG_OFFSET + 24(sp)
	sw	t0, KERN_REG_OFFSET + 28(sp)
	mfc0	a0, MACH_COP_0_STATUS_REG	# First arg is the status reg.
	sw	t1, KERN_REG_OFFSET + 32(sp)
	sw	t2, KERN_REG_OFFSET + 36(sp)
	sw	t3, KERN_REG_OFFSET + 40(sp)
	sw	t4, KERN_REG_OFFSET + 44(sp)
	mfc0	a1, MACH_COP_0_CAUSE_REG	# Second arg is the cause reg.
	sw	t5, KERN_REG_OFFSET + 48(sp)
	sw	t6, KERN_REG_OFFSET + 52(sp)
	sw	t7, KERN_REG_OFFSET + 56(sp)
	sw	t8, KERN_REG_OFFSET + 60(sp)
	mfc0	a2, MACH_COP_0_BAD_VADDR	# Third arg is the fault addr.
	sw	t9, KERN_REG_OFFSET + 64(sp)
	sw	ra, KERN_REG_OFFSET + 68(sp)
	sw	v0, KERN_MULT_LO_OFFSET(sp)
	sw	v1, KERN_MULT_HI_OFFSET(sp)
	mfc0	a3, MACH_COP_0_EXC_PC		# Fourth arg is the pc.
	sw	a0, KERN_SR_OFFSET(sp)
/*
 * Call the exception handler.
 */
	jal	trap
	sw	a3, STAND_RA_OFFSET(sp)		# for debugging
/*
 * Restore registers and return from the exception.
 * v0 contains the return address.
 */
	lw	a0, KERN_SR_OFFSET(sp)
	lw	t0, KERN_MULT_LO_OFFSET(sp)
	lw	t1, KERN_MULT_HI_OFFSET(sp)
	mtc0	a0, MACH_COP_0_STATUS_REG	# Restore the SR, disable intrs
	mtlo	t0
	mthi	t1
	move	k0, v0
	lw	AT, KERN_REG_OFFSET + 0(sp)
	lw	v0, KERN_REG_OFFSET + 4(sp)
	lw	v1, KERN_REG_OFFSET + 8(sp)
	lw	a0, KERN_REG_OFFSET + 12(sp)
	lw	a1, KERN_REG_OFFSET + 16(sp)
	lw	a2, KERN_REG_OFFSET + 20(sp)
	lw	a3, KERN_REG_OFFSET + 24(sp)
	lw	t0, KERN_REG_OFFSET + 28(sp)
	lw	t1, KERN_REG_OFFSET + 32(sp)
	lw	t2, KERN_REG_OFFSET + 36(sp)
	lw	t3, KERN_REG_OFFSET + 40(sp)
	lw	t4, KERN_REG_OFFSET + 44(sp)
	lw	t5, KERN_REG_OFFSET + 48(sp)
	lw	t6, KERN_REG_OFFSET + 52(sp)
	lw	t7, KERN_REG_OFFSET + 56(sp)
	lw	t8, KERN_REG_OFFSET + 60(sp)
	lw	t9, KERN_REG_OFFSET + 64(sp)
	lw	ra, KERN_REG_OFFSET + 68(sp)
	addu	sp, sp, KERN_EXC_FRAME_SIZE
	j	k0				# Now return from the
	rfe					#  exception.
	.set	at
END(MachKernGenException)

/*----------------------------------------------------------------------------
 *
 * MachUserGenException --
 *
 *	Handle an exception from user mode.
 *
 * Results:
 * 	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
NNON_LEAF(MachUserGenException, STAND_FRAME_SIZE, ra)
	.set	noat
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
/*
 * Save all of the registers except for the kernel temporaries in u.u_pcb.
 */
	sw	AT, UADDR+U_PCB_REGS+(AST * 4)
	sw	v0, UADDR+U_PCB_REGS+(V0 * 4)
	sw	v1, UADDR+U_PCB_REGS+(V1 * 4)
	sw	a0, UADDR+U_PCB_REGS+(A0 * 4)
	mflo	v0
	sw	a1, UADDR+U_PCB_REGS+(A1 * 4)
	sw	a2, UADDR+U_PCB_REGS+(A2 * 4)
	sw	a3, UADDR+U_PCB_REGS+(A3 * 4)
	sw	t0, UADDR+U_PCB_REGS+(T0 * 4)
	mfhi	v1
	sw	t1, UADDR+U_PCB_REGS+(T1 * 4)
	sw	t2, UADDR+U_PCB_REGS+(T2 * 4)
	sw	t3, UADDR+U_PCB_REGS+(T3 * 4)
	sw	t4, UADDR+U_PCB_REGS+(T4 * 4)
	mfc0	a0, MACH_COP_0_STATUS_REG	# First arg is the status reg.
	sw	t5, UADDR+U_PCB_REGS+(T5 * 4)
	sw	t6, UADDR+U_PCB_REGS+(T6 * 4)
	sw	t7, UADDR+U_PCB_REGS+(T7 * 4)
	sw	s0, UADDR+U_PCB_REGS+(S0 * 4)
	mfc0	a1, MACH_COP_0_CAUSE_REG	# Second arg is the cause reg.
	sw	s1, UADDR+U_PCB_REGS+(S1 * 4)
	sw	s2, UADDR+U_PCB_REGS+(S2 * 4)
	sw	s3, UADDR+U_PCB_REGS+(S3 * 4)
	sw	s4, UADDR+U_PCB_REGS+(S4 * 4)
	mfc0	a2, MACH_COP_0_BAD_VADDR	# Third arg is the fault addr
	sw	s5, UADDR+U_PCB_REGS+(S5 * 4)
	sw	s6, UADDR+U_PCB_REGS+(S6 * 4)
	sw	s7, UADDR+U_PCB_REGS+(S7 * 4)
	sw	t8, UADDR+U_PCB_REGS+(T8 * 4)
	mfc0	a3, MACH_COP_0_EXC_PC		# Fourth arg is the pc.
	sw	t9, UADDR+U_PCB_REGS+(T9 * 4)
	sw	gp, UADDR+U_PCB_REGS+(GP * 4)
	sw	sp, UADDR+U_PCB_REGS+(SP * 4)
	sw	s8, UADDR+U_PCB_REGS+(S8 * 4)
	li	sp, KERNELSTACK - STAND_FRAME_SIZE	# switch to kernel SP
	sw	ra, UADDR+U_PCB_REGS+(RA * 4)
	sw	v0, UADDR+U_PCB_REGS+(MULLO * 4)
	sw	v1, UADDR+U_PCB_REGS+(MULHI * 4)
	sw	a0, UADDR+U_PCB_REGS+(SR * 4)
 #	la	gp, _gp				# switch to kernel GP
	sw	a3, UADDR+U_PCB_REGS+(PC * 4)
	sw	a3, STAND_RA_OFFSET(sp)		# for debugging
	.set	at
	and	t0, a0, ~MACH_SR_COP_1_BIT	# Turn off the FPU.
	.set	noat
/*
 * Call the exception handler.
 */
	jal	trap
	mtc0	t0, MACH_COP_0_STATUS_REG
/*
 * Restore user registers and return. NOTE: interrupts are enabled.
 */
	lw	a0, UADDR+U_PCB_REGS+(SR * 4)
	lw	t0, UADDR+U_PCB_REGS+(MULLO * 4)
	lw	t1, UADDR+U_PCB_REGS+(MULHI * 4)
	mtc0	a0, MACH_COP_0_STATUS_REG	# this should disable interrupts
	mtlo	t0
	mthi	t1
	lw	k0, UADDR+U_PCB_REGS+(PC * 4)
	lw	AT, UADDR+U_PCB_REGS+(AST * 4)
	lw	v0, UADDR+U_PCB_REGS+(V0 * 4)
	lw	v1, UADDR+U_PCB_REGS+(V1 * 4)
	lw	a0, UADDR+U_PCB_REGS+(A0 * 4)
	lw	a1, UADDR+U_PCB_REGS+(A1 * 4)
	lw	a2, UADDR+U_PCB_REGS+(A2 * 4)
	lw	a3, UADDR+U_PCB_REGS+(A3 * 4)
	lw	t0, UADDR+U_PCB_REGS+(T0 * 4)
	lw	t1, UADDR+U_PCB_REGS+(T1 * 4)
	lw	t2, UADDR+U_PCB_REGS+(T2 * 4)
	lw	t3, UADDR+U_PCB_REGS+(T3 * 4)
	lw	t4, UADDR+U_PCB_REGS+(T4 * 4)
	lw	t5, UADDR+U_PCB_REGS+(T5 * 4)
	lw	t6, UADDR+U_PCB_REGS+(T6 * 4)
	lw	t7, UADDR+U_PCB_REGS+(T7 * 4)
	lw	s0, UADDR+U_PCB_REGS+(S0 * 4)
	lw	s1, UADDR+U_PCB_REGS+(S1 * 4)
	lw	s2, UADDR+U_PCB_REGS+(S2 * 4)
	lw	s3, UADDR+U_PCB_REGS+(S3 * 4)
	lw	s4, UADDR+U_PCB_REGS+(S4 * 4)
	lw	s5, UADDR+U_PCB_REGS+(S5 * 4)
	lw	s6, UADDR+U_PCB_REGS+(S6 * 4)
	lw	s7, UADDR+U_PCB_REGS+(S7 * 4)
	lw	t8, UADDR+U_PCB_REGS+(T8 * 4)
	lw	t9, UADDR+U_PCB_REGS+(T9 * 4)
	lw	gp, UADDR+U_PCB_REGS+(GP * 4)
	lw	sp, UADDR+U_PCB_REGS+(SP * 4)
	lw	s8, UADDR+U_PCB_REGS+(S8 * 4)
	lw	ra, UADDR+U_PCB_REGS+(RA * 4)
	j	k0
	rfe
	.set	at
END(MachUserGenException)

/*----------------------------------------------------------------------------
 *
 * MachKernIntr --
 *
 *	Handle an interrupt from kernel mode.
 *	Interrupts use the standard kernel stack.
 *	swtch_exit sets up a kernel stack after exit so interrupts won't fail.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
#define KINTR_REG_OFFSET	(STAND_FRAME_SIZE)
#define KINTR_SR_OFFSET		(STAND_FRAME_SIZE + KERN_REG_SIZE)
#define KINTR_MULT_LO_OFFSET	(STAND_FRAME_SIZE + KERN_REG_SIZE + 4)
#define KINTR_MULT_HI_OFFSET	(STAND_FRAME_SIZE + KERN_REG_SIZE + 8)
#define	KINTR_FRAME_SIZE	(STAND_FRAME_SIZE + KERN_REG_SIZE + 12)

NNON_LEAF(MachKernIntr, KINTR_FRAME_SIZE, ra)
	.set	noat
	subu	sp, sp, KINTR_FRAME_SIZE	# allocate stack frame
	.mask	0x80000000, (STAND_RA_OFFSET - KINTR_FRAME_SIZE)
/*
 * Save the relevant kernel registers onto the stack.
 * We don't need to save s0 - s8, sp and gp because
 * the compiler does it for us.
 */
	sw	AT, KINTR_REG_OFFSET + 0(sp)
	sw	v0, KINTR_REG_OFFSET + 4(sp)
	sw	v1, KINTR_REG_OFFSET + 8(sp)
	sw	a0, KINTR_REG_OFFSET + 12(sp)
	mflo	v0
	mfhi	v1
	sw	a1, KINTR_REG_OFFSET + 16(sp)
	sw	a2, KINTR_REG_OFFSET + 20(sp)
	sw	a3, KINTR_REG_OFFSET + 24(sp)
	sw	t0, KINTR_REG_OFFSET + 28(sp)
	mfc0	a0, MACH_COP_0_STATUS_REG	# First arg is the status reg.
	sw	t1, KINTR_REG_OFFSET + 32(sp)
	sw	t2, KINTR_REG_OFFSET + 36(sp)
	sw	t3, KINTR_REG_OFFSET + 40(sp)
	sw	t4, KINTR_REG_OFFSET + 44(sp)
	mfc0	a1, MACH_COP_0_CAUSE_REG	# Second arg is the cause reg.
	sw	t5, KINTR_REG_OFFSET + 48(sp)
	sw	t6, KINTR_REG_OFFSET + 52(sp)
	sw	t7, KINTR_REG_OFFSET + 56(sp)
	sw	t8, KINTR_REG_OFFSET + 60(sp)
	mfc0	a2, MACH_COP_0_EXC_PC		# Third arg is the pc.
	sw	t9, KINTR_REG_OFFSET + 64(sp)
	sw	ra, KINTR_REG_OFFSET + 68(sp)
	sw	v0, KINTR_MULT_LO_OFFSET(sp)
	sw	v1, KINTR_MULT_HI_OFFSET(sp)
	sw	a0, KINTR_SR_OFFSET(sp)
/*
 * Call the interrupt handler.
 */
	jal	interrupt
	sw	a2, STAND_RA_OFFSET(sp)		# for debugging
/*
 * Restore registers and return from the interrupt.
 */
	lw	a0, KINTR_SR_OFFSET(sp)
	lw	t0, KINTR_MULT_LO_OFFSET(sp)
	lw	t1, KINTR_MULT_HI_OFFSET(sp)
	mtc0	a0, MACH_COP_0_STATUS_REG	# Restore the SR, disable intrs
	mtlo	t0
	mthi	t1
	lw	k0, STAND_RA_OFFSET(sp)
	lw	AT, KINTR_REG_OFFSET + 0(sp)
	lw	v0, KINTR_REG_OFFSET + 4(sp)
	lw	v1, KINTR_REG_OFFSET + 8(sp)
	lw	a0, KINTR_REG_OFFSET + 12(sp)
	lw	a1, KINTR_REG_OFFSET + 16(sp)
	lw	a2, KINTR_REG_OFFSET + 20(sp)
	lw	a3, KINTR_REG_OFFSET + 24(sp)
	lw	t0, KINTR_REG_OFFSET + 28(sp)
	lw	t1, KINTR_REG_OFFSET + 32(sp)
	lw	t2, KINTR_REG_OFFSET + 36(sp)
	lw	t3, KINTR_REG_OFFSET + 40(sp)
	lw	t4, KINTR_REG_OFFSET + 44(sp)
	lw	t5, KINTR_REG_OFFSET + 48(sp)
	lw	t6, KINTR_REG_OFFSET + 52(sp)
	lw	t7, KINTR_REG_OFFSET + 56(sp)
	lw	t8, KINTR_REG_OFFSET + 60(sp)
	lw	t9, KINTR_REG_OFFSET + 64(sp)
	lw	ra, KINTR_REG_OFFSET + 68(sp)
	addu	sp, sp, KINTR_FRAME_SIZE
	j	k0				# Now return from the
	rfe					#  interrupt.
	.set	at
END(MachKernIntr)

/*----------------------------------------------------------------------------
 *
 * MachUserIntr --
 *
 *	Handle an interrupt from user mode.
 *	Note: we save minimal state in the u.u_pcb struct and use the standard
 *	kernel stack since there has to be a u page if we came from user mode.
 *	If there is a pending software interrupt, then save the remaining state
 *	and call softintr(). This is all because if we call swtch() inside
 *	interrupt(), not all the user registers have been saved in u.u_pcb.
 *
 * Results:
 * 	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
NNON_LEAF(MachUserIntr, STAND_FRAME_SIZE, ra)
	.set	noat
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
/*
 * Save the relevant user registers into the u.u_pcb struct.
 * We don't need to save s0 - s8 because
 * the compiler does it for us.
 */
	sw	AT, UADDR+U_PCB_REGS+(AST * 4)
	sw	v0, UADDR+U_PCB_REGS+(V0 * 4)
	sw	v1, UADDR+U_PCB_REGS+(V1 * 4)
	sw	a0, UADDR+U_PCB_REGS+(A0 * 4)
	mflo	v0
	mfhi	v1
	sw	a1, UADDR+U_PCB_REGS+(A1 * 4)
	sw	a2, UADDR+U_PCB_REGS+(A2 * 4)
	sw	a3, UADDR+U_PCB_REGS+(A3 * 4)
	sw	t0, UADDR+U_PCB_REGS+(T0 * 4)
	mfc0	a0, MACH_COP_0_STATUS_REG	# First arg is the status reg.
	sw	t1, UADDR+U_PCB_REGS+(T1 * 4)
	sw	t2, UADDR+U_PCB_REGS+(T2 * 4)
	sw	t3, UADDR+U_PCB_REGS+(T3 * 4)
	sw	t4, UADDR+U_PCB_REGS+(T4 * 4)
	mfc0	a1, MACH_COP_0_CAUSE_REG	# Second arg is the cause reg.
	sw	t5, UADDR+U_PCB_REGS+(T5 * 4)
	sw	t6, UADDR+U_PCB_REGS+(T6 * 4)
	sw	t7, UADDR+U_PCB_REGS+(T7 * 4)
	sw	t8, UADDR+U_PCB_REGS+(T8 * 4)
	mfc0	a2, MACH_COP_0_EXC_PC		# Third arg is the pc.
	sw	t9, UADDR+U_PCB_REGS+(T9 * 4)
	sw	gp, UADDR+U_PCB_REGS+(GP * 4)
	sw	sp, UADDR+U_PCB_REGS+(SP * 4)
	sw	ra, UADDR+U_PCB_REGS+(RA * 4)
	li	sp, KERNELSTACK - STAND_FRAME_SIZE	# switch to kernel SP
	sw	v0, UADDR+U_PCB_REGS+(MULLO * 4)
	sw	v1, UADDR+U_PCB_REGS+(MULHI * 4)
	sw	a0, UADDR+U_PCB_REGS+(SR * 4)
	sw	a2, UADDR+U_PCB_REGS+(PC * 4)
 #	la	gp, _gp				# switch to kernel GP
	.set	at
	and	t0, a0, ~MACH_SR_COP_1_BIT	# Turn off the FPU.
	.set	noat
	mtc0	t0, MACH_COP_0_STATUS_REG
/*
 * Call the interrupt handler.
 */
	jal	interrupt
	sw	a2, STAND_RA_OFFSET(sp)		# for debugging
/*
 * Restore registers and return from the interrupt.
 */
	lw	a0, UADDR+U_PCB_REGS+(SR * 4)
	lw	v0, astpending			# any pending interrupts?
	mtc0	a0, MACH_COP_0_STATUS_REG	# Restore the SR, disable intrs
	bne	v0, zero, 1f			# dont restore, call softintr
	lw	t0, UADDR+U_PCB_REGS+(MULLO * 4)
	lw	t1, UADDR+U_PCB_REGS+(MULHI * 4)
	lw	k0, UADDR+U_PCB_REGS+(PC * 4)
	lw	AT, UADDR+U_PCB_REGS+(AST * 4)
	lw	v0, UADDR+U_PCB_REGS+(V0 * 4)
	lw	v1, UADDR+U_PCB_REGS+(V1 * 4)
	lw	a0, UADDR+U_PCB_REGS+(A0 * 4)
	lw	a1, UADDR+U_PCB_REGS+(A1 * 4)
	lw	a2, UADDR+U_PCB_REGS+(A2 * 4)
	lw	a3, UADDR+U_PCB_REGS+(A3 * 4)
	mtlo	t0
	mthi	t1
	lw	t0, UADDR+U_PCB_REGS+(T0 * 4)
	lw	t1, UADDR+U_PCB_REGS+(T1 * 4)
	lw	t2, UADDR+U_PCB_REGS+(T2 * 4)
	lw	t3, UADDR+U_PCB_REGS+(T3 * 4)
	lw	t4, UADDR+U_PCB_REGS+(T4 * 4)
	lw	t5, UADDR+U_PCB_REGS+(T5 * 4)
	lw	t6, UADDR+U_PCB_REGS+(T6 * 4)
	lw	t7, UADDR+U_PCB_REGS+(T7 * 4)
	lw	t8, UADDR+U_PCB_REGS+(T8 * 4)
	lw	t9, UADDR+U_PCB_REGS+(T9 * 4)
	lw	gp, UADDR+U_PCB_REGS+(GP * 4)
	lw	sp, UADDR+U_PCB_REGS+(SP * 4)
	lw	ra, UADDR+U_PCB_REGS+(RA * 4)
	j	k0				# Now return from the
	rfe					#  interrupt.

1:
/*
 * We have pending software interrupts; save remaining user state in u.u_pcb.
 */
	sw	s0, UADDR+U_PCB_REGS+(S0 * 4)
	sw	s1, UADDR+U_PCB_REGS+(S1 * 4)
	sw	s2, UADDR+U_PCB_REGS+(S2 * 4)
	sw	s3, UADDR+U_PCB_REGS+(S3 * 4)
	sw	s4, UADDR+U_PCB_REGS+(S4 * 4)
	sw	s5, UADDR+U_PCB_REGS+(S5 * 4)
	sw	s6, UADDR+U_PCB_REGS+(S6 * 4)
	sw	s7, UADDR+U_PCB_REGS+(S7 * 4)
	sw	s8, UADDR+U_PCB_REGS+(S8 * 4)
	li	t0, MACH_HARD_INT_MASK | MACH_SR_INT_ENA_CUR
/*
 * Call the software interrupt handler.
 */
	jal	softintr
	mtc0	t0, MACH_COP_0_STATUS_REG	# enable interrupts (spl0)
/*
 * Restore user registers and return. NOTE: interrupts are enabled.
 */
	lw	a0, UADDR+U_PCB_REGS+(SR * 4)
	lw	t0, UADDR+U_PCB_REGS+(MULLO * 4)
	lw	t1, UADDR+U_PCB_REGS+(MULHI * 4)
	mtc0	a0, MACH_COP_0_STATUS_REG	# this should disable interrupts
	mtlo	t0
	mthi	t1
	lw	k0, UADDR+U_PCB_REGS+(PC * 4)
	lw	AT, UADDR+U_PCB_REGS+(AST * 4)
	lw	v0, UADDR+U_PCB_REGS+(V0 * 4)
	lw	v1, UADDR+U_PCB_REGS+(V1 * 4)
	lw	a0, UADDR+U_PCB_REGS+(A0 * 4)
	lw	a1, UADDR+U_PCB_REGS+(A1 * 4)
	lw	a2, UADDR+U_PCB_REGS+(A2 * 4)
	lw	a3, UADDR+U_PCB_REGS+(A3 * 4)
	lw	t0, UADDR+U_PCB_REGS+(T0 * 4)
	lw	t1, UADDR+U_PCB_REGS+(T1 * 4)
	lw	t2, UADDR+U_PCB_REGS+(T2 * 4)
	lw	t3, UADDR+U_PCB_REGS+(T3 * 4)
	lw	t4, UADDR+U_PCB_REGS+(T4 * 4)
	lw	t5, UADDR+U_PCB_REGS+(T5 * 4)
	lw	t6, UADDR+U_PCB_REGS+(T6 * 4)
	lw	t7, UADDR+U_PCB_REGS+(T7 * 4)
	lw	s0, UADDR+U_PCB_REGS+(S0 * 4)
	lw	s1, UADDR+U_PCB_REGS+(S1 * 4)
	lw	s2, UADDR+U_PCB_REGS+(S2 * 4)
	lw	s3, UADDR+U_PCB_REGS+(S3 * 4)
	lw	s4, UADDR+U_PCB_REGS+(S4 * 4)
	lw	s5, UADDR+U_PCB_REGS+(S5 * 4)
	lw	s6, UADDR+U_PCB_REGS+(S6 * 4)
	lw	s7, UADDR+U_PCB_REGS+(S7 * 4)
	lw	t8, UADDR+U_PCB_REGS+(T8 * 4)
	lw	t9, UADDR+U_PCB_REGS+(T9 * 4)
	lw	gp, UADDR+U_PCB_REGS+(GP * 4)
	lw	sp, UADDR+U_PCB_REGS+(SP * 4)
	lw	s8, UADDR+U_PCB_REGS+(S8 * 4)
	lw	ra, UADDR+U_PCB_REGS+(RA * 4)
	j	k0
	rfe
	.set	at
END(MachUserIntr)

#if 0
/*----------------------------------------------------------------------------
 *
 * MachTLBModException --
 *
 *	Handle a TLB modified exception.
 *	The BaddVAddr, Context, and EntryHi registers contain the failed
 *	virtual address.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
NLEAF(MachTLBModException)
	.set	noat
	tlbp					# find the TLB entry
	mfc0	k0, MACH_COP_0_TLB_LOW		# get the physical address
	mfc0	k1, MACH_COP_0_TLB_INDEX	# check to be sure its valid
	or	k0, k0, VMMACH_TLB_MOD_BIT	# update TLB
	blt	k1, zero, 4f			# not found!!!
	mtc0	k0, MACH_COP_0_TLB_LOW
	li	k1, MACH_CACHED_MEMORY_ADDR
	subu	k0, k0, k1
	srl	k0, k0, VMMACH_TLB_PHYS_PAGE_SHIFT
	la	k1, pmap_attributes
	addu	k0, k0, k1
	lbu	k1, 0(k0)			# fetch old value
	nop
	or	k1, k1, 1			# set modified bit
	sb	k1, 0(k0)			# save new value
	mfc0	k0, MACH_COP_0_EXC_PC		# get return address
	nop
	j	k0
	rfe
4:
	break	0				# panic
	.set	at
END(MachTLBModException)
#endif

/*----------------------------------------------------------------------------
 *
 * MachTLBMissException --
 *
 *	Handle a TLB miss exception from kernel mode.
 *	The BaddVAddr, Context, and EntryHi registers contain the failed
 *	virtual address.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
NLEAF(MachTLBMissException)
	.set	noat
	mfc0	k0, MACH_COP_0_BAD_VADDR	# get the fault address
	li	k1, VM_MIN_KERNEL_ADDRESS	# compute index
	subu	k0, k0, k1
	lw	k1, Sysmapsize			# index within range?
	srl	k0, k0, PGSHIFT
	sltu	k1, k0, k1
	beq	k1, zero, 1f			# No. check for valid stack
	nop
	lw	k1, Sysmap
	sll	k0, k0, 2			# compute offset from index
	addu	k1, k1, k0
	lw	k0, 0(k1)			# get PTE entry
	mfc0	k1, MACH_COP_0_EXC_PC		# get return address
	mtc0	k0, MACH_COP_0_TLB_LOW		# save PTE entry
	and	k0, k0, PG_V			# check for valid entry
	beq	k0, zero, MachKernGenException	# PTE invalid
	nop
	tlbwr					# update TLB
	j	k1
	rfe

1:
	subu	k0, sp, UADDR + 0x200		# check to see if we have a
	sltiu	k0, UPAGES*NBPG - 0x200		#  valid kernel stack
	bne	k0, zero, MachKernGenException	# Go panic
	nop

	la	a0, start - START_FRAME - 8	# set sp to a valid place
	sw	sp, 24(a0)
	move	sp, a0
	la	a0, 1f
	mfc0	a2, MACH_COP_0_STATUS_REG
	mfc0	a3, MACH_COP_0_CAUSE_REG
	mfc0	a1, MACH_COP_0_EXC_PC
	sw	a2, 16(sp)
	sw	a3, 20(sp)
	sw	sp, 24(sp)
	move	a2, ra
	jal	printf
	mfc0	a3, MACH_COP_0_BAD_VADDR
	.data
1:
	.asciiz	"ktlbmiss: PC %x RA %x ADR %x\nSR %x CR %x SP %x\n"
	.text

	la	sp, start - START_FRAME		# set sp to a valid place
	PANIC("kernel stack overflow")
	.set	at
END(MachTLBMissException)

/*
 * Set/clear software interrupt routines.
 */

LEAF(setsoftclock)
	mfc0	v0, MACH_COP_0_CAUSE_REG	# read cause register
	nop
	or	v0, v0, MACH_SOFT_INT_MASK_0	# set soft clock interrupt
	mtc0	v0, MACH_COP_0_CAUSE_REG	# save it
	j	ra
	nop
END(setsoftclock)

LEAF(clearsoftclock)
	mfc0	v0, MACH_COP_0_CAUSE_REG	# read cause register
	nop
	and	v0, v0, ~MACH_SOFT_INT_MASK_0	# clear soft clock interrupt
	mtc0	v0, MACH_COP_0_CAUSE_REG	# save it
	j	ra
	nop
END(clearsoftclock)

LEAF(setsoftnet)
	mfc0	v0, MACH_COP_0_CAUSE_REG	# read cause register
	nop
	or	v0, v0, MACH_SOFT_INT_MASK_1	# set soft net interrupt
	mtc0	v0, MACH_COP_0_CAUSE_REG	# save it
	j	ra
	nop
END(setsoftnet)

LEAF(clearsoftnet)
	mfc0	v0, MACH_COP_0_CAUSE_REG	# read cause register
	nop
	and	v0, v0, ~MACH_SOFT_INT_MASK_1	# clear soft net interrupt
	mtc0	v0, MACH_COP_0_CAUSE_REG	# save it
	j	ra
	nop
END(clearsoftnet)

/*
 * Set/change interrupt priority routines.
 */

LEAF(MachEnableIntr)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	nop
	or	v0, v0, MACH_SR_INT_ENA_CUR
	mtc0	v0, MACH_COP_0_STATUS_REG	# enable all interrupts
	j	ra
	nop
END(MachEnableIntr)

LEAF(spl0)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	nop
	or	t0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
	mtc0	t0, MACH_COP_0_STATUS_REG	# enable all interrupts
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(spl0)

LEAF(splsoftclock)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~MACH_SOFT_INT_MASK_0	# disable soft clock
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(splsoftclock)

LEAF(Mach_spl0)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~(MACH_INT_MASK_0|MACH_SOFT_INT_MASK_1|MACH_SOFT_INT_MASK_0)
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(Mach_spl0)

LEAF(Mach_spl1)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~(MACH_INT_MASK_1|MACH_SOFT_INT_MASK_0|MACH_SOFT_INT_MASK_1)
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(Mach_spl1)

LEAF(Mach_spl2)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~(MACH_INT_MASK_2|MACH_SOFT_INT_MASK_1|MACH_SOFT_INT_MASK_0)
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(Mach_spl2)

LEAF(Mach_spl3)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~(MACH_INT_MASK_3|MACH_SOFT_INT_MASK_1|MACH_SOFT_INT_MASK_0)
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(Mach_spl3)

/*
 * We define an alternate entry point after mcount is called so it
 * can be used in mcount without causeing a recursive loop.
 */
LEAF(splhigh)
ALEAF(_splhigh)
	mfc0	v0, MACH_COP_0_STATUS_REG	# read status register
	li	t0, ~MACH_SR_INT_ENA_CUR	# disable all interrupts
	and	t0, t0, v0
	mtc0	t0, MACH_COP_0_STATUS_REG	# save it
	j	ra
	and	v0, v0, (MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
END(splhigh)

/*
 * Restore saved interrupt mask.
 */
LEAF(splx)
ALEAF(_splx)
	mfc0	v0, MACH_COP_0_STATUS_REG
	li	t0, ~(MACH_INT_MASK | MACH_SR_INT_ENA_CUR)
	and	t0, t0, v0
	or	t0, t0, a0
	mtc0	t0, MACH_COP_0_STATUS_REG
	j	ra
	nop
END(splx)

/*----------------------------------------------------------------------------
 *
 * MachEmptyWriteBuffer --
 *
 *	Return when the write buffer is empty.
 *
 *	MachEmptyWriteBuffer()
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachEmptyWriteBuffer)
	nop
	nop
	nop
	nop
1:	bc0f	1b
	nop
	j	ra
	nop
END(MachEmptyWriteBuffer)

/*--------------------------------------------------------------------------
 *
 * MachTLBWriteIndexed --
 *
 *	Write the given entry into the TLB at the given index.
 *
 *	MachTLBWriteIndexed(index, highEntry, lowEntry)
 *		int index;
 *		int highEntry;
 *		int lowEntry;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	TLB entry set.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBWriteIndexed)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Save the current PID.

	sll	a0, a0, VMMACH_TLB_INDEX_SHIFT
	mtc0	a0, MACH_COP_0_TLB_INDEX	# Set the index.
	mtc0	a1, MACH_COP_0_TLB_HI		# Set up entry high.
	mtc0	a2, MACH_COP_0_TLB_LOW		# Set up entry low.
	nop
	tlbwi					# Write the TLB

	mtc0	t0, MACH_COP_0_TLB_HI		# Restore the PID.
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBWriteIndexed)

#if 0
/*--------------------------------------------------------------------------
 *
 * MachTLBWriteRandom --
 *
 *	Write the given entry into the TLB at a random location.
 *
 *	MachTLBWriteRandom(highEntry, lowEntry)
 *		unsigned highEntry;
 *		unsigned lowEntry;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	TLB entry set.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBWriteRandom)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	v0, MACH_COP_0_TLB_HI		# Save the current PID.
	nop

	mtc0	a0, MACH_COP_0_TLB_HI		# Set up entry high.
	mtc0	a1, MACH_COP_0_TLB_LOW		# Set up entry low.
	nop
	tlbwr					# Write the TLB

	mtc0	v0, MACH_COP_0_TLB_HI		# Restore the PID.
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBWriteRandom)
#endif

/*--------------------------------------------------------------------------
 *
 * MachSetPID --
 *
 *	Write the given pid into the TLB pid reg.
 *
 *	MachSetPID(pid)
 *		int pid;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PID set in the entry hi register.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachSetPID)
	sll	a0, a0, VMMACH_TLB_PID_SHIFT	# put PID in right spot
	mtc0	a0, MACH_COP_0_TLB_HI		# Write the hi reg value
	j	ra
	nop
END(MachSetPID)

/*--------------------------------------------------------------------------
 *
 * MachTLBFlush --
 *
 *	Flush the "random" entries from the TLB.
 *
 *	MachTLBFlush()
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The TLB is flushed.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBFlush)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Save the PID
	li	t1, MACH_CACHED_MEMORY_ADDR	# invalid address
	mtc0	t1, MACH_COP_0_TLB_HI		# Mark entry high as invalid
	mtc0	zero, MACH_COP_0_TLB_LOW	# Zero out low entry.
/*
 * Align the starting value (t1) and the upper bound (t2).
 */
	li	t1, VMMACH_FIRST_RAND_ENTRY << VMMACH_TLB_INDEX_SHIFT
	li	t2, VMMACH_NUM_TLB_ENTRIES << VMMACH_TLB_INDEX_SHIFT
1:
	mtc0	t1, MACH_COP_0_TLB_INDEX	# Set the index register.
	addu	t1, t1, 1 << VMMACH_TLB_INDEX_SHIFT	# Increment index.
	bne	t1, t2, 1b
	tlbwi					# Write the TLB entry.

	mtc0	t0, MACH_COP_0_TLB_HI		# Restore the PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBFlush)

#if 0
/*--------------------------------------------------------------------------
 *
 * MachTLBFlushPID --
 *
 *	Flush all entries with the given PID from the TLB.
 *
 *	MachTLBFlushPID(pid)
 *		int pid;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	All entries corresponding to this PID are flushed.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBFlushPID)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Save the current PID
	sll	a0, a0, VMMACH_TLB_PID_SHIFT	# Align the pid to flush.
/*
 * Align the starting value (t1) and the upper bound (t2).
 */
	li	t1, VMMACH_FIRST_RAND_ENTRY << VMMACH_TLB_INDEX_SHIFT
	li	t2, VMMACH_NUM_TLB_ENTRIES << VMMACH_TLB_INDEX_SHIFT
	mtc0	t1, MACH_COP_0_TLB_INDEX	# Set the index register
1:
	addu	t1, t1, 1 << VMMACH_TLB_INDEX_SHIFT	# Increment index.
	tlbr					# Read from the TLB
	mfc0	t4, MACH_COP_0_TLB_HI		# Fetch the hi register.
	nop
	and	t4, t4, VMMACH_TLB_PID		# compare PIDs
	bne	t4, a0, 2f
	li	v0, MACH_CACHED_MEMORY_ADDR	# invalid address
	mtc0	v0, MACH_COP_0_TLB_HI		# Mark entry high as invalid
	mtc0	zero, MACH_COP_0_TLB_LOW	# Zero out low entry.
	nop
	tlbwi					# Write the entry.
2:
	bne	t1, t2, 1b
	mtc0	t1, MACH_COP_0_TLB_INDEX	# Set the index register

	mtc0	t0, MACH_COP_0_TLB_HI		# restore PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBFlushPID)
#endif

/*--------------------------------------------------------------------------
 *
 * MachTLBFlushAddr --
 *
 *	Flush any TLB entries for the given address and TLB PID.
 *
 *	MachTLBFlushAddr(highreg)
 *		unsigned highreg;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The process's page is flushed from the TLB.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBFlushAddr)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Get current PID
	nop

	mtc0	a0, MACH_COP_0_TLB_HI		# look for addr & PID
	nop
	tlbp					# Probe for the entry.
	mfc0	v0, MACH_COP_0_TLB_INDEX	# See what we got
	li	t1, MACH_CACHED_MEMORY_ADDR	# Load invalid entry.
	bltz	v0, 1f				# index < 0 => !found
	mtc0	t1, MACH_COP_0_TLB_HI		# Mark entry high as invalid
	mtc0	zero, MACH_COP_0_TLB_LOW	# Zero out low entry.
	nop
	tlbwi
1:
	mtc0	t0, MACH_COP_0_TLB_HI		# restore PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBFlushAddr)

/*--------------------------------------------------------------------------
 *
 * MachTLBUpdate --
 *
 *	Update the TLB if highreg is found; otherwise, enter the data.
 *
 *	MachTLBUpdate(highreg, lowreg)
 *		unsigned highreg, lowreg;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBUpdate)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Save current PID
	nop					# 2 cycles before intr disabled
	mtc0	a0, MACH_COP_0_TLB_HI		# init high reg.
	nop
	tlbp					# Probe for the entry.
	mfc0	v0, MACH_COP_0_TLB_INDEX	# See what we got
	mtc0	a1, MACH_COP_0_TLB_LOW		# init low reg.
	bltz	v0, 1f				# index < 0 => !found
	sra	v0, v0, VMMACH_TLB_INDEX_SHIFT	# convert index to regular num
	b	2f
	tlbwi					# update slot found
1:
	mtc0	a0, MACH_COP_0_TLB_HI		# init high reg.
	nop
	tlbwr					# enter into a random slot
2:
	mtc0	t0, MACH_COP_0_TLB_HI		# restore PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBUpdate)

#if defined(DEBUG)
/*--------------------------------------------------------------------------
 *
 * MachTLBFind --
 *
 *	Search the TLB for the given entry.
 *
 *	MachTLBFind(hi)
 *		unsigned hi;
 *
 * Results:
 *	Returns a value >= 0 if the entry was found (the index).
 *	Returns a value < 0 if the entry was not found.
 *
 * Side effects:
 *	tlbhi and tlblo will contain the TLB entry found.
 *
 *--------------------------------------------------------------------------
 */
	.comm	tlbhi, 4
	.comm	tlblo, 4
LEAF(MachTLBFind)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Get current PID
	nop
	mtc0	a0, MACH_COP_0_TLB_HI		# Set up entry high.
	nop
	tlbp					# Probe for the entry.
	mfc0	v0, MACH_COP_0_TLB_INDEX	# See what we got
	nop
	bltz	v0, 1f				# not found
	nop
	tlbr					# read TLB
	mfc0	t1, MACH_COP_0_TLB_HI		# See what we got
	mfc0	t2, MACH_COP_0_TLB_LOW		# See what we got
	sw	t1, tlbhi
	sw	t2, tlblo
	srl	v0, v0, VMMACH_TLB_INDEX_SHIFT	# convert index to regular num
1:
	mtc0	t0, MACH_COP_0_TLB_HI		# Restore current PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBFind)

/*--------------------------------------------------------------------------
 *
 * MachTLBRead --
 *
 *	Read the TLB entry.
 *
 *	MachTLBRead(entry)
 *		unsigned entry;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	tlbhi and tlblo will contain the TLB entry found.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBRead)
	mfc0	v1, MACH_COP_0_STATUS_REG	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts
	mfc0	t0, MACH_COP_0_TLB_HI		# Get current PID

	sll	a0, a0, VMMACH_TLB_INDEX_SHIFT
	mtc0	a0, MACH_COP_0_TLB_INDEX	# Set the index register
	nop
	tlbr					# Read from the TLB
	mfc0	t3, MACH_COP_0_TLB_HI		# fetch the hi entry
	mfc0	t4, MACH_COP_0_TLB_LOW		# fetch the low entry
	sw	t3, tlbhi
	sw	t4, tlblo

	mtc0	t0, MACH_COP_0_TLB_HI		# restore PID
	j	ra
	mtc0	v1, MACH_COP_0_STATUS_REG	# Restore the status register
END(MachTLBRead)

/*--------------------------------------------------------------------------
 *
 * MachTLBGetPID --
 *
 *	MachTLBGetPID()
 *
 * Results:
 *	Returns the current TLB pid reg.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------------------
 */
LEAF(MachTLBGetPID)
	mfc0	v0, MACH_COP_0_TLB_HI		# get PID
	nop
	and	v0, v0, VMMACH_TLB_PID		# mask off PID
	j	ra
	srl	v0, v0, VMMACH_TLB_PID_SHIFT	# put PID in right spot
END(MachTLBGetPID)

/*
 * Return the current value of the cause register.
 */
LEAF(MachGetCauseReg)
	mfc0	v0, MACH_COP_0_CAUSE_REG
	j	ra
	nop
END(MachGetCauseReg)
#endif /* DEBUG */

/*----------------------------------------------------------------------------
 *
 * MachSwitchFPState --
 *
 *	Save the current state into 'from' and restore it from 'to'.
 *
 *	MachSwitchFPState(from, to)
 *		struct proc *from;
 *		struct user *to;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachSwitchFPState)
	mfc0	t1, MACH_COP_0_STATUS_REG	# Save old SR
	li	t0, MACH_SR_COP_1_BIT		# enable the coprocessor
	mtc0	t0, MACH_COP_0_STATUS_REG

	beq	a0, zero, 1f			# skip save if NULL pointer
	nop
/*
 * First read out the status register to make sure that all FP operations
 * have completed.
 */
	lw	a0, P_ADDR(a0)			# get pointer to pcb for proc
	cfc1	t0, MACH_FPC_CSR		# stall til FP done
	cfc1	t0, MACH_FPC_CSR		# now get status
	li	t3, ~MACH_SR_COP_1_BIT
	lw	t2, U_PCB_REGS+(PS * 4)(a0)	# get CPU status register
	sw	t0, U_PCB_FPREGS+(32 * 4)(a0)	# save FP status
	and	t2, t2, t3			# clear COP_1 enable bit
	sw	t2, U_PCB_REGS+(PS * 4)(a0)	# save new status register
/*
 * Save the floating point registers.
 */
	swc1	$f0, U_PCB_FPREGS+(0 * 4)(a0)
	swc1	$f1, U_PCB_FPREGS+(1 * 4)(a0)
	swc1	$f2, U_PCB_FPREGS+(2 * 4)(a0)
	swc1	$f3, U_PCB_FPREGS+(3 * 4)(a0)
	swc1	$f4, U_PCB_FPREGS+(4 * 4)(a0)
	swc1	$f5, U_PCB_FPREGS+(5 * 4)(a0)
	swc1	$f6, U_PCB_FPREGS+(6 * 4)(a0)
	swc1	$f7, U_PCB_FPREGS+(7 * 4)(a0)
	swc1	$f8, U_PCB_FPREGS+(8 * 4)(a0)
	swc1	$f9, U_PCB_FPREGS+(9 * 4)(a0)
	swc1	$f10, U_PCB_FPREGS+(10 * 4)(a0)
	swc1	$f11, U_PCB_FPREGS+(11 * 4)(a0)
	swc1	$f12, U_PCB_FPREGS+(12 * 4)(a0)
	swc1	$f13, U_PCB_FPREGS+(13 * 4)(a0)
	swc1	$f14, U_PCB_FPREGS+(14 * 4)(a0)
	swc1	$f15, U_PCB_FPREGS+(15 * 4)(a0)
	swc1	$f16, U_PCB_FPREGS+(16 * 4)(a0)
	swc1	$f17, U_PCB_FPREGS+(17 * 4)(a0)
	swc1	$f18, U_PCB_FPREGS+(18 * 4)(a0)
	swc1	$f19, U_PCB_FPREGS+(19 * 4)(a0)
	swc1	$f20, U_PCB_FPREGS+(20 * 4)(a0)
	swc1	$f21, U_PCB_FPREGS+(21 * 4)(a0)
	swc1	$f22, U_PCB_FPREGS+(22 * 4)(a0)
	swc1	$f23, U_PCB_FPREGS+(23 * 4)(a0)
	swc1	$f24, U_PCB_FPREGS+(24 * 4)(a0)
	swc1	$f25, U_PCB_FPREGS+(25 * 4)(a0)
	swc1	$f26, U_PCB_FPREGS+(26 * 4)(a0)
	swc1	$f27, U_PCB_FPREGS+(27 * 4)(a0)
	swc1	$f28, U_PCB_FPREGS+(28 * 4)(a0)
	swc1	$f29, U_PCB_FPREGS+(29 * 4)(a0)
	swc1	$f30, U_PCB_FPREGS+(30 * 4)(a0)
	swc1	$f31, U_PCB_FPREGS+(31 * 4)(a0)

1:
/*
 *  Restore the floating point registers.
 */
	lw	t0, U_PCB_FPREGS+(32 * 4)(a1)	# get status register
	lwc1	$f0, U_PCB_FPREGS+(0 * 4)(a1)
	lwc1	$f1, U_PCB_FPREGS+(1 * 4)(a1)
	lwc1	$f2, U_PCB_FPREGS+(2 * 4)(a1)
	lwc1	$f3, U_PCB_FPREGS+(3 * 4)(a1)
	lwc1	$f4, U_PCB_FPREGS+(4 * 4)(a1)
	lwc1	$f5, U_PCB_FPREGS+(5 * 4)(a1)
	lwc1	$f6, U_PCB_FPREGS+(6 * 4)(a1)
	lwc1	$f7, U_PCB_FPREGS+(7 * 4)(a1)
	lwc1	$f8, U_PCB_FPREGS+(8 * 4)(a1)
	lwc1	$f9, U_PCB_FPREGS+(9 * 4)(a1)
	lwc1	$f10, U_PCB_FPREGS+(10 * 4)(a1)
	lwc1	$f11, U_PCB_FPREGS+(11 * 4)(a1)
	lwc1	$f12, U_PCB_FPREGS+(12 * 4)(a1)
	lwc1	$f13, U_PCB_FPREGS+(13 * 4)(a1)
	lwc1	$f14, U_PCB_FPREGS+(14 * 4)(a1)
	lwc1	$f15, U_PCB_FPREGS+(15 * 4)(a1)
	lwc1	$f16, U_PCB_FPREGS+(16 * 4)(a1)
	lwc1	$f17, U_PCB_FPREGS+(17 * 4)(a1)
	lwc1	$f18, U_PCB_FPREGS+(18 * 4)(a1)
	lwc1	$f19, U_PCB_FPREGS+(19 * 4)(a1)
	lwc1	$f20, U_PCB_FPREGS+(20 * 4)(a1)
	lwc1	$f21, U_PCB_FPREGS+(21 * 4)(a1)
	lwc1	$f22, U_PCB_FPREGS+(22 * 4)(a1)
	lwc1	$f23, U_PCB_FPREGS+(23 * 4)(a1)
	lwc1	$f24, U_PCB_FPREGS+(24 * 4)(a1)
	lwc1	$f25, U_PCB_FPREGS+(25 * 4)(a1)
	lwc1	$f26, U_PCB_FPREGS+(26 * 4)(a1)
	lwc1	$f27, U_PCB_FPREGS+(27 * 4)(a1)
	lwc1	$f28, U_PCB_FPREGS+(28 * 4)(a1)
	lwc1	$f29, U_PCB_FPREGS+(29 * 4)(a1)
	lwc1	$f30, U_PCB_FPREGS+(30 * 4)(a1)
	lwc1	$f31, U_PCB_FPREGS+(31 * 4)(a1)

	and	t0, t0, ~MACH_FPC_EXCEPTION_BITS
	ctc1	t0, MACH_FPC_CSR
	nop

	mtc0	t1, MACH_COP_0_STATUS_REG	# Restore the status register.
	j	ra
	nop
END(MachSwitchFPState)

/*----------------------------------------------------------------------------
 *
 * MachSaveCurFPState --
 *
 *	Save the current floating point coprocessor state.
 *
 *	MachSaveCurFPState(p)
 *		struct proc *p;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	machFPCurProcPtr is cleared.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachSaveCurFPState)
	lw	a0, P_ADDR(a0)			# get pointer to pcb for proc
	mfc0	t1, MACH_COP_0_STATUS_REG	# Disable interrupts and
	li	t0, MACH_SR_COP_1_BIT		#  enable the coprocessor
	mtc0	t0, MACH_COP_0_STATUS_REG
	sw	zero, machFPCurProcPtr		# indicate state has been saved
/*
 * First read out the status register to make sure that all FP operations
 * have completed.
 */
	lw	t2, U_PCB_REGS+(PS * 4)(a0)	# get CPU status register
	li	t3, ~MACH_SR_COP_1_BIT
	and	t2, t2, t3			# clear COP_1 enable bit
	cfc1	t0, MACH_FPC_CSR		# stall til FP done
	cfc1	t0, MACH_FPC_CSR		# now get status
	sw	t2, U_PCB_REGS+(PS * 4)(a0)	# save new status register
	sw	t0, U_PCB_FPREGS+(32 * 4)(a0)	# save FP status
/*
 * Save the floating point registers.
 */
	swc1	$f0, U_PCB_FPREGS+(0 * 4)(a0)
	swc1	$f1, U_PCB_FPREGS+(1 * 4)(a0)
	swc1	$f2, U_PCB_FPREGS+(2 * 4)(a0)
	swc1	$f3, U_PCB_FPREGS+(3 * 4)(a0)
	swc1	$f4, U_PCB_FPREGS+(4 * 4)(a0)
	swc1	$f5, U_PCB_FPREGS+(5 * 4)(a0)
	swc1	$f6, U_PCB_FPREGS+(6 * 4)(a0)
	swc1	$f7, U_PCB_FPREGS+(7 * 4)(a0)
	swc1	$f8, U_PCB_FPREGS+(8 * 4)(a0)
	swc1	$f9, U_PCB_FPREGS+(9 * 4)(a0)
	swc1	$f10, U_PCB_FPREGS+(10 * 4)(a0)
	swc1	$f11, U_PCB_FPREGS+(11 * 4)(a0)
	swc1	$f12, U_PCB_FPREGS+(12 * 4)(a0)
	swc1	$f13, U_PCB_FPREGS+(13 * 4)(a0)
	swc1	$f14, U_PCB_FPREGS+(14 * 4)(a0)
	swc1	$f15, U_PCB_FPREGS+(15 * 4)(a0)
	swc1	$f16, U_PCB_FPREGS+(16 * 4)(a0)
	swc1	$f17, U_PCB_FPREGS+(17 * 4)(a0)
	swc1	$f18, U_PCB_FPREGS+(18 * 4)(a0)
	swc1	$f19, U_PCB_FPREGS+(19 * 4)(a0)
	swc1	$f20, U_PCB_FPREGS+(20 * 4)(a0)
	swc1	$f21, U_PCB_FPREGS+(21 * 4)(a0)
	swc1	$f22, U_PCB_FPREGS+(22 * 4)(a0)
	swc1	$f23, U_PCB_FPREGS+(23 * 4)(a0)
	swc1	$f24, U_PCB_FPREGS+(24 * 4)(a0)
	swc1	$f25, U_PCB_FPREGS+(25 * 4)(a0)
	swc1	$f26, U_PCB_FPREGS+(26 * 4)(a0)
	swc1	$f27, U_PCB_FPREGS+(27 * 4)(a0)
	swc1	$f28, U_PCB_FPREGS+(28 * 4)(a0)
	swc1	$f29, U_PCB_FPREGS+(29 * 4)(a0)
	swc1	$f30, U_PCB_FPREGS+(30 * 4)(a0)
	swc1	$f31, U_PCB_FPREGS+(31 * 4)(a0)

	mtc0	t1, MACH_COP_0_STATUS_REG	# Restore the status register.
	j	ra
	nop
END(MachSaveCurFPState)

/*----------------------------------------------------------------------------
 *
 * MachFPInterrupt --
 *
 *	Handle a floating point interrupt.
 *
 *	MachFPInterrupt(statusReg, causeReg, pc)
 *		unsigned statusReg;
 *		unsigned causeReg;
 *		unsigned pc;
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
NON_LEAF(MachFPInterrupt, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	mfc0	t0, MACH_COP_0_STATUS_REG
	sw	ra, STAND_RA_OFFSET(sp)
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)

	or	t1, t0, MACH_SR_COP_1_BIT
	mtc0	t1, MACH_COP_0_STATUS_REG
	nop
	nop
	cfc1	t1, MACH_FPC_CSR	# stall til FP done
	cfc1	t1, MACH_FPC_CSR	# now get status
	nop
	sll	t2, t1, (31 - 17)	# unimplemented operation?
	bgez	t2, 3f			# no, normal trap
	nop
/*
 * We got an unimplemented operation trap so
 * fetch the instruction, compute the next PC and emulate the instruction.
 */
	bgez	a1, 1f			# Check the branch delay bit.
	nop
/*
 * The instruction is in the branch delay slot so the branch will have to
 * be emulated to get the resulting PC.
 */
	sw	a2, STAND_FRAME_SIZE + 8(sp)
	li	a0, UADDR+U_PCB_REGS	# first arg is ptr to CPU registers
	move	a1, a2			# second arg is instruction PC
	move	a2, t1			# third arg is floating point CSR
	jal	MachEmulateBranch	# compute PC after branch
	move	a3, zero		# fourth arg is FALSE
/*
 * Now load the floating-point instruction in the branch delay slot
 * to be emulated.
 */
	lw	a2, STAND_FRAME_SIZE + 8(sp)	# restore EXC pc
	b	2f
	lw	a0, 4(a2)			# a0 = coproc instruction
/*
 * This is not in the branch delay slot so calculate the resulting
 * PC (epc + 4) into v0 and continue to MachEmulateFP().
 */
1:
	lw	a0, 0(a2)			# a0 = coproc instruction
	addu	v0, a2, 4			# v0 = next pc
2:
	sw	v0, UADDR+U_PCB_REGS+(PC * 4)	# save new pc
/*
 * Check to see if the instruction to be emulated is a floating-point
 * instruction.
 */
	srl	a3, a0, MACH_OPCODE_SHIFT
	beq	a3, MACH_OPCODE_C1, 4f		# this should never fail
/*
 * Send a floating point exception signal to the current process.
 */
3:
	lw	a0, curproc			# get current process
	cfc1	a2, MACH_FPC_CSR		# code = FP execptions
	ctc1	zero, MACH_FPC_CSR		# Clear exceptions
	jal	trapsignal
	li	a1, SIGFPE
	b	FPReturn
	nop

/*
 * Finally, we can call MachEmulateFP() where a0 is the instruction to emulate.
 */
4:
	jal	MachEmulateFP
	nop

/*
 * Turn off the floating point coprocessor and return.
 */
FPReturn:
	mfc0	t0, MACH_COP_0_STATUS_REG
	lw	ra, STAND_RA_OFFSET(sp)
	and	t0, t0, ~MACH_SR_COP_1_BIT
	mtc0	t0, MACH_COP_0_STATUS_REG
	j	ra
	addu	sp, sp, STAND_FRAME_SIZE
END(MachFPInterrupt)

/*----------------------------------------------------------------------------
 *
 * MachConfigCache --
 *
 *	Size the caches.
 *	NOTE: should only be called from mach_init().
 *
 * Results:
 *     	None.
 *
 * Side effects:
 *	The size of the data cache is stored into machDataCacheSize and the
 *	size of instruction cache is stored into machInstCacheSize.
 *
 *----------------------------------------------------------------------------
 */
NON_LEAF(MachConfigCache, STAND_FRAME_SIZE, ra)
	subu	sp, sp, STAND_FRAME_SIZE
	sw	ra, STAND_RA_OFFSET(sp)		# Save return address.
	.mask	0x80000000, (STAND_RA_OFFSET - STAND_FRAME_SIZE)
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts.
	la	v0, 1f
	or	v0, MACH_UNCACHED_MEMORY_ADDR	# Run uncached.
	j	v0
	nop
1:
/*
 * This works because jal doesn't change pc[31..28] and the
 * linker still thinks SizeCache is in the cached region so it computes
 * the correct address without complaining.
 */
	jal	SizeCache			# Get the size of the d-cache.
	nop
	sw	v0, machDataCacheSize
	nop					# Make sure sw out of pipe
	nop
	nop
	nop
	li	v0, MACH_SR_SWAP_CACHES		# Swap caches
	mtc0	v0, MACH_COP_0_STATUS_REG
	nop					# Insure caches stable
	nop
	nop
	nop
	jal	SizeCache			# Get the size of the i-cache.
	nop
	mtc0	zero, MACH_COP_0_STATUS_REG	# Swap back caches and enable.
	nop
	nop
	nop
	nop
	sw	v0, machInstCacheSize
	la	t0, 1f
	j	t0				# Back to cached mode
	nop
1:
	lw	ra, STAND_RA_OFFSET(sp)		# Restore return addr
	addu	sp, sp, STAND_FRAME_SIZE	# Restore sp.
	j	ra
	nop
END(MachConfigCache)

/*----------------------------------------------------------------------------
 *
 * SizeCache --
 *
 *	Get the size of the cache.
 *
 * Results:
 *	The size of the cache.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------------
 */
LEAF(SizeCache)
	mfc0	t0, MACH_COP_0_STATUS_REG	# Save the current status reg.
	nop
	or	v0, t0, MACH_SR_ISOL_CACHES	# Isolate the caches.
	nop					# Make sure no stores in pipe
	mtc0	v0, MACH_COP_0_STATUS_REG
	nop					# Make sure isolated
	nop
	nop
/*
 * Clear cache size boundaries.
 */
	li	v0, MACH_MIN_CACHE_SIZE
	li	v1, MACH_CACHED_MEMORY_ADDR
	li	t2, MACH_MAX_CACHE_SIZE
1:
	addu	t1, v0, v1			# Compute address to clear
	sw	zero, 0(t1)			# Clear cache memory
	bne	v0, t2, 1b
	sll	v0, v0, 1

	li	v0, -1
	sw	v0, 0(v1)			# Store marker in cache
	li	v0, MACH_MIN_CACHE_SIZE
2:
	addu	t1, v0, v1			# Compute address
	lw	t3, 0(t1)			# Look for marker
	nop
	bne	t3, zero, 3f			# Found marker.
	nop
	bne	v0, t2, 2b			# keep looking
	sll	v0, v0, 1			# cache size * 2

	move	v0, zero			# must be no cache
3:
	mtc0	t0, MACH_COP_0_STATUS_REG
	nop					# Make sure unisolated
	nop
	nop
	nop
	j	ra
	nop
END(SizeCache)

/*----------------------------------------------------------------------------
 *
 * MachFlushCache --
 *
 *	Flush the caches.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The contents of the caches is flushed.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachFlushCache)
	lw	t1, machInstCacheSize		# Must load before isolating
	lw	t2, machDataCacheSize		# Must load before isolating
	mfc0	t3, MACH_COP_0_STATUS_REG 	# Save the status register.
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts.
	la	v0, 1f
	or	v0, MACH_UNCACHED_MEMORY_ADDR	# Run uncached.
	j	v0
	nop
/*
 * Flush the instruction cache.
 */
1:
	li	v0, MACH_SR_ISOL_CACHES | MACH_SR_SWAP_CACHES
	mtc0	v0, MACH_COP_0_STATUS_REG	# Isolate and swap caches.
	li	t0, MACH_UNCACHED_MEMORY_ADDR
	subu	t0, t0, t1
	li	t1, MACH_UNCACHED_MEMORY_ADDR
	la	v0, 1f				# Run cached
	j	v0
	nop
1:
	addu	t0, t0, 4
	bne	t0, t1, 1b
	sb	zero, -4(t0)

	la	v0, 1f
	or	v0, MACH_UNCACHED_MEMORY_ADDR
	j	v0				# Run uncached
	nop
/*
 * Flush the data cache.
 */
1:
	li	v0, MACH_SR_ISOL_CACHES
	mtc0	v0, MACH_COP_0_STATUS_REG	# Isolate and swap back caches
	li	t0, MACH_UNCACHED_MEMORY_ADDR
	subu	t0, t0, t2
	la	v0, 1f
	j	v0				# Back to cached mode
	nop
1:
	addu	t0, t0, 4
	bne	t0, t1, 1b
	sb	zero, -4(t0)

	nop					# Insure isolated stores
	nop					#   out of pipe.
	nop
	nop
	mtc0	t3, MACH_COP_0_STATUS_REG	# Restore status reg.
	nop					# Insure cache unisolated.
	nop
	nop
	nop
	j	ra
	nop
END(MachFlushCache)

/*----------------------------------------------------------------------------
 *
 * MachFlushICache --
 *
 *	void MachFlushICache(addr, len)
 *		vm_offset_t addr, len;
 *
 *	Flush instruction cache for range of addr to addr + len - 1.
 *	The address can be any valid address so long as no TLB misses occur.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The contents of the cache is flushed.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachFlushICache)
	mfc0	t0, MACH_COP_0_STATUS_REG	# Save SR
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts.

	la	v1, 1f
	or	v1, MACH_UNCACHED_MEMORY_ADDR	# Run uncached.
	j	v1
	nop
1:
	bc0f	1b				# make sure stores are complete
	li	v1, MACH_SR_ISOL_CACHES | MACH_SR_SWAP_CACHES
	mtc0	v1, MACH_COP_0_STATUS_REG
	nop
	addu	a1, a1, a0			# compute ending address
1:
	addu	a0, a0, 4
	bne	a0, a1, 1b
	sb	zero, -4(a0)

	mtc0	t0, MACH_COP_0_STATUS_REG	# enable interrupts
	j	ra				# return and run cached
	nop
END(MachFlushICache)

/*----------------------------------------------------------------------------
 *
 * MachFlushDCache --
 *
 *	void MachFlushDCache(addr, len)
 *		vm_offset_t addr, len;
 *
 *	Flush data cache for range of addr to addr + len - 1.
 *	The address can be any valid address so long as no TLB misses occur.
 *	(Be sure to use cached K0SEG kernel addresses)
 * Results:
 *	None.
 *
 * Side effects:
 *	The contents of the cache is flushed.
 *
 *----------------------------------------------------------------------------
 */
LEAF(MachFlushDCache)
	mfc0	t0, MACH_COP_0_STATUS_REG	# Save SR
	mtc0	zero, MACH_COP_0_STATUS_REG	# Disable interrupts.

	la	v1, 1f
	or	v1, MACH_UNCACHED_MEMORY_ADDR	# Run uncached.
	j	v1
	nop
1:
	bc0f	1b				# make sure stores are complete
	li	v1, MACH_SR_ISOL_CACHES
	mtc0	v1, MACH_COP_0_STATUS_REG
	nop
	addu	a1, a1, a0			# compute ending address
1:
	addu	a0, a0, 4
	bne	a0, a1, 1b
	sb	zero, -4(a0)

	mtc0	t0, MACH_COP_0_STATUS_REG	# enable interrupts
	j	ra				# return and run cached
	nop
END(MachFlushDCache)

#ifdef KADB
/*
 * Read a long and return it.
 * Note: addresses can be unaligned!
 *
 * long
L* kdbpeek(addr)
L*	caddt_t addr;
L* {
L*	return (*(long *)addr);
L* }
 */
LEAF(kdbpeek)
	li	v0, KADBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	and	v0, a0, 3		# unaligned address?
	bne	v0, zero, 1f
	nop
	b	2f
	lw	v0, (a0)		# aligned access
1:
	lwr	v0, 0(a0)		# get next 4 bytes (unaligned)
	lwl	v0, 3(a0)
2:
	j	ra			# made it w/o errors
	sw	zero, UADDR+U_PCB_ONFAULT
kadberr:
	li	v0, 1			# trap sends us here
	sw	v0, kdbmkfault
	j	ra
	nop
END(kdbpeek)

/*
 * Write a long to 'addr'.
 * Note: addresses can be unaligned!
 *
L* void
L* kdbpoke(addr, value)
L*	caddt_t addr;
L*	long value;
L* {
L*	*(long *)addr = value;
L* }
 */
LEAF(kdbpoke)
	li	v0, KADBERR
	sw	v0, UADDR+U_PCB_ONFAULT
	and	v0, a0, 3		# unaligned address?
	bne	v0, zero, 1f
	nop
	b	2f
	sw	a1, (a0)		# aligned access
1:
	swr	a1, 0(a0)		# store next 4 bytes (unaligned)
	swl	a1, 3(a0)
	and	a0, a0, ~3		# align address for cache flush
2:
	sw	zero, UADDR+U_PCB_ONFAULT
	b	MachFlushICache		# flush instruction cache
	li	a1, 8
END(kdbpoke)

/*
 * Save registers and state so we can do a 'kdbreset' (like longjmp) later.
 * Always returns zero.
 *
L* int kdb_savearea[11];
L*
L* int
L* kdbsetexit()
L* {
L*	kdb_savearea[0] = 0;
L*	return (0);
L* }
 */
	.comm	kdb_savearea, (11 * 4)

LEAF(kdbsetexit)
	la	a0, kdb_savearea
	sw	s0, 0(a0)
	sw	s1, 4(a0)
	sw	s2, 8(a0)
	sw	s3, 12(a0)
	sw	s4, 16(a0)
	sw	s5, 20(a0)
	sw	s6, 24(a0)
	sw	s7, 28(a0)
	sw	sp, 32(a0)
	sw	s8, 36(a0)
	sw	ra, 40(a0)
	j	ra
	move	v0, zero
END(kdbsetexit)

/*
 * Restore registers and state (like longjmp) and return x.
 *
L* int
L* kdbreset(x)
L* {
L*	return (x);
L* }
 */
LEAF(kdbreset)
	la	v0, kdb_savearea
	lw	ra, 40(v0)
	lw	s0, 0(v0)
	lw	s1, 4(v0)
	lw	s2, 8(v0)
	lw	s3, 12(v0)
	lw	s4, 16(v0)
	lw	s5, 20(v0)
	lw	s6, 24(v0)
	lw	s7, 28(v0)
	lw	sp, 32(v0)
	lw	s8, 36(v0)
	j	ra
	move	v0, a0
END(kdbreset)

/*
 * Trap into the debugger.
 *
L* void
L* kdbpanic()
L* {
L* }
 */
LEAF(kdbpanic)
	break	MACH_BREAK_KDB_VAL
	j	ra
	nop
END(kdbpanic)
#endif /* KADB */

#ifdef DEBUG
LEAF(cpu_getregs)
	sw	sp, 0(a0)
	sw	ra, 4(a0)
	j	ra
	sw	s8, 8(a0)
END(cpu_getregs)
#endif /* DEBUG */

/*
 * Interrupt counters for vmstat.
 * XXX These aren't used yet.
 */
	.data
	.globl	intrcnt, eintrcnt, intrnames, eintrnames
intrnames:
	.asciiz	"spur"
	.asciiz	"hil"
	.asciiz	"lev2"
	.asciiz	"lev3"
	.asciiz	"lev4"
	.asciiz	"lev5"
	.asciiz	"dma"
	.asciiz	"clock"
	.asciiz	"statclock"
	.asciiz	"nmi"
eintrnames:
	.align	2
intrcnt:
	.word	0,0,0,0,0,0,0,0,0,0
eintrcnt:
