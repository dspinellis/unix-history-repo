/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)pcb.h	7.2 (Berkeley) 5/8/91
 */

/*
 * TAHOE process control block
 */
struct pcb {
	int	pcb_ksp; 	/* kernel stack pointer */
	int	pcb_usp; 	/* user stack pointer */
	int	pcb_r0; 
	int	pcb_r1; 
	int	pcb_r2; 
	int	pcb_r3; 
	int	pcb_r4; 
	int	pcb_r5; 
	int	pcb_r6; 
	int	pcb_r7; 
	int	pcb_r8; 
	int	pcb_r9; 
	int	pcb_r10; 
	int	pcb_r11; 
	int	pcb_r12; 
	int	pcb_r13; 
#define	pcb_fp pcb_r13
	int	pcb_pc; 	/* program counter */
	int	pcb_psl; 	/* program status longword */
	struct  pte *pcb_p0br; 	/* seg 0 base register */
	int	pcb_p0lr; 	/* seg 0 length register and astlevel */
	struct  pte *pcb_p1br; 	/* seg 1 base register */
	int	pcb_p1lr; 	/* seg 1 length register and pme */
	struct  pte *pcb_p2br; 	/* seg 2 base register */
	int	pcb_p2lr; 	/* seg 2 length register and pme */
	int	pcb_ach;	/* accumulator - high order longword */
	int	pcb_acl;	/* accumulator - low order longword */
#define ACH	pcb_ach
#define ACL	pcb_acl
	int	pcb_hfs;	/* fp status register */
/*
 * Software pcb (extension)
 */
	union {
		float 	*faddr;	/* address of single precision accumulator */
		double	*daddr; /* address of double precision accumulator */
	} pcb_savacc;
#define FSAVACC	pcb_savacc.faddr
#define DSAVACC pcb_savacc.daddr
	int	pcb_szpt; 	/* number of pages of user page table */
	int	pcb_cmap2;
	int	*pcb_sswap;
	long	pcb_sigc[5];	/* sigcode actually 19 bytes */
};

extern long	*user_psl;

#define	aston() { \
	u.u_pcb.pcb_psl |= PSL_SFE; \
	if ((int)user_psl != 0) \
		*user_psl |= PSL_SFE; \
}

#define	astoff() { \
	u.u_pcb.pcb_psl &= ~ PSL_SFE; \
	if ((int)user_psl != 0) \
		*user_psl &= ~PSL_SFE; \
}
