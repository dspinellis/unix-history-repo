/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
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
 * from: Utah $Hdr: pte.h 1.11 89/09/03$
 *
 *	@(#)pte.h	8.1 (Berkeley) 6/10/93
 */

/*
 * R2000 hardware page table entry
 */

#ifndef LOCORE
struct pte {
#if BYTE_ORDER == BIG_ENDIAN
unsigned int	pg_pfnum:20,		/* HW: core page frame number or 0 */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_v:1,			/* HW: valid bit */
		pg_g:1,			/* HW: ignore pid bit */
		:4,
		pg_swapm:1,		/* SW: page must be forced to swap */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_prot:2;		/* SW: access control */
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
unsigned int	pg_prot:2,		/* SW: access control */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_swapm:1,		/* SW: page must be forced to swap */
		:4,
		pg_g:1,			/* HW: ignore pid bit */
		pg_v:1,			/* HW: valid bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_pfnum:20;		/* HW: core page frame number or 0 */
#endif
};

typedef union pt_entry {
	unsigned int	pt_entry;	/* for copying, etc. */
	struct pte	pt_pte;		/* for getting to bits by name */
} pt_entry_t;	/* Mach page table entry */
#endif /* LOCORE */

#define	PT_ENTRY_NULL	((pt_entry_t *) 0)

#define	PG_PROT		0x00000003
#define PG_RW		0x00000000
#define PG_RO		0x00000001
#define PG_WIRED	0x00000002
#define	PG_G		0x00000100
#define	PG_V		0x00000200
#define	PG_NV		0x00000000
#define	PG_M		0x00000400
#define	PG_N		0x00000800
#define	PG_FRAME	0xfffff000
#define PG_SHIFT	12
#define	PG_PFNUM(x)	(((x) & PG_FRAME) >> PG_SHIFT)

#if defined(KERNEL) && !defined(LOCORE)
/*
 * Kernel virtual address to page table entry and visa versa.
 */
#define	kvtopte(va) \
	(Sysmap + (((vm_offset_t)(va) - VM_MIN_KERNEL_ADDRESS) >> PGSHIFT))
#define	ptetokv(pte) \
	((((pt_entry_t *)(pte) - Sysmap) << PGSHIFT) + VM_MIN_KERNEL_ADDRESS)

extern	pt_entry_t *Sysmap;		/* kernel pte table */
extern	u_int Sysmapsize;		/* number of pte's in Sysmap */
#endif
