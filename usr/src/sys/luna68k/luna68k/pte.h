/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1982, 1986, 1990, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: pte.h 1.13 92/01/20$
 * from: hp300/hp300/pte.h	7.4 (Berkeley) 6/5/92
 *
 *	@(#)pte.h	8.1 (Berkeley) 6/10/93
 */

/*
 * LUNA68K hardware segment/page table entries
 */

struct ste {
	unsigned int	sg_pfnum:20;	/* page table frame number */
	unsigned int	:8;		/* reserved at 0 */
	unsigned int	:1;		/* reserved at 1 */
	unsigned int	sg_prot:1;	/* write protect bit */
	unsigned int	sg_v:2;		/* valid bits */
};

struct ste40 {
	unsigned int	sg_ptaddr:24;	/* page table page addr */
	unsigned int	:4;		/* reserved at 0 */
	unsigned int	sg_u;		/* hardware modified (dirty) bit */
	unsigned int	sg_prot:1;	/* write protect bit */
	unsigned int	sg_v:2;		/* valid bits */
};

struct pte {
	unsigned int	pg_pfnum:20;	/* page frame number or 0 */
	unsigned int	:3;
	unsigned int	pg_w:1;		/* is wired */
	unsigned int	:1;		/* reserved at zero */
	unsigned int	pg_ci:1;	/* cache inhibit bit */
	unsigned int	:1;		/* reserved at zero */
	unsigned int	pg_m:1;		/* hardware modified (dirty) bit */
	unsigned int	pg_u:1;		/* hardware used (reference) bit */
	unsigned int	pg_prot:1;	/* write protect bit */
	unsigned int	pg_v:2;		/* valid bit */
};

typedef struct ste	st_entry_t;	/* segment table entry */
typedef struct pte	pt_entry_t;	/* Mach page table entry */

#define	PT_ENTRY_NULL	((pt_entry_t *) 0)
#define	ST_ENTRY_NULL	((st_entry_t *) 0)

#define	SG_V		0x00000002	/* segment is valid */
#define	SG_NV		0x00000000
#define	SG_PROT		0x00000004	/* access protection mask */
#define	SG_RO		0x00000004
#define	SG_RW		0x00000000
#define	SG_U		0x00000008	/* modified bit (68040) */
#define	SG_FRAME	0xfffff000
#define	SG_IMASK	0xffc00000
#define	SG_ISHIFT	22
#define	SG_PMASK	0x003ff000
#define	SG_PSHIFT	12

/* 68040 additions */
#define	SG4_MASK1	0xfe000000
#define	SG4_SHIFT1	25
#define	SG4_MASK2	0x01fc0000
#define	SG4_SHIFT2	18
#define	SG4_MASK3	0x0003f000
#define	SG4_SHIFT3	12
#define	SG4_ADDR1	0xfffffe00
#define	SG4_ADDR2	0xffffff00
#define	SG4_LEV1SIZE	128
#define	SG4_LEV2SIZE	128
#define	SG4_LEV3SIZE	64

#define	PG_V		0x00000001
#define	PG_NV		0x00000000
#define	PG_PROT		0x00000004
#define	PG_U		0x00000008
#define	PG_M		0x00000010
#define	PG_W		0x00000100
#define	PG_RO		0x00000004
#define	PG_RW		0x00000000
#define	PG_FRAME	0xfffff000
#define	PG_CI		0x00000040
#define PG_SHIFT	12
#define	PG_PFNUM(x)	(((x) & PG_FRAME) >> PG_SHIFT)

/* 68040 additions */
#define	PG_CMASK	0x00000060	/* cache mode mask */
#define	PG_CWT		0x00000000	/* writethrough caching */
#define	PG_CCB		0x00000020	/* copyback caching */
#define	PG_CIS		0x00000040	/* cache inhibited serialized */
#define	PG_CIN		0x00000060	/* cache inhibited nonserialized */
#define	PG_SO		0x00000080	/* supervisor only */

#define LUNA_STSIZE	(MAXUL2SIZE*SG4_LEV2SIZE*sizeof(st_entry_t))
                                                /* user process segment table size */
#define LUNA_MAX_PTSIZE	0x400000		/* max size of UPT */
#define LUNA_MAX_KPTSIZE	0x100000	/* max memory to allocate to KPT */
#define LUNA_PTBASE		0x10000000	/* UPT map base address */
#define LUNA_PTMAXSIZE		0x20000000	/* UPT map maximum size */

/*
 * Kernel virtual address to page table entry and to physical address.
 */
#define	kvtopte(va) \
	(&Sysmap[((unsigned)(va) - VM_MIN_KERNEL_ADDRESS) >> PGSHIFT])
#define	ptetokv(pt) \
	((((pt_entry_t *)(pt) - Sysmap) << PGSHIFT) + VM_MIN_KERNEL_ADDRESS)
#define	kvtophys(va) \
	((kvtopte(va)->pg_pfnum << PGSHIFT) | ((int)(va) & PGOFSET))

