/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kdbreg.h	7.2 (Berkeley) 7/9/88
 */

/*
 * The KDB50 registers are embedded inside the bi interface
 * general-purpose registers.
 */
struct	kdb_regs {
	struct	biiregs kdb_bi;
	short	kdb_xxx;	/* first half of GPR 0 unused */
	short	kdb_ip;		/* initialisation and polling */
	short	kdb_sa;		/* status & address (r/o half) */
	short	kdb_sw;		/* status & address (w/o half) */
};

/*
 * Bits in KDB status register during initialisation
 */
#define	KDB_ERR		0x8000	/* error */
#define	KDB_STEP4	0x4000	/* step 4 has started */
#define	KDB_STEP3	0x2000	/* step 3 has started */
#define	KDB_STEP2	0x1000	/* step 2 has started */
#define	KDB_STEP1	0x0800	/* step 1 has started */
#define	KDB_DI		0x0100	/* controller implements diagnostics */
#define	KDB_IE		0x0080	/* interrupt enable */
#define	KDB_NCNRMASK	0x003f	/* in STEP1, bits 0-2=NCMDL2, 3-5=NRSPL2 */
#define	KDB_IVECMASK	0x007f	/* in STEP2, bits 0-6 are interruptvec / 4 */

/* after initialisation: */
#define	KDB_GO		0x0001	/* run */

#define KDBSR_BITS \
"\20\20ERR\17STEP4\16STEP3\15STEP2\14STEP1\13oldNV\12oldQB\11DI\10IE\1GO"

/*
 * KDB Communications Area.  Note that this structure definition
 * requires NRSP and NCMD to be defined already.
 */
struct kdbca {
	short	ca_xxx1;	/* unused */
	char	ca_xxx2;	/* unused */
	char	ca_bdp;		/* BDP to purge  XXX */
	short	ca_cmdint;	/* command ring transition flag */
	short	ca_rspint;	/* response ring transition flag */
	long	ca_rspdsc[NRSP];/* response descriptors */
	long	ca_cmddsc[NCMD];/* command descriptors */
};

/*
 * Simplified routines (crash dump) use one command and one response.
 */
struct kdb1ca {
	long	ca_xxx;
	short	ca_cmdint;
	short	ca_rspint;
	long	ca_rspdsc;
	long	ca_cmddsc;
};

/*
 * Asserting KDB_MAP in values placed in mscp_seq.seq_buffer tells
 * the KDB to use mscp_seq.seq_mapbase as a set of PTEs and seq_buffer
 * as an offset value.  Hence we need no mappings; the KDB50 reads
 * the hardware page tables directly.  (Without KDB_MAP, seq_bufer
 * represents the physical memory address instead, and seq_mapbase is
 * unused.)
 */
#define	KDB_MAP		0x80000000
#define	KDB_PHYS	0		/* pseudo flag */

/*
 * KDB statistics.
 */
#define	KS_MAXC	32

struct kdbstats {
	int	ks_sys;		/* transfers done from Sysmap */
	int	ks_paget;	/* transfers done from Usrptmap */
	int	ks_contig;	/* transfers done from contiguous user map */
	int	ks_copies;	/* transfers done from pte copies */
	int	ks_mapwait;	/* number of out-of-map waits */
	int	ks_cmd[KS_MAXC];/* commands started at once */
	int	ks_inval;	/* copies due to !PG_V */
};
