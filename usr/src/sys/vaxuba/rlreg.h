/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rlreg.h	7.1 (Berkeley) 6/5/86
 */

struct rldevice {
	short	rlcs;		/* control status */
	u_short	rlba;		/* bus address */
	union {			/* disk address */
		u_short	seek;		/* disk seek address */
		u_short	rw;		/* disk read/write address */
		u_short	getstat;	/* get disk status command */
	} rlda;
	union {			/* multi-purpose register */
		u_short	getstat;	/* get status */
		u_short readhdr;	/* read header */
		u_short	rw;		/* read/write word count */
	} rlmp;
};

#define	NRLCYLN		512	/* number of cylinders per disk */
#define NRLTRKS		2	/* number of tracks per cylinder */
#define NRLSECT		40	/* number of sectors per track */
#define NRLBPSC		256	/* bytes per sector */

/* rlcs */
/* commands */
#define RL_NOOP		0000000		/* no-operation */
#define RL_WCHECK	0000002		/* write check */
#define RL_GETSTAT	0000004		/* get status */
#define	RL_SEEK		0000006		/* seek */
#define	RL_RHDR		0000010		/* read header */
#define	RL_WRITE	0000012		/* write data */
#define	RL_READ		0000014		/* read data */
#define	RL_RDNCK	0000016		/* read data without hdr check */

#define RL_DRDY		0000001		/* When set indicates drive ready */
#define RL_BAE		0000060		/* UNIBUS address bits 16 & 17 */
#define	RL_IE		0000100		/* interrupt enable */
#define	RL_CRDY		0000200		/* controller ready */
#define RL_DS0		0000400		/* drive select 0 */
#define RL_DS1		0001000		/* drive select 1 */
#define	RL_OPI		0002000		/* operation incomplete */
#define	RL_DCRC		0004000		/* CRC error occurred */
#define	RL_DLT		0010000		/* data late or header not found */
#define	RL_NXM		0020000		/* non-existant memory */
#define	RL_DE		0040000		/* selected drive flagged an error */
#define	RL_ERR		0100000		/* composite error */

#define	RL_DCRDY	(RL_DRDY | RL_CRDY)

#define	RLCS_BITS \
"\10\20ERR\17DE\16NXM\15DLT\14DCRC\13OPI\1DRDY"

/* da_seek */
#define	RLDA_LOW	0000001		/* lower cylinder seek */
#define	RLDA_HGH	0000005		/* higher cylinder seek */
#define	RLDA_HSU	0000000		/* upper head select */
#define	RLDA_HSL	0000020		/* lower head select */
#define	RLDA_CA		0177600		/* cylinder address */

/* da_rw */
#define	RLDA_SA		0000077		/* sector address */
#define RLDA_HST	0000000		/* upper head select */
#define	RLDA_HSB	0000100		/* lower head select */

/* da_getstat */

#define	RL_GSTAT	0000003		/* Get status */
#define	RL_RESET	0000013		/* get status with reset */

/* mp_getstat */
#define	RLMP_STA	0000001		/* drive state: load cartridge */
#define	RLMP_STB	0000002		/* drive state: brush cycle */
#define	RLMP_STC	0000004		/* drive state: seek */
#define	RLMP_BH		0000010		/* set when brushes are home */
#define	RLMP_HO		0000020		/* set when brushes over the disk */
#define	RLMP_CO		0000040		/* set when cover open */
#define	RLMP_HS		0000100		/* indicates selected head:
						0 upper head
						1 lower head */
#define	RLMP_DT		0000200		/* indicates drive type:
						0 RL01
						1 RL02 */
#define	RLMP_DSE	0000400		/* set on multiple drive selection */
#define	RLMP_VC		0001000		/* set on pack mounted and spining */
#define	RLMP_WGE	0002000		/* write gate error */
#define	RLMP_SPE	0004000		/* spin speed error */
#define	RLMP_SKTO	0010000		/*\* seek time out error */
#define RLMP_WL		0020000		/* set on protected drive */
#define RLMP_CHE	0040000		/* current head error */
#define RLMP_WDE	0100000		/* write data error */

/* mp_rhc */
#define	RLMP_SA		0000077		/* sector address */
#define	RLMP_CA		0177600		/* cylinder address */

/* check these bits after a get status and reset */
#define RLMP_STATUS (RLMP_WDE|RLMP_CHE|RLMP_SKTO|RLMP_SPE|RLMP_WGE \
	|RLMP_VC|RLMP_DSE|RLMP_CO|RLMP_HO|RLMP_BH|RLMP_STC|RLMP_STB|RLMP_STA)

/* these are the bits that should be on in the above check */
#define RLMP_STATOK (RLMP_HO|RLMP_BH|RLMP_STC|RLMP_STA)

/* mp_rw */
#define	RLMP_WC		0017777		/* word count 2's complement */

#define	RLER_BITS \
"\10\20WDE\17CHE\16WL\15SKTO\14SPE\13WGE\12VC\11DSE\
\10DT\7HS\6CO\5HO\4BH\3STC\2STB\1STA"
