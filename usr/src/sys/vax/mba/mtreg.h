/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)mtreg.h	7.3 (Berkeley) %G%
 */

/*
 * TU78 registers.
 */

struct mtdevice {
	int	mtcs;		/* control status register */
	int	mter;		/* error register */
	int	mtca;		/* command address, rec cnt, skp cnt reg */
	int	mtmr1;		/* maintenance register */
	int	mtas;		/* attention summary register */
	int	mtbc;		/* byte count register */
	int	mtdt;		/* drive type register */
	int	mtds;		/* drive status register */
	int	mtsn;		/* serial number register */
	int	mtmr2;		/* maintenance register */
	int	mtmr3;		/* maintenance register */
	int	mtner;		/* non-data transfer error register */
	int	mtncs[4];	/* non-data transfer command registers */
	int	mtia;		/* internal address */
	int	mtid;		/* internal data */
};

/* mtcs */
#define	MT_GO		000001		/* go bit */
#define	MT_NOOP		000002		/* no operation */
#define	MT_UNLOAD	000004		/* unload tape */
#define	MT_REW		000006		/* rewind */
#define	MT_SENSE	000010		/* sense */
#define	MT_DSE		000012		/* data security erase */
#define	MT_WTMPE	000014		/* write phase encoded tape mark */
#define	MT_WTM		MT_WTMPE	/* generic write tape mark */
#define	MT_WTMGCR	000016		/* write GCR tape mark */
#define	MT_SFORW	000020		/* space forward record */
#define	MT_SREV		000022		/* space reverse record */
#define	MT_SFORWF	000024		/* space forward file */
#define	MT_SREVF	000026		/* space reverse file */
#define	MT_SFORWE	000030		/* space forward either */
#define	MT_SREVE	000032		/* space reverse either */
#define	MT_ERGPE	000034		/* erase tape, set PE */
#define	MT_ERASE	MT_ERGPE	/* generic erase tape */
#define	MT_ERGGCR	000036		/* erase tape, set GCR */
#define	MT_CLSPE	000040		/* close file PE */
#define	MT_CLS		MT_CLSPE	/* generic close file */
#define	MT_CLSGCR	000042		/* close file GCR */
#define	MT_SLEOT	000044		/* space to logical EOT */
#define	MT_SFLEOT	000046		/* space forward file, stop on LEOT */
#define	MT_WCHFWD	000050		/* write check forward */
#define	MT_WCHREV	000056		/* write check reverse */
#define	MT_WRITEPE	000060		/* write phase encoded */
#define	MT_WRITE	MT_WRITEPE	/* generic write */
#define	MT_WRITEGCR	000062		/* write group coded */
#define	MT_READ		000070		/* read forward */
#define	MT_EXSNS	000072		/* read extended sense error log */
#define	MT_READREV	000076		/* read reverse */
#define	MT_GCR		000002		/* make generic ops GCR ops */

/* mtds */
#define	MTDS_RDY	0100000		/* tape ready */
#define	MTDS_PRES	0040000		/* tape unit has power */
#define	MTDS_ONL	0020000		/* online */
#define	MTDS_REW	0010000		/* tape rewinding */
#define	MTDS_PE		0004000		/* tape set for phase encoded */
#define	MTDS_BOT	0002000		/* tape at BOT */
#define	MTDS_EOT	0001000		/* tape at EOT */
#define	MTDS_FPT	0000400		/* write protected */
#define	MTDS_AVAIL	0000200		/* unit available */
#define	MTDS_SHR	0000100		/* unit is shared */
#define	MTDS_MAINT	0000040		/* maintenance mode */
#define	MTDS_DSE	0000020		/* DSE in progress */

#define	MTDS_BITS	\
"\10\20RDY\17PRES\16ONL\15REW\14PE\13BOT\12EOT\11FPT\10AVAIL\
\7SHR\6MAINT\5DSE"

/* mter */
#define	MTER_INTCODE	0377		/* mask for interrupt code */
#define	MTER_FAILCODE	0176000		/* failure code */
#define	MTER_FSHIFT	10		/* shift to make units */

/* interrupt codes */
#define	MTER_DONE	001		/* operation complete */
#define	MTER_TM		002		/* unexpected tape mark */
#define	MTER_BOT	003		/* unexpected BOT detected */
#define	MTER_EOT	004		/* tape positioned beyond EOT */
#define	MTER_LEOT	005		/* unexpected LEOT detected */
#define	MTER_NOOP	006		/* no-op completed */
#define	MTER_RWDING	007		/* rewinding */
#define	MTER_FPT	010		/* write protect error */
#define	MTER_NOTRDY	011		/* not ready */
#define	MTER_NOTAVL	012		/* not available */
#define	MTER_OFFLINE	013		/* offline */
#define	MTER_NONEX	014		/* unit does not exist */
#define	MTER_NOTCAP	015		/* not capable */
#define	MTER_ONLINE	017		/* tape came online */
#define	MTER_LONGREC	020		/* long tape record */
#define	MTER_SHRTREC	021		/* short tape record */
#define	MTER_RETRY	022		/* retry */
#define	MTER_RDOPP	023		/* read opposite */
#define	MTER_UNREAD	024		/* unreadable */
#define	MTER_ERROR	025		/* error */
#define	MTER_EOTERR	026		/* EOT error */
#define	MTER_BADTAPE	027		/* tape position lost */
#define	MTER_TMFLTA	030		/* TM fault A */
#define	MTER_TUFLTA	031		/* TU fault A */
#define	MTER_TMFLTB	032		/* TM fault B */
#define	MTER_MBFLT	034		/* Massbus fault */
#define	MTER_KEYFAIL	077		/* keypad entry error */

/* mtdt */
#define	MTDT_NSA	0100000		/* not sector addressed; always 1 */
#define	MTDT_TAP	0040000		/* tape; always 1 */
#define	MTDT_MOH	0020000		/* moving head; always 0 */
#define	MTDT_7CH	0010000		/* 7 channel; always 0 */
#define	MTDT_DRQ	0004000		/* drive request required */
#define	MTDT_SPR	0002000		/* slave present; always 1 ??? */
/* bit 9 is spare */
/* bits 8-0 are formatter/transport type */

/* mtid */
#define	MTID_RDY	0100000		/* controller ready */
#define	MTID_CLR	0040000		/* controller clear */

#define	b_repcnt  b_bcount
#define	b_command b_resid
