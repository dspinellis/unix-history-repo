/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tsreg.h	7.3 (Berkeley) %G%
 */

/*
 * TS11 controller registers
 */
struct	tsdevice {
	u_short	tsdb;		/* data buffer */
	u_short	tssr;		/* status register */
};

/* Bits in (unibus) status register */
#define	TS_SC	0100000		/* special condition (error) */
#define	TS_UPE	0040000		/* Unibus parity error */
#define	TS_SPE	0020000		/* serial bus parity error */
#define	TS_RMR	0010000		/* register modification refused */
#define	TS_NXM	0004000		/* non-existent memory */
#define	TS_NBA	0002000		/* need buffer address */
#define	TS_XMEM	0001400		/* Unibus xmem bits */
#define	TS_SSR	0000200		/* subsytem ready */
#define	TS_OFL	0000100		/* off-line */
#define	TS_FTC	0000060		/* fatal termination class */
#define	TS_TC	0000016		/* termination class */

#define	TS_SUCC	000		/* successful termination */
#define	TS_ATTN	002		/* attention */
#define	TS_ALERT 004		/* tape status alert */
#define	TS_REJECT 06		/* function reject */
#define	TS_RECOV 010		/* recoverable error */
#define	TS_RECNM 012		/* recoverable error, no tape motion */
#define	TS_UNREC 014		/* unrecoverable error */
#define	TS_FATAL 016		/* fatal error */

#define	TSSR_BITS	\
"\10\20SC\17UPE\16SPE\15RMR\14NXM\13NBA\12A17\11A16\10SSR\
\7OFL\6FC1\5FC0\4TC2\3TC1\2TC0\1-"

#define	b_repcnt	b_bcount
#define	b_command	b_resid

/* status message */
struct	ts_sts {
	u_short	s_sts;		/* packet header */
	u_short	s_len;		/* packet length */
	u_short s_rbpcr;	/* residual frame count */
	u_short	s_xs0;		/* extended status 0 - 3 */
	u_short	s_xs1;
	u_short	s_xs2;
	u_short	s_xs3;
};

/* Error codes in xstat 0 */
#define	TS_TMK	0100000		/* tape mark detected */
#define	TS_RLS	0040000		/* record length short */
#define	TS_LET	0020000		/* logical end of tape */
#define	TS_RLL	0010000		/* record length long */
#define	TS_WLE	0004000		/* write lock error */
#define	TS_NEF	0002000		/* non-executable function */
#define	TS_ILC	0001000		/* illegal command */
#define	TS_ILA	0000400		/* illegal address */
#define	TS_MOT	0000200		/* capstan is moving */
#define	TS_ONL	0000100		/* on-line */
#define	TS_IES	0000040		/* interrupt enable status */
#define	TS_VCK	0000020		/* volume check */
#define	TS_PED	0000010		/* phase-encoded drive */
#define	TS_WLK	0000004		/* write locked */
#define	TS_BOT	0000002		/* beginning of tape */
#define	TS_EOT	0000001		/* end of tape */

#define	TSXS0_BITS	\
"\10\20TMK\17RLS\16LET\15RLL\14WLE\13NEF\12ILC\11ILA\10MOT\
\7ONL\6IES\5VCK\4PED\3WLK\2BOT\1EOT"

/* Error codes in xstat 1 */
#define	TS_DLT	0100000		/* data late */
#define	TS_COR	0020000		/* correctable data */
#define	TS_CRS	0010000		/* crease detected */
#define	TS_TIG	0004000		/* trash in the gap */
#define	TS_DBF	0002000		/* deskew buffer full */
#define	TS_SCK	0001000		/* speed check */
#define	TS_IPR	0000200		/* invalid preamble */
#define	TS_SYN	0000100		/* synchronization failure */
#define	TS_IPO	0000040		/* invalid postamble */
#define	TS_IED	0000020		/* invalid end of data */
#define	TS_POS	0000010		/* postamble short */
#define	TS_POL	0000004		/* postamble long */
#define	TS_UNC	0000002		/* uncorrectable data */
#define	TS_MTE	0000001		/* multitrack error */

#define	TSXS1_BITS	\
"\10\20DLT\17-\16COR\15CRS\14TIG\13DBF\12SCK\11-\10IPR\
\7SYN\6IPO\5IED\4POS\3POL\2UNC\1MTE"

/* Error codes in xstat 2 */
#define	TS_OPM	0100000		/* operation in progress */
#define	TS_SIP	0040000		/* silo parity error */
#define	TS_BPE	0020000		/* serial bus parity error */
#define	TS_CAF	0010000		/* capstan acceleration failure */
#define	TS_WCF	0002000		/* write card fail */
#define	TS_DTP	0000400		/* dead track parity */
#define	TS_DT	0000377		/* dead tracks */

#define	TSXS2_BITS	\
"\10\20OPM\17SIP\16BPE\15CAF\14-\13WCF\12-\11DTP"

/* Error codes in xstat 3 */
#define	TS_MEC	0177400		/* microdiagnostic error code */
#define	TS_LMX	0000200		/* limit exceeded */
#define	TS_OPI	0000100		/* operation incomplete */
#define	TS_REV	0000040		/* reverse */
#define	TS_CRF	0000020		/* capstan response fail */
#define	TS_DCK	0000010		/* density check */
#define	TS_NOI	0000004		/* noise record */
#define	TS_LXS	0000002		/* limit exceeded statically */
#define	TS_RIB	0000001		/* reverse into BOT */

#define	TSXS3_BITS	\
"\10\10LMX\7OPI\6REV\5CRF\4DCK\3NOI\2LXS\1RIB"


/* command message */
struct ts_cmd {
	u_short	c_cmd;		/* command */
	u_short	c_loba;		/* low order buffer address */
	u_short	c_hiba;		/* high order buffer address */
#define	c_repcnt c_loba
	u_short	c_size;		/* byte count */
};

/* commands and command bits */
#define	TS_ACK		0100000		/* ack - release command packet */
#define	TS_CVC		0040000		/* clear volume check */
#define	TS_IE		0000200
#define	TS_RCOM		0000001
#define	TS_REREAD	0001001		/* read data retry */
#define	TS_SETCHR	0000004		/* set characteristics */
#define	TS_WCOM		0000005
#define	TS_REWRITE	0001005		/* write data retry */
#define	TS_RETRY	0001000		/* retry bit for read and write */
#define	TS_SFORW	0000010		/* forward space record */
#define	TS_SREV		0000410		/* reverse space record */
#define	TS_SFORWF	0001010		/* forward space file */
#define	TS_SREVF	0001410		/* reverse space file */
#define	TS_REW		0002010		/* rewind */
#define	TS_OFFL		0000412		/* unload */
#define	TS_WEOF		0000011		/* write tape mark */
#define	TS_SENSE	0000017		/* get status */

/* characteristics data */
struct ts_char {
	long	char_addr;		/* address of status packet */
	u_short	char_size;		/* its size */
	u_short	char_mode;		/* characteristics */
};


/* characteristics */
#define	TS_ESS	0200		/* enable skip tape marks stop */
#define	TS_ENB	0100		/* enable skip stop BOT */
#define	TS_EAI	0040		/* enable attention interrupts */
#define	TS_ERI	0020		/* enable message buffer release interrupts */

/* Emulex TC13 only */
#define	TS_NRZI	0001		/* select NRZI (800bpi) rather than PE */
