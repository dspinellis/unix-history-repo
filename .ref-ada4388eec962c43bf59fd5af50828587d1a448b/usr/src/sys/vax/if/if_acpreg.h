/*-
 *	@(#)if_acpreg.h	7.2 (Berkeley) %G%
 */

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*       ________________________________________________________        */
/*      /                                                        \       */
/*     |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*     |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |       AAAA AAAA      CCCC              CCCC              |      */
/*     |      AAAA   AAAA     CCCC              CCCC              |      */
/*     |     AAAA     AAAA    CCCC              CCCC              |      */
/*     |    AAAA       AAAA   CCCC              CCCC              |      */
/*     |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*      \________________________________________________________/       */
/*                                                                       */
/*  	Copyright (c) 1986 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		if_acpreg.h                                      */
/*                                                                       */
/*  Author:		Arthur Berggreen                                 */
/*                                                                       */
/*  Project:		ACP6100 (UPB with HDLC firmware)                 */
/*          		ACP5100 (QPB with HDLC firmware)                 */
/*                                                                       */
/*  Revision History:                                                    */
/*                                                                       */
/*    21-NOV-1985  Clare Russ:  add fileheader and comments              */
/*         Add definitions for implementatin of new Command Interface    */
/*         (CIF) and Access Path Allocation Protocol (APAP).             */
/*                                                                       */
/*************************************************************************/


/* device registers */
struct acpregs {
	u_short	csr;			/* control and status register */
					/* I/O request mailbox */
	u_short req_chan;		/*   FDX channel number */
	u_char	req_adx;		/*   address bits 17-16 */
	u_char	req_flags;		/*   mailbox flags */
	u_short req_addr;		/*   address bits 15-00 */
	u_short req_cnt;		/*   byte count */
	u_char	req_fcn;		/*   I/O function */
	u_char	req_sbf;		/*   I/O subfunction */
					/* I/O completion mailbox */
	u_short cmp_chan;		/*   FDX channel number */
	u_char	cmp_unused;		/*   address bits 17-16 */
	u_char	cmp_flags;		/*   mailbox flags */
	u_short cmp_cnt;		/*   byte count */
	u_char	cmp_stat;		/*   I/O status */
	u_char	cmp_sbst;		/*   I/O substatus */
					/* Transfer request mailbox */
	u_short xfr_chan;		/*   FDX channel number */
	u_char	xfr_adx;		/*   address bits 17-16 */
	u_char	xfr_flags;		/*   mailbox flags */
	u_short xfr_addr;		/*   address bits 15-00 */
	u_short xfr_cnt;		/*   byte count */
					/* System status mailbox */
	u_char	sys_vers;		/*   system version number */
	u_char	sys_id;			/*   system identification */
	u_char	sys_vect;		/*   interrupt vector base */
	u_char  sys_stat;		/*   system status */
};


/* defines for CSR */

#define	CSR_BIT15	0x8000
#define	CSR_BIT14	0x4000
#define	CSR_MAINT	0x2000
#define	CSR_HALT	0x1000
#define	CSR_IBPEND	0x0800
#define	CSR_IAPEND	0x0400
#define	CSR_IBREQ	0x0200
#define	CSR_IAREQ	0x0100
#define	CSR_BIT7	0x0080
#define	CSR_BIT6	0x0040
#define	CSR_INTRB	0x0020		/* ACP CPU Interrupt A Request */
#define	CSR_INTRA	0x0010		/* ACP CPU Interrupt B Request */
#define	CSR_IENB	0x0008		/* enable UNIBUS interrupt b   */
#define	CSR_IENA	0x0004		/* enable UNIBUS interrupt a   */
#define	CSR_DMAEN	0x0002
#define	CSR_RESET	0x0001

/* mailbox handshake flags */

#define FLAGS_RDY	0x80
#define FLAGS_DON	0x40
#define FLAGS_DIR	0x20

/* I/O request function code definitions */

#define ACPRED		0x01		/* read from ACP */
#define ACPWRT		0x02		/* write to ACP */
#define ACPSTR		0x10		/* stream flag */
#define ACPEOS		(0x20|ACPSTR)	/* end of stream flag */
#define ACPABT		0x04		/* abort flag */
#define ACPXFR		0x40		/* tranfer req/grant flag */

#define FCN_MASK	0x07
