/*************************************************************************/
/*									 */
/*									 */
/*	 ________________________________________________________	 */
/*	/							 \	 */
/*     |	  AAA	       CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*     |	 AAAAA	      CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     |	AAAAAAA	      CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |       AAAA AAAA      CCCC		CCCC		  |	 */
/*     |      AAAA   AAAA     CCCC		CCCC		  |	 */
/*     |     AAAA     AAAA    CCCC		CCCC		  |	 */
/*     |    AAAA       AAAA   CCCC		CCCC		  |	 */
/*     |   AAAA	 AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |  AAAA	  AAAAAAAAAAA CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     | AAAA	   AAAAAAAAA   CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*	\________________________________________________________/	 */
/*									 */
/*	Copyright (c) 1986 by Advanced Computer Communications		 */
/*	720 Santa Barbara Street, Santa Barbara, California  93101	 */
/*	(805) 963-9431							 */
/*									 */
/*									 */
/*  File:		ddareg.h					 */
/*			When this file is copied to the /sys/vaxif	 */
/*			directory, it is renamed 'if_ddareg.h'.		 */
/*									 */
/*  Project:		UNIX 4.n BSD DDA-X.25 Network Interface Driver	 */
/*			for ACP 5250 and ACP 6250			 */
/*									 */
/*  Function:		This file contains definitions of the hardware	 */
/*			interface of the ACP 5250/6250.			 */
/*									 */
/*  Revision History at end of file					 */
/*									 */
/*************************************************************************/


/* ACP device Communication Register layout */


#ifdef SIMULATION
/* device registers */
struct ddaregs {
	u_short	csr;			/* control and status register */
					/* I/O request mailbox */
	u_short req_chan;		/*   FDX channel number */
	u_char	req_flags;		/*   mailbox flags */
	u_char	req_adx;		/*   address bits 17-16 */
	u_short req_addr;		/*   address bits 15-00 */
	u_short req_cnt;		/*   byte count */
	u_char	req_sbf;		/*   I/O subfunction */
	u_char	req_fcn;		/*   I/O function */
					/* I/O completion mailbox */
	u_short cmp_chan;		/*   FDX channel number */
	u_char	cmp_flags;		/*   mailbox flags */
	u_char	cmp_unused;		/*   address bits 17-16 */
	u_short cmp_cnt;		/*   byte count */
	u_char	cmp_sbst;		/*   I/O substatus */
	u_char	cmp_stat;		/*   I/O status */
					/* Transfer request mailbox */
	u_short xfr_chan;		/*   FDX channel number */
	u_char	xfr_flags;		/*   mailbox flags */
	u_char	xfr_adx;		/*   address bits 17-16 */
	u_short xfr_addr;		/*   address bits 15-00 */
	u_short xfr_cnt;		/*   byte count */
					/* System status mailbox */
	u_char	sys_id;			/*   system identification */
	u_char	sys_vers;		/*   system version number */
	u_char  sys_stat;		/*   system status */
	u_char	sys_vect;		/*   interrupt vector base */
};
#else
/* device registers */
struct ddaregs {
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
#endif

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

/* mailbox handshake flags, these flags are used with the req_flags, */
/* cmp_flags, and xfr_flags to indicate current state of events      */

#define FLAGS_RDY	0x80		/* indicates ready */
#define FLAGS_DON	0x40		/* indicates done */
#define FLAGS_DIR	0x20		/* indicates write (host to ACP) */

/* I/O request function code definitions */

#define DDARDB		0x01		/* read from ACP */
#define DDAWRT		0x02		/* write to ACP */
#define DDASTR		0x10		/* stream flag */
#define DDAEOS		(0x20|DDASTR)	/* end of stream flag */

#define DDAABT		0x04		/* abort flag */
#define DDAXFR		0x40		/* indicates transfer request   */
				/* The UNIBUS address in req_addr is    */
				/* invalid.  The ACP device must issue  */
				/* a Transfer Request in order to       */
				/* obtain the UNIBUS address            */
#define DDASWP		0x80		/* Swap host high/low bytes     */
				/* The ACP device views UNIBUS memory   */
				/* as if it were MC68000 memory:  the   */
				/* MS byte of a word is even-addressed  */
				/* byte and the LS byte is the odd-     */
				/* addressed byte.                      */

#define FCN_MASK	0x07

#define DDA_BITS \
"\10\20UER\17NXM\16PER\15ZRUN\14ZGO\10MBLK\7SRV\6MAIN\5DMA\4WRT\3IEN\2RST\1NMI"

/*  Host Request Mailbox Completion Status  */

#define DDAIOCOK	0x01	/* successful completion */
#define DDAIOCOKP 	0x02	/* successful completion, more data pending */
#define DDAIOCABT 	0xff	/* i/o aborted */
#define DDAIOCERR 	0xfe	/* program error */
#define DDAIOCOVR 	0xfd	/* overrun error */
#define DDAIOCUBE 	0xfc	/* Transfer count = 0 */
				/* Either program error (byte count on I/O   */
				/* request equals 0) or driver error (driver */
				/* granted byte count = 0 in response to     */
				/* Transfer Request)                         */
#define DDAIODMAE 	0xfb	/* DMA completion error:  Completion sub-    */
				/* status equals error bits from the ACP DMA */
				/* status register                           */
#define DDAIOLCOL 	0xf9	/* Listen Collision:  Both sides of a DPN in */
				/* the same direction have Listen requests   */
				/* pending.  Both requests are terminated    */
				/* with this status code.                    */
#define DDAIOFUNC 	0xf8	/* Invalid function:  The function specified */
				/* in a request is invalid.                  */
#define DDAIODPN  	0xf7	/* Invalid DPN:  The DPN specified in a      */
				/* request is out of the range handled by    */
				/* CRI (Communications Register Interface)   */

#ifdef	__GNU__
#define ddaregs ddaregs volatile
#endif	__GNU__

/*
Revision History:

26-Mar-1986: V1.0 Clair Russ
		First generated.
??-???-1988: V1.1 Steve Johnson
		Added code for simulation
19-Mar-1989: V4.3.1 Paul Traina
		Added Multinet / GCC support for structure padding
28-May-1989: V4.3.6 Paul Traina
		Cosmetic changes only
*/
