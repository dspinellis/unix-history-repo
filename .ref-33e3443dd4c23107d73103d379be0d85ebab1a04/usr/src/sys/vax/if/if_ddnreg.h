/*	@(#)if_ddnreg.h	7.1 (Berkeley) %G% */


/************************************************************************\

     ________________________________________________________
    /                                                        \
   |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
   |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |       AAAA AAAA      CCCC              CCCC              |
   |      AAAA   AAAA     CCCC              CCCC              |
   |     AAAA     AAAA    CCCC              CCCC              |
   |    AAAA       AAAA   CCCC              CCCC              |
   |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
    \________________________________________________________/

	Copyright (c) 1985 by Advanced Computer Communications
	720 Santa Barbara Street, Santa Barbara, California  93101
	(805) 963-9431

	This software may be duplicated and used on systems
	which are licensed to run U.C. Berkeley versions of
	the UNIX operating system.  Any duplication of any
	part of this software must include a copy of ACC's
	copyright notice.


File:
		if_ddnreg.h

Author:
		Art Berggreen

Project:
		4.2 DDN X.25 network driver

Function:
		This file contains definitions of the hardware
		interface of the ACP625 (IF-11/X25).

Components:

Revision History:
		16-May-1985:	V1.0 - First release.
				Art Berggreen.

\************************************************************************/


/*	if_ddnvar.h	 V1.0	5/16/85	*/

/*
 * ACC IF-11/DDN-X25 interface
 */

struct ddnregs {			/* device registers */
	u_short	csr;			/* control and status register */
	u_char	iochn;			/* logical channel */
	u_char	ioadx;			/* address extension (A16,A17) */
	u_short	ioadl;			/* buffer address (A0-A15) */
	u_short	iocnt;			/* byte count */
	u_char	iofcn;			/* UMC funciton code */
	u_char	iosbf;			/* UMC subfunction code */
	u_char	ioini;			/* comm regs valid flag */
	u_char	staack;			/* interrupt acknowledge flag */
	u_char	ionmi;			/* NMI routine active flag */
	u_char	xfrgnt;			/* UMR transfer grant flag */
	u_char	stachn;			/* interrupt channel number */
	u_char	statyp;			/* interrupt type code */
	u_char	stacc;			/* completion function code */
	u_char	stacs;			/* completion subfunction code */
	u_short	stacnt;			/* completion byte count */
};

#define	iovect	iochn

/* defines for CSR */

#define DDN_UER		0100000		/* UMC error condition */
#define DDN_NXM		0040000		/* non-existent memory error */
#define DDN_PER		0020000		/* UNIBUS parity error */
#define DDN_ZRUN	0010000		/* Z80 running */
#define DDN_ZGO		0004000		/* Z80 not in wait state */
#define DDN_MBLK	0000200		/* memory swap state (0=main, 1=srv) */
#define	DDN_SRV		0000100		/* select UMC service memory */
#define DDN_MAIN	0000040		/* select UMC main memory */
#define DDN_DMA		0000020		/* DMA enable */
#define DDN_WRT		0000010		/* DMA write enable */
#define DDN_IEN		0000004		/* interrupt enable */
#define DDN_RST		0000002		/* reset */
#define	DDN_NMI		0000001		/* cause NMI */

#define DDN_BITS \
"\10\20UER\17NXM\16PER\15ZRUN\14ZGO\10MBLK\7SRV\6MAIN\5DMA\4WRT\3IEN\2RST\1NMI"

/* start i/o function code definitions */

#define DDNWRT		0	/* write to if-11 */
#define DDNRDB		1	/* read from if-11 */
#define DDNSTR		2	/* stream flag */
#define DDNEOS		(4|DDNSTR)  /* end of stream flag */
#define DDNABT		8	/* abort flag */
#define DDNUMR		16	/* UMR protocol flag */

/* interrupt type definitions */

#define DDNSACK		0	/* start i/o ack */
#define DDNDONE		1	/* i/o completion */
#define DDNXREQ		2	/* UMR protocol transfer request */

/* i/o completion codes */

#define DDNIOCOK	0001	/* successful completion */
#define DDNIOCOKP 	0002	/* successful completion, more data pending */
#define DDNIOCABT 	0361	/* i/o aborted */
#define DDNIOCERR 	0321	/* program error */
#define DDNIOCOVR 	0363	/* overrun error */
#define DDNIOCUBE 	0374	/* non-existant memory or unibus error */

/* UMR protocol transfer grant code definitions */

#define DDNXEVN		1	/* start with even address */
#define DDNXODD		2	/* start with odd address */
#define DDNNUMR		4	/* non-UMR transfer */
#define DDNXABT		8	/* abort transfer */
