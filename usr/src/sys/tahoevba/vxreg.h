/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)vxreg.h	7.2 (Berkeley) 6/29/88
 */

/*
 * Vioc hardware interface structure.
 */
struct	vxdevice {
	char	v_vioc;		/*  0 type of interrupt + vioc busy flag */
	char	v_hdwre;	/*  1 trap, reset, or hard interrupt */
	char	v_vcbsy;	/*  2 command busy (set by host) */
	char	v_vcid;		/*  3 command identifier */
	short	v_vcp[2];	/*  4 command pointer (sent by host) */
	short	v_unused;	/*  8 unused */
	short	v_rspoff;	/*  a offset into vblock for response buf */
	char	v_ustat;	/*  c status */
	char	v_uqual;	/*  d interrupt qualifier */
	char	v_usdata[0x3E];	/*  e unsolicited interrupt data */
	short	v_maxxmt;	/* 4C max xmit block size */
	short	v_maxsilo;	/* 4E max silo size */
	char	v_ident;	/* 50 identifies type of vioc */
	char	v_fault;	/* 51 denotes fault or ready after reset */
	short	v_dcd;		/* 52 bit mask for carr detect by port */
	short	v_cts;		/* 54 bit mask for cts by port */
	short	v_dsr;		/* 56 bit mask for dsr by port */
	short	v_secrec;	/* 58 bit mask for secondary receive */
	short	v_badport;	/* 5a bit mask of failed ports */
	char	v_portyp[16];	/* 5c type of each port */
};

/* control bits for v_vioc and v_hdwre */
#define	V_RESET 0x1		/* cause a vioc reset */
#define	V_TRP	0x2		/* cause a vioc trap */
#define	V_INTR	0x4		/* cause a vioc interrupt */
#define	V_ERR	0x80		/* generic error flag */
#define	V_BSY	0x80		/* generic busy flag */
#define	V_UNBSY	0x80		/* not busy for unsolicited interrupt */

/* command identifier codes */
#define	VXC_LIDENT	0x0000		/* load ident, set int vectors */
#define	VXC_XMITDTA	0x0200		/* transmit */
#define	VXC_XMITIMM	0x0400		/* xmit immediate */
#define	VXC_FDTATOX	0x0300		/* flush data */
#define	VXC_LPARAX	0x0600		/* load params */
#define	VXC_SNDBRKX	0x0C00		/* send break to port */
#define	VXC_MDMCTL	0x1000		/* auto modem control */
/* bisync specific command identifiers */
#define	VXC_LPARAX1	0x060a
#define	VXC_MDMCTL1	0x1004
#define	VXC_HUNTMD1	0x0804

/* vioc types returned during config */
#define	VXT_VIOCBOP	0x05		/* vioc-bop */
#define	VXT_PVIOCX	0x0A		/* old connector panel vioc-x */
#define	VXT_VIOCX	0x0B		/* new connector panel vioc-x */
#define	VXT_VIOCB	0x0C		/* vioc-bisync */
#define	VXT_NEW		0x10		/* new type bit (or'd in) */

#define	VX_BISYNC	0x1		/* bisync flag indicator for bscport */

/* connector panel types (per port) */
#define	VXT_NONE	0		/* no connector panel */
#define	VXT_8PORT	1		/* 8 port RS-232C */
#define	VXT_RS422	2		/* 8 port RS-422 (nonexistent) */
#define	VXT_4PORT	3		/* 4 port RS-232 (with DSR/RING) */
#define	VXT_PARALLEL	4		/* 4 port panel parallel port */

/* v_fault status values */
#define	VXF_READY	0x55		/* no err in vioc self-initializaton */

/* line parameters, set with VXC_LPARAX */
#define	BITS5		0x00		/* 5 bits per character */
#define	BITS6		0x80		/* 6 bits per character */
#define	BITS7		0x40		/* 7 bits per character */
#define	BITS8		0xc0		/* 8 bits per character */

#define	VNOPARITY	0x00		/* no parity bit */
#define	VODDP		0x01		/* odd parity bit */
#define	VEVENP		0x03		/* even parity bit */

#define	VSTOP1		0x04		/* 1 stop bit */
#define	VSTOP2		0x0c		/* 2 stop bit */

#define	V19200		0x13		/* 19.2 kbaud */

/* modem control flags */
#define	VMOD_ON		1
#define	VMOD_OFF	0

#define	V_AUTO		0x00		/* auto control of RTS, uses CTS */
#define	V_MANUAL	0x80		/* manual control of RTS, ignore CTS */
#define	V_DTR_ON	0x02		/* set DTR output */
#define	V_DTR_OFF	0x00		/* drop DTR output */
#define	V_RTS		0x01		/* set RTS output (manual only) */

#define	BRK_CHR	040			/* break character */
#define	DCD_ON	020			/* */
#define	DCD_OFF	010			/* */
#define	CTS_ON	004			/* */
#define	DSR_ON	0200			/* modem signal states for bisync */
#define	DSR_OFF 0100
#define	DSR_CHG	(DSR_ON|DSR_OFF)

#define	VX_SILO	0x800			/* offset to base of silo */

/* input status bits returned in silo */
#define	VX_PE	0x40			/* parity error */
#define	VX_FE	0x80			/* framing error */
#define	VX_RO	0xc0			/* receiver overrun */

#define	VRESPLEN	12
#define	VCMDLEN		64
#define	VC_IQLEN	64		/* Interrupt circular queue length */
#define	NVCXBUFS	16*3		/* 3 bufs per port per viocx */
#define	VC_CMDBUFL	NVCXBUFS	/* circular cmd (to exec) queue len*/

struct	vcmds {
	int	v_cmdsem;		/* # cmds waiting for itrque */
	int	v_curcnt;		/* # cmds in itrque and executing */
	caddr_t	v_curcmd[VCMDLEN];	/* pointers to cmds being executed */
	int	v_fill;			/* circular fill index */
	int	v_empty;		/* circular empty index */
	caddr_t	cmdbuf[VC_CMDBUFL];	/* circular cmd (to exec) queue */
	int	v_itrfill;		/* circular intr issue queue fill */
	int	v_itrempt;		/* circular intr issue queue empty */
	short	v_itrqueu[VC_IQLEN];	/* circular intr issue queue */
};

struct	vxcmd {
	struct	vxcmd *c_fwd;
	short	cmd;
	char	par[58];		/* pad to 64 total size */
};

struct	vxmit {
	char	line;
	char	bcount;
	char	ostream[6];
};

#define	SSPEED	13			/* standard speed 9600 bps */
