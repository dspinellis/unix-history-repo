/*	vxreg.h	1.2	86/01/13	*/

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
#define V_RESET 0x1		/* cause a vioc reset */
#define V_TRP	0x2		/* cause a vioc trap */
#define V_INTR	0x4		/* cause a vioc interrupt */
#define V_ERR	0x80		/* generic error flag */
#define V_BSY	0x80		/* generic busy flag */
#define V_UNBSY	0x80		/* not busy for unsolicited interrupt */

/* command identifier codes */
#define VXC_LIDENT	0x0000		/* load ident, set int vectors */
#define VXC_XMITDTA	0x0200		/* transmit */
#define VXC_XMITIMM	0x0400		/* xmit immediate */
#define VXC_FDTATOX	0x0300		/* flush data */
#define	VXC_LPARAX	0x0600		/* load params */
#define VXC_SNDBRKX	0x0C00		/* send break to port */
#define VXC_MDMCTL	0x1000		/* auto modem control */
/* bisync specific command identifiers */
#define VXC_LPARAX1	0x060a
#define VXC_MDMCTL1	0x1004
#define VXC_HUNTMD1	0x0804

/* vioc types returned during config */
#define	VXT_VIOCBOP	0x05		/* vioc-bop */
#define VXT_PVIOCX	0x0A		/* old connector panel vioc-x */
#define VXT_VIOCX	0x0B		/* new connector panel vioc-x */
#define VXT_VIOCB	0x0C		/* vioc-bisync */
#define	VXT_NEW		0x10		/* new type bit (or'd in) */

#define VX_BISYNC	0x1		/* bisync flag indicator for bscport */

/* v_fault status values */
#define	VXF_READY	0x55		/* no err in vioc self-initializaton */

/* modem control flags */
#define VMOD_ON		1
#define VMOD_OFF	0

#define V_ENAB	0002			/* auto + DTR */
#define V_DISAB	0000			/* auto + disable DTR */

#define BRK_CHR	040			/* break character */
#define DCD_ON	020			/* */
#define DCD_OFF	010			/* */
#define	CTS_ON	004			/* */
#define DSR_ON	0200			/* modem signal states for bisync */
#define DSR_OFF 0100
#define DSR_CHG	(DSR_ON|DSR_OFF)

#define VX_SILO	0x800			/* offset to base of silo */

/* input status bits returned in silo */
#define	VX_PE	0x40			/* parity error */
#define	VX_FE	0x80			/* framing error */
#define	VX_RO	0xc0			/* receiver overrun */

#define VRESPLEN	12
#define VCMDLEN		64
#define VC_IQLEN	64		/* Interrupt circular queue length */
#define NVCXBUFS	16*3		/* 3 bufs per port per viocx */
#define VC_CMDBUFL	NVCXBUFS	/* circular cmd (to exec) queue len*/

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

#define SSPEED	13			/* standard speed 9600 bps */
