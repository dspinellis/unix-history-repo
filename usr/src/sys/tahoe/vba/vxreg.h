/*	vxreg.h	1.1	85/07/21	*/

#define V_RESETTING 1		/* in process of reseting */
#define V_RESET 1		/* cause a vioc reset */
#define V_TRP	2		/* cause a vioc trap */
#define V_INTR	4		/* cause a vioc interrupt */
#define V_ERR	0x80		/* generic error flag */
#define V_BSY	0x80		/* generic busy flag */
#define V_UNBSY	0x80		/* not busy for unsolicited interrupt */

#define	VCVECT	0x40 		/* base vioc vector */

#define	VBAS(x)  ( (struct vblok *)(VIOCBAS[x]) )

#define	NVIOCX NVX		/* nbr of viocx's - defined by config in vx.h */
#define NVXPORTS 16*NVIOCX	/* Total nbr of viocx terminal ports */
#define NVCXBUFS 16*3		/* 3 bufs per port per viocx */
#define	VREADY 0x55		/* denotes no err in vioc self-initializaton */
#define PVIOCX	0x0A		/* old connector panel vioc-x */
#define VIOCX	0x0B		/* new (modular) connector panel vioc-x */
#define	NPVIOCX	0x1A		/* new pviocx */
#define	NWVIOCX	0x1B		/* new viocx */
#define	VBOPID	0x5		/* Vioc-bop */

#include "vbsc.h"
#if NVBSC > 0
#define VIOCB	0x0C
#define NWVIOCB 0x1C
#define BISYNC 0x1
#define LPARAX1 0x060a
#define MDMCTL1 0x1004
#define HUNTMD1 0x0804
#endif


#define	V_NEW	1		/* NPVIOCX | NVIOCX */
#define	V_OLD	0		/* PVIOCX | VIOCX */
/* Modem Control flags */
#define VMOD_ON 1
#define VMOD_OFF 0

#define VRESPLEN 12
#define VCMDLEN 64
#define VC_IQLEN 64			/* Interrupt circular queue length */
#define VC_CMDBUFL NVCXBUFS		/* circular cmd (to exec) queue len*/

#define SSPEED	13		/* standard speed 9600 bps */

#define V_ENAB	0002	/* auto + DTR */
#define V_DISAB	0000	/* auto + disable DTR */

#define BRK_CHR	040
#define DCD_ON	020
#define DCD_OFF	010
#define	CTS_ON	004
#define DSR_ON	0200	/* modem signal states for bisync */
#define DSR_OFF 0100
#define DSR_CHG	(DSR_ON | DSR_OFF)

#define SILOBAS 0x800
/* Command Operation Codes */
#define LIDENT	0x0000		/* load ident, set int vectors */
#define XMITDTA	0x0200		/* transmit */
#define XMITIMM	0x0400		/* xmit immediate */
#define FDTATOX	0x0300		/* flush data */
#define	LPARAX	0x0600		/* load params */
#define SNDBRKX	0x0C00		/* send break to port */
#define MDMCTL	0x1000		/* auto modem control */

#define PERROR	0x40
#define FERROR	0x80
#define RCVOVRN	0xc0

/*
 * Data Structures
 */

struct	vblok	{		/* command sent to vioc  */
	char	v_vioc;		/*  0 type of interrupt + voic bsy flg */
	char	v_hdwre;	/*  1 trap, reset, or hard interrupt */
	char	v_vcbsy;	/*  2 command busy (set by host) */
	char	v_vcid;		/*  3 command id. */
	short	v_vcp[2];	/*  4 command pointer (sent by host) */
	short	v_unused;	/*  8 unused */
	short	v_rspoff;	/*  a offset into vblock for response buf */
	char	v_ustat;	/*  c status */
	char	v_uqual;	/*  d qualifies the interrupt */
	char	v_usdata[0x3E];	/*  e unsolicited interrupt data */
	short	v_maxxmt ;	/* 4C max xmit block size */
	short	v_maxsilo ;	/* 4E max silo size */
	char	v_ident ;	/* 50 identifies type of vioc */
	char	v_fault ;	/* 51 denotes fault or ready after reset */
	short	v_dcd ;		/* 52 bit mask for carr detect by port */
	short	v_cts ;		/* 54 bit mask for cts by port */
	short	v_dsr;		/* 56 bit mask for dsr by port */
	short	v_secrec;	/* 58 bit mask for secondary receive */
	short	v_badport;	/* 5a bit mask of failed ports */
	char	v_portyp[16];	/* 5c type of each port */
} ;

struct	vcmds	{
	int	v_cmdsem;		/* cmds waiting for itrque */
	int	v_curcnt;		/* count of cmds in itrque and executing */
	caddr_t	v_curcmd[VCMDLEN] ;	/* pointers to cmds being executed */
	int	v_fill ;		/* circular fill index */
	int	v_empty ;		/* circular empty index */
	caddr_t	cmdbuf[VC_CMDBUFL] ;	/* circular cmd (to exec) queue */
	int	v_itrfill ;		/* circular intr issue queue fill */
	int	v_itrempt ;		/* circular intr issue queue empty */
	short	v_itrqueu[VC_IQLEN] ;	/* circular intr issue queue */
} ;

struct	vxcmd	{
	struct	vxcmd	*c_fwd ;
	short	cmd ;
	char	par[58] ; /* 64 total size */
} ;

struct	vxmit	{
	char	line ;
	char	bcount ;
	char	ostream[6] ;
} ;
/* should be sizeof(struct vxmit), but has alignment problems */
#define sizvxmit 6

struct	vcx	{
	char	v_loport ;	/* low port nbr */
	char	v_hiport ;	/* high port nbr */
	short	v_nbr ;		/* viocx number */
	short	v_maxcmd ;	/* max number of concurrent cmds */
	short	v_silosiz ;	/* silo size */
	short	v_vers ;	/* vioc/pvioc version */
	char	v_actflg ;	/* active command */
	char	v_xmtcnt ;	/* xmit commands pending */
	char	v_actport[16] ;	/* act flag per port per port */
	short	v_brkreq ;	/* send break requests pending */
	struct	vxcmd	*vx_avail ;
	struct	vxcmd	*vx_build ;
	struct	vxcmd	vx_lst[NVCXBUFS] ;
	short 	v_state;
	caddr_t v_mricmd;	/* Most recent issued cmd */
};

#ifdef KERNEL
caddr_t VIOCBAS[NVIOCX];	/* base I/O addr */
#define vpanic(x) printf("%s\n", x)
#endif
