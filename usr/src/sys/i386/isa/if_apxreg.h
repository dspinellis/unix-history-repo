/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_apxreg.h	7.1 (Berkeley) %G%
 */


/* SGS Thompson MK5205 structures and registers */

/* 
 * Device Control Ports
 * (Multiplexed CSR's)
 */
struct sgcp {
	u_short sgcp_rdp;
	u_short sgcp_rap;
};
/*
 * Operating Parameters for timers etc.
 * (Suitable for reseting by ioctl).
 */ 
struct sgop {
	u_short lsaddr;
	u_short rsaddr;
	u_short	n1;
	u_short	n2_scale;
	u_short	t1;
	u_short	t3;
	u_short	tp;
};

/*
 * common format for tx/rx descriptors
 */

struct sgdx {
	u_short	sgdx_flags;
	u_short	sgdx_addr;
	short	sgdx_bcnt;
	u_short	sgdx_mcnt;
};

/*
 * interpretation of reported errors 
 */
struct sger {
	u_short	sger_bad;
	u_short	sger_t1timo;
	u_short	sger_frmr;
	u_short	sger_txrej;
	u_short	sger_rxrej;
	u_short	sger_short;
};

/*
 * Status Buffer, paried bytes hi order low order
 */
struct sgsb {
	u_short	sgsb_vrvs;
	u_short	sgsb_lsrs;
	u_short sgsb_phzva;
	u_short	sgsb_hirxcnt;
	u_short	sgsb_lorxcnt;
	u_short	sgsb_hitxcnt;
	u_short	sgsb_lotxcnt;
};

#define SGMTU 1032
#define SGRBUF 16
#define SGRBUFLOG2 4
#define SG_RLEN (SGRBUF_LOG2 << 12)
#define SGTBUF 8
#define SGTBUFLOG2 3
#define SG_TLEN (SGTBUF_LOG2 << 12)

/*
 * APC-PCX RAM
 */

struct apc_mem {
	/* Initialization Block */
	u_short	apc_mode;
	struct	sgop apc_sgop;
	u_short	apc_rlen;
	u_short	apc_rdra;
	u_short	apc_tlen;
	u_short	apc_tdra;
	struct	sgdx apc_txtid;
	struct	sgdx apc_rxtid;
	u_short	apc_stathi;
	u_short	apc_statlo;
	struct	sger apc_sger;
	struct	sgsb apc_sgsb;		/* Status Buffer */
	struct	sgdx apc_rxmd[SGRBUF];	/* Receive Message Descriptors */
	struct	sgdx apc_txmd[SGTBUF];	/* Transmit Message Descriptors */
	char	apc_rbuf[SGRBUF][SGMTU]; /* Receive Message Buffers */
	char	apc_tbuf[SGTBUF][SGMTU]; /* Transmit Message Buffers */
	char	apc_rxidbuf[SGMTU];
	char	apc_txidbuf[SGMTU];
};
/*
 * APC-PCX registers
 */
struct apc_reg {
	struct	sgcb axr_sgcb[2];
	u_char	arx_pad0[4];
	u_char	axr_cnt0;
	u_char	axr_cnt1;
	u_char	axr_cnt2;
	u_char	axr_ccr;
	u_char	axr_mode;
	u_char	axr_pad1;
	u_char	axr_status;
	u_char	axr_pad2;
	u_char	axr_altmode;
	u_char	axr_pad3[11];
};

/*
 * Hardware tweaking
 */
struct apc_modes {
	struct	sgop apm_sgop;	/* tweak timers */
	u_char	apm_txwin;	/* set parameter for transmit window */
	u_char	apm_apxmode;
	u_char	apm_apxaltmode;
	u_char 	apm_iftype;	/* someday indicate PPP vs X.25 */
};

#define	SIOCSIFMODE	_IOW('i', 127, struct apc_modes) /* set parameters */

/* TX RX descriptor bits */
#define SG_OWN 0x8000
#define SG_SLF 0x2000
#define SG_ELF 0x1000
#define	SG_TUI 0x800

/* CSR0 quantities */
#define SG_TDMD	0x8000
#define SG_STOPPED 0x4000
#define SG_DTX	0x2000
#define SG_DRX	0x1000
#define SG_TXON 0x800
#define SG_RXON 0x400
#define SG_INEA 0x200
#define SG_INTR 0x100
#define SG_MERR 0x80
#define SG_MISS 0x40
#define SG_ROR	0x20
#define SG_TUR	0x10
#define SG_PINT 0x8
#define SG_TINT 0x4
#define SG_RINT 0x2

/* CSR1 quantities */
#define SG_UAV	0x4000
#define SG_PAV	0x40
#define SG_STOP	0
#define	SG_INIT	(2 << 8)
#define SG_TRANS (3 << 8)
#define SG_STAT	(4 << 8)

#define SG_PROM	0x400
#define SG_UIE	0x200
#define SG_BSWAP 0x20
