/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_apxreg.h	8.1 (Berkeley) 6/11/93
 */


/*
 * SGS Thompson MK5205 structures and registers
 */

/* 
 * Device Control Ports (Multiplexed CSR's)
 */
struct sgcp {
	u_short	sgcp_rdp;
	u_short	sgcp_rap;
};

/*
 * Operating Parameters for timers etc.
 * (Suitable for reseting by ioctl).
 */ 
struct sgop {
	u_short	lsaddr;
	u_short	rsaddr;
	u_short	n1;
	u_short	n2_scale;
	u_short	t1;
	u_short	t3;
	u_short	tp;
};

/*
 * Common addressing element rife through chip
 */
struct sgae {
	u_short	f_hi;
	u_short	lo;
};
/*
 * Common format for tx/rx descriptors
 */

struct sgdx {
	struct	sgae sgdx_ae;
#define		sgdx_flags sgdx_ae.f_hi
#define		sgdx_addr sgdx_ae.lo
	short	sgdx_bcnt;
	short	sgdx_mcnt;
};

/*
 * Interpretation of reported errors 
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
 * Status buffer, paired bytes hi order low order
 */
struct sgsb {
	u_short	sgsb_vrvs;
	u_short	sgsb_lsrs;
	u_short	sgsb_phzva;
	u_short	sgsb_hirxcnt;
	u_short	sgsb_lorxcnt;
	u_short	sgsb_hitxcnt;
	u_short	sgsb_lotxcnt;
};

#define SGMTU 1032
#define SGRBUF 16
#define SGRBUF_LOG2 4
#define SG_RLEN (SGRBUF_LOG2 << 12)
#define SGTBUF 8
#define SGTBUF_LOG2 3
#define SG_TLEN (SGTBUF_LOG2 << 12)

/*
 * APC-PCX RAM
 */

struct apc_mem {
	/* Initialization Block */
	u_short	apc_mode;
	struct	sgop apc_sgop;
	struct	sgae apc_rxdd;
	struct	sgae apc_txdd;
	struct	sgdx apc_txtid;
	struct	sgdx apc_rxtid;
	struct	sgae apc_stdd;
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
	struct	sgcp axr_sgcp[2];
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
	u_char	apm_iftype;	/* someday indicate PPP vs X.25 */
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
#define SG_INIT	(2 << 8)
#define SG_TRANS (3 << 8)
#define SG_STAT	(4 << 8)

#define SG_PROM	0x400
#define SG_UIE	0x200
#define SG_BSWAP 0x20
