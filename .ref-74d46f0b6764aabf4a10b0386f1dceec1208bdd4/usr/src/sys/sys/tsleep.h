/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tsleep.h	7.4 (Berkeley) %G%
 */

#ifndef _TSLEEP_
#define _TSLEEP_

#define tsleep ttsleep	/* tmp tsleep, does longjmp */
#define SLP_WAIT	"child"		/* "wait" */
#define SLP_LOGREAD	"syslog"	/* "log_read" */
#define SLP_SELECT	"select"	/* "select" */
#define SLP_TTYOUT	"ttyout"	/* "ttyout" */
#define SLP_TTY_NOTFG	"tty"		/* "tty_notfg" */
#define SLP_TTY_CARR	"ttyopn"	/* "tty_carrier" */
#define SLP_TTYIN_NOTFG	"ttyin"		/* "ttyin_notfg" */
#define SLP_TTY_READ	"ttyin"		/* "ttread" */
#define SLP_TTY_TSTP	"ttyin"		/* "tty_tstp" */
#define SLP_TTYOUT_NOTFG	"ttyout"	/* "ttyout_notfg" */
#define SLP_TTYOUT_CLIST	"ttybuf"	/* "tty_clists" */
#define SLP_PTS_OPEN	"ttyopn"	/* "tty_ptsopen" */
#define SLP_PTC_WRITE	"ttyout"	/* "tty_ptcwrite" */
#define SLP_SO_LINGER	"netcls"	/* "so_linger" */
#define SLP_SO_SBWAIT	"netio"		/* "so_sbwait" */
#define SLP_SO_ACCEPT	"netcon"	/* "so_accept" */
#define SLP_SO_ACCEPT2	"netcon"	/* "so_accept2" */
#define SLP_EXLCK	"flock"		/* "exlock" */
#define SLP_SHLCK	"flock"		/* "shlock" */
#define SLP_ISO_CONSOUT	"netcon"	/* "iso_cons" */
#define SLP_ISO_CONSCONN	"netcon"	/* "iso_consconn" */
#define SLP_NFS_IOD	"nfsio"		/* "nfs_iod" */
#define SLP_SO_SBLOCK	"netio"		/* "so_sblock" */
#define SLP_TU_OPN	"devopn"	/* "tu58_opn" */
#define SLP_MFS		"mfsio"		/* "mfs_idle" */
#define SLP_FIFO_OPEN	"devopn"	/* "fifo_open" */
#define SLP_HP_OPEN	"devopn"	/* "hp_open" */
#define SLP_AD_GETW	"devi"		/* "ad_getw" */
#define SLP_PCAT_OUT	"devout"		/* "pcat_out" */
#define SLP_PCAT_CLIST	"ttybuf"	/* "pcat_clist" */
#define SLP_DH_OPN	"ttyopn"	/* "dh_opn" */
#define SLP_DHU_OPN	"ttyopn"	/* "dhu_opn" */
#define SLP_DMFL_ASLP	"ttyout"		/* "dmfl_aslp" */
#define SLP_DMFL_ERROR	"ttyout"		/* "dmfl_err" */
#define SLP_DMX_OPN	"devopn"	/* "dmx_opn" */
#define SLP_DN_REG	"ttyout"	/* "dn_reg" */
#define SLP_DN_PAUSE	"ttyout"	/* "dn_pause" */
#define SLP_DZ_OPN	"ttyopn"	/* "dz_opn" */
#define SLP_IK_BUSY	"devout"		/* "ik_busy" */
#define SLP_LP_OUT	"devout"	/* "lp_out" */
#define SLP_LP_CLIST	"ttybuf"	/* "lp_clist" */
#define SLP_NP_SLP	"devin"		/* "np_slp" */
#define SLP_PS_REFRESH	"devout"		/* "ps_refresh" */
#define SLP_PS_MAP	"devout"		/* "ps_map" */
#define SLP_TM_OPN	"devopn"		/* "tm_opn" */
#define SLP_UDA_OPN	"devopn"	/* "uda_opn" */
#define SLP_UT_OPN	"devopn"		/* "ut_opn" */
#define SLP_UU_OPN	"devopn"		/* "uu_opn" */
#define SLP_VS_WAIT	"devout"		/* "vs_wait" */
#define SLP_VS_USRWAIT	"devout"		/* "vs_usrwait" */
#define SLP_VS_START	"devout"	/* "vs_start" */
#define SLP_VS_ABORT	"devout"	/* "vs_abort" */
#define SLP_VS_PWRUP	"devout"	/* "vs_pwrup" */
#define SLP_VS_IOBCTL	"devout"	/* "vs_iobctl" */
#define SLP_VS_FIB	"devout"	/* "vs_fib" */
#define SLP_VS_FIBRET	"devout"	/* "vs_fibret" */
#define SLP_VS_INITF	"devout"	/* "vs_initf" */
#define SLP_VS_INITDEV	"devout"	/* "vs_initdev" */
#define SLP_DR_WAIT	"devout"	/* "dr_wait" */
#define SLP_DR_RESET	"devout"	/* "dr_reset" */
#define SLP_DR_ACTV	"devout"	/* "dr_actv" */
#define SLP_HD_OPN	"devopn"	/* "hd_opn" */
#define SLP_MP_1OPN	"ttyopn"	/* "mp_1opn" */
#define SLP_MP_POPN	"ttyopn"	/* "mp_popn" */
#define SLP_MP_OPN	"ttyopn"	/* "mp_opn" */
#define SLP_MP_1CLS	"ttycls"	/* "mp_1cls" */
#define SLP_MP_CLS	"ttycls"	/* "mp_cls" */
#define SLP_MP_BRK	"ttyout"	/* "mp_brk" */
#define SLP_MP_STDL	"ttyout"	/* "mp_stdl" */
#define SLP_MP_DLWAIT	"ttyout"	/* "mp_dlwait" */
#define SLP_VD_OPN	"devopn"	/* "vd_opn" */
#define SLP_VX_OPN	"ttyopn"	/* "vx_opn" */
#define SLP_VX_CLS	"ttycls"	/* "vx_cls" */
#define SLP_VX_PARAM	"ttyout"	/* "vx_param" */
#endif
