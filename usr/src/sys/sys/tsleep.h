/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)tsleep.h	7.2 (Berkeley) %G%
 */
#ifndef _TSLEEP_
#define _TSLEEP_

#define	SLP_WAIT	0
#define	SLP_PAUSE	1
#define	SLP_LOGREAD	2
#define	SLP_SELECT	3
#define	SLP_TTYOUT	4
#define	SLP_TTY_NOTFG	5
#define	SLP_TTY_CARR	6
#define	SLP_TTYIN_NOTFG	7
#define	SLP_TTY_READ	8
#define	SLP_TTY_TSTP	9
#define	SLP_TTYOUT_NOTFG	10
#define	SLP_TTYOUT_CLIST	11
#define	SLP_PTS_OPEN	12
#define	SLP_PTC_WRITE	13
#define	SLP_SO_LINGER	14
#define	SLP_SO_SBWAIT	15
#define	SLP_SO_ACCEPT	16
#define	SLP_SO_ACCEPT2	17
#define	SLP_EXLCK	18
#define	SLP_SHLCK	19
#define	SLP_ISO_CONSOUT	20
#define	SLP_ISO_CONSCONN 21
#define	SLP_NFS_IOD	22
#define	SLP_SO_SBLOCK	23
#define	SLP_TU_OPN	24
#define	SLP_MFS		25
#define	SLP_HP_OPEN	26
#define	SLP_AD_GETW	27
#define	SLP_PCAT_OUT	28
#define	SLP_PCAT_CLIST	29
#define	SLP_DH_OPN	30
#define	SLP_DHU_OPN	31
#define	SLP_DMFL_ASLP	32
#define	SLP_DMFL_ERROR	33
#define	SLP_DMX_OPN	34
#define	SLP_DN_REG	35
#define	SLP_DN_PAUSE	36
#define	SLP_DZ_OPN	37
#define	SLP_IK_BUSY	38
#define	SLP_LP_OUT	39
#define	SLP_LP_CLIST	40
#define	SLP_NP_SLP	41
#define	SLP_PS_REFRESH	42
#define	SLP_PS_MAP	43
#define	SLP_TM_OPN	44
#define	SLP_UDA_OPN	45
#define SLP_UT_OPN	46
#define	SLP_UU_OPN	47
#define	SLP_VS_WAIT	48
#define SLP_VS_USRWAIT	49
#define	SLP_VS_START	50
#define SLP_VS_ABORT	51
#define SLP_VS_PWRUP	52
#define	SLP_VS_IOBCTL	53
#define	SLP_VS_FIB	54
#define	SLP_VS_FIBRET	55
#define	SLP_VS_INITF	56
#define SLP_VS_INITDEV	57
#define	SLP_DR_WAIT	58
#define	SLP_DR_RESET	59
#define	SLP_DR_ACTV	60
#define	SLP_HD_OPN	61
#define	SLP_MP_1OPN	62
#define	SLP_MP_POPN	63
#define	SLP_MP_OPN	64
#define	SLP_MP_1CLS	65
#define	SLP_MP_CLS	66
#define	SLP_MP_BRK	67
#define	SLP_MP_STDL	68
#define	SLP_MP_DLWAIT	69
#define	SLP_VD_OPN	70
#define	SLP_VX_OPN	71
#define	SLP_VX_CLS	72
#define	SLP_VX_PARAM	73
#endif

#ifdef SLP_MSGLIST
char *slp_mesg[] = {
	"wait",
	"pause",
	"log_read",
	"select",
	"ttyout",
	"tty_notfg",
	"tty_carrier",
	"ttyin_notfg",
	"ttread",
	"tty_tstp",
	"ttyout_notfg",
	"tty_clists",
	"tty_ptsopen",
	"tty_ptcwrite",	/* 13 */
	"so_linger",
	"so_sbwait",
	"so_accept",
	"so_accept2",
	"exlock",
	"shlock",
	"iso_cons",
	"iso_consconn",
	"nfs_iod",
	"so_sblock",
	"tu58_opn",
	"mfs_idle",
	"hp_open",
	"ad_getw",
	"pcat_out",
	"pcat_clist",
	"dh_opn",
	"dhu_opn",
	"dmfl_aslp",
	"dmfl_err",
	"dmx_opn",
	"dn_reg",
	"dn_pause",
	"dz_opn",
	"ik_busy",
	"lp_out",
	"lp_clist",
	"np_slp",
	"ps_refresh",
	"ps_map",
	"tm_opn",
	"uda_opn",
	"ut_opn",
	"uu_opn",
	"vs_wait",
	"vs_usrwait",
	"vs_start",
	"vs_abort",
	"vs_pwrup",
	"vs_iobctl",
	"vs_fib",
	"vs_fibret",
	"vs_initf",
	"vs_initdev",
	"dr_wait",
	"dr_reset",
	"dr_actv",
	"hd_opn",
	"mp_1opn",
	"mp_popn",
	"mp_opn",
	"mp_1cls",
	"mp_cls",
	"mp_brk",
	"mp_stdl",
	"mp_dlwait",
	"vd_opn",
	"vx_opn",
	"vx_cls",
	"vx_param",
};
#endif
