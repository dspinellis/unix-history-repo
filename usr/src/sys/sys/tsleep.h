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
 *	@(#)tsleep.h	7.1 (Berkeley) %G%
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
#define	SLP_SO_LINGER	14
	"so_linger",
#define	SLP_SO_SBWAIT	15
	"so_sbwait",
#define	SLP_SO_ACCEPT	16
	"so_accept",
#define	SLP_SO_ACCEPT2	17
	"so_accept2",
#define	SLP_EXLCK	18
	"exlock",
#define	SLP_SHLCK	19
	"shlock",
#define	SLP_ISO_CONSOUT	20
	"iso_cons",
#define	SLP_ISO_CONSCONN 21
	"iso_consconn",
#define	SLP_NFS_IOD	22
	"nfs_iod",
#define	SLP_SO_SBLOCK	23
	"so_sblock",
#define	SLP_TU_OPN	24
	"tu58_opn",
#define	SLP_MFS		25
	"mfs_idle",
#define	SLP_HP_OPEN	26
	"hp_open",
#define	SLP_AD_GETW	27
	"ad_getw",
#define	SLP_PCAT_OUT	28
	"pcat_out",
#define	SLP_PCAT_CLIST	29
	"pcat_clist",
#define	SLP_DH_OPN	30
	"dh_opn",
#define	SLP_DHU_OPN	31
	"dhu_opn",
#define	SLP_DMFL_ASLP	32
	"dmfl_aslp",
#define	SLP_DMFL_ERROR	33
	"dmfl_err",
#define	SLP_DMX_OPN	34
	"dmx_opn",
#define	SLP_DN_REG	35
	"dn_reg",
#define	SLP_DN_PAUSE	36
	"dn_pause",
#define	SLP_DZ_OPN	37
	"dz_opn",
#define	SLP_IK_BUSY	38
	"ik_busy",
#define	SLP_LP_OUT	39
	"lp_out",
#define	SLP_LP_CLIST	40
	"lp_clist",
#define	SLP_NP_SLP	41
	"np_slp",
#define	SLP_PS_REFRESH	42
	"ps_refresh",
#define	SLP_PS_MAP	43
	"ps_map",
#define	SLP_TM_OPN	44
	"tm_opn",
#define	SLP_UDA_OPN	45
	"uda_opn",
#define SLP_UT_OPN	46
	"ut_opn",
#define	SLP_UU_OPN	47
	"uu_opn",
#define	SLP_VS_WAIT	48
	"vs_wait",
#define SLP_VS_USRWAIT	49
	"vs_usrwait",
#define	SLP_VS_START	50
	"vs_start",
#define SLP_VS_ABORT	51
	"vs_abort",
#define SLP_VS_PWRUP	52
	"vs_pwrup",
#define	SLP_VS_IOBCTL	53
	"vs_iobctl",
#define	SLP_VS_FIB	54
	"vs_fib",
#define	SLP_VS_FIBRET	55
	"vs_fibret",
#define	SLP_VS_INITF	56
	"vs_initf",
#define SLP_VS_INITDEV	57
	"vs_initdev",
#define	SLP_DR_WAIT	58
	"dr_wait",
#define	SLP_DR_RESET	59
	"dr_reset",
#define	SLP_DR_ACTV	60
	"dr_actv",
#define	SLP_HD_OPN	61
	"hd_opn",
#define	SLP_MP_1OPN	62
	"mp_1opn",
#define	SLP_MP_POPN	63
	"mp_popn",
#define	SLP_MP_OPN	64
	"mp_opn",
#define	SLP_MP_1CLS	65
	"mp_1cls",
#define	SLP_MP_CLS	66
	"mp_cls",
#define	SLP_MP_BRK	67
	"mp_brk",
#define	SLP_MP_STDL	68
	"mp_stdl",
#define	SLP_MP_DLWAIT	69
	"mp_dlwait",
#define	SLP_VD_OPN	70
	"vd_opn",
#define	SLP_VX_OPN	71
	"vx_opn",
#define	SLP_VX_CLS	72
	"vx_cls",
#define	SLP_VX_PARAM	73
	"vx_param",
};
#endif
#endif
