/* GNU Emacs VMS UAF definition file.
   Copyright (C) 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * User Authorization File record formats
 */
#ifndef UAF$K_LENGTH

struct UAF {
#define	UAF$C_USER_ID	1
#define	UAF$C_VERSION1	1
#define	UAF$C_KEYED_PART	52
#define	UAF$C_AD_II	0
#define	UAF$C_PURDY	1
#define	UAF$C_PURDY_V	2
#define	UAF$K_FIXED	644
#define	UAF$C_FIXED	644
#define	UAF$K_LENGTH	1412
#define	UAF$C_LENGTH	1412
#define	UAF$S_UAFDEF	1412
#define	UAF$B_RTYPE	0
  char uaf$b_rtype;
#define	UAF$B_VERSION	1
  char uaf$b_version;
#define	UAF$W_USRDATOFF	2
  short	uaf$w_usrdatoff;
#define	UAF$S_USERNAME	32
#define	UAF$T_USERNAME	4
#define	UAF$T_USERNAME_TAG	35
  char uaf$t_username[UAF$S_USERNAME];
#define	UAF$L_UIC	36
#define	UAF$W_MEM	36
#define	UAF$W_GRP	38
  union {
    long uaf_l_uic;
#define	uaf$l_uic	uaf_u_uic.uaf_l_uic
    struct {
      short uaf_w_mem;
      short uaf_w_grp;
#define	uaf$w_mem	uaf_u_uic.uaf_u_mem_grp.uaf_w_mem
#define	uaf$w_grp	uaf_u_uic.uaf_u_mem_grp.uaf_w_grp
    } uaf_u_mem_grp;
  } uaf_u_uic;
#define	UAF$L_SUB_ID	40
  long	uaf$l_sub_id;
#define	UAF$S_PARENT_ID	8
#define	UAF$Q_PARENT_ID	44
  char	uaf$q_parent_id[UAF$S_PARENT_ID];
#define	UAF$S_ACCOUNT	32
#define	UAF$T_ACCOUNT	52
  char	uaf$t_account[UAF$S_ACCOUNT];
#define	UAF$S_OWNER	32
#define	UAF$T_OWNER	84
  char	uaf$t_owner[UAF$S_OWNER];
#define	UAF$S_DEFDEV	32
#define	UAF$T_DEFDEV	116
  char	uaf$t_defdev[UAF$S_DEFDEV];
#define	UAF$S_DEFDIR	64
#define	UAF$T_DEFDIR	148
  char	uaf$t_defdir[UAF$S_DEFDIR];
#define	UAF$S_LGICMD	64
#define	UAF$T_LGICMD	212
  char	uaf$t_lgicmd[UAF$S_LGICMD];
#define	UAF$S_DEFCLI	32
#define	UAF$T_DEFCLI	276
  char	uaf$t_defcli[UAF$S_DEFCLI];
#define	UAF$S_CLITABLES	32
#define	UAF$T_CLITABLES	308
  char	uaf$t_clitables[UAF$S_CLITABLES];
#define	UAF$S_PWD	8
#define	UAF$Q_PWD	340
#define	UAF$L_PWD	340
  char	uaf$q_pwd[UAF$S_PWD];
#define uaf$l_pwd	uaf$q_pwd[0]
#define	UAF$S_PWD2	8
#define	UAF$Q_PWD2	348
  char	uaf$q_pwd2[UAF$S_PWD2];
#define	UAF$W_LOGFAILS	356
  short	uaf$w_logfails;
#define	UAF$W_SALT	358
  short	uaf$w_salt;
#define	UAF$B_ENCRYPT	360
  char	uaf$b_encrypt;
#define	UAF$B_ENCRYPT2	361
  char	uaf$b_encrypt2;
#define	UAF$B_PWD_LENGTH	362
  char	uaf$b_pwd_length;
#define	UAF$S_EXPIRATION	8
#define	UAF$Q_EXPIRATION	364
  char	uaf$q_expiration[UAF$S_EXPIRATION];
#define	UAF$S_PWD_LIFETIME	8
#define	UAF$Q_PWD_LIFETIME	372
  char	uaf$q_pwd_lifetime[UAF$S_PWD_LIFETIME];
#define	UAF$S_PWD_DATE	8
#define	UAF$Q_PWD_DATE	380
  char	uaf$q_pwd_date[UAF$S_PWD_DATE];
#define	UAF$S_PWD2_DATE	8
#define	UAF$Q_PWD2_DATE	388
  char	uaf$q_pwd2_date[UAF$S_PWD2_DATE];
#define	UAF$S_LASTLOGIN_I	8
#define	UAF$Q_LASTLOGIN_I	396
  char	uaf$q_lastlogin_i[UAF$S_LASTLOGIN_I];
#define	UAF$S_LASTLOGIN_N	8
#define	UAF$Q_LASTLOGIN_N	404
  char	uaf$q_lastlogin_n[UAF$S_LASTLOGIN_N];
#define	UAF$S_PRIV	8
#define	UAF$Q_PRIV	412
  char	uaf$q_priv[UAF$S_PRIV];
#define	UAF$S_DEF_PRIV	8
#define	UAF$Q_DEF_PRIV	420
  char	uaf$q_def_priv[UAF$S_DEF_PRIV];
#define	UAF$S_MIN_CLASS	20
#define	UAF$R_MIN_CLASS	428
  char	uaf$r_min_class[UAF$S_MIN_CLASS];
#define	UAF$S_MAX_CLASS	20
#define	UAF$R_MAX_CLASS	448
  char	uaf$r_max_class[UAF$S_MAX_CLASS];
#define	UAF$L_FLAGS	468
#define	UAF$V_DISCTLY	0
#define	UAF$V_DEFCLI	1
#define	UAF$V_LOCKPWD	2
#define	UAF$V_CAPTIVE	3
#define	UAF$V_DISACNT	4
#define	UAF$V_DISWELCOM	5
#define	UAF$V_DISMAIL	6
#define	UAF$V_NOMAIL	7
#define	UAF$V_GENPWD	8
#define	UAF$V_PWD_EXPIRED	9
#define	UAF$V_PWD2_EXPIRED	10
#define	UAF$V_AUDIT	11
#define	UAF$V_DISREPORT	12
#define	UAF$V_DISRECONNECT	13
  union {
    unsigned long	uaf_l_flags;
#define uaf$l_flags uaf_u_flags.uaf_l_flags
    struct {
      unsigned long
	uaf_v_disctly : 1,
#define uaf$v_disctly uaf_u_flags.uaf_v_flags.uaf_v_disctly
      uaf_v_defcli : 1,
#define uaf$v_defcli uaf_u_flags.uaf_v_flags.uaf_v_discli
      uaf_v_lockpwd : 1,
#define uaf$v_lockpwd uaf_u_flags.uaf_v_flags.uaf_v_lockpwd
      uaf_v_captive : 1,
#define uaf$v_captive uaf_u_flags.uaf_v_flags.uaf_v_captive
      uaf_v_disacnt : 1,
#define uaf$v_disacnt uaf_u_flags.uaf_v_flags.uaf_v_disacnt
      uaf_v_diswelcom : 1,
#define uaf$v_diswelcom uaf_u_flags.uaf_v_flags.uaf_v_diswelcom
      uaf_v_dismail : 1,
#define uaf$v_dismail uaf_u_flags.uaf_v_flags.uaf_v_dismail
      uaf_v_nomail : 1,
#define uaf$v_nomail uaf_u_flags.uaf_v_flags.uaf_v_nomail
      uaf_v_genpwd : 1,
#define uaf$v_genpwd uaf_u_flags.uaf_v_flags.uaf_v_genpwd
      uaf_v_pwd_expired : 1,
#define uaf$v_pwd_expired uaf_u_flags.uaf_v_flags.uaf_v_pwd_expired
      uaf_v_pwd2_expired : 1,
#define uaf$v_pwd2_expired uaf_u_flags.uaf_v_flags.uaf_v_pwd2_expired
      uaf_v_audit : 1,
#define uaf$v_audit uaf_u_flags.uaf_v_flags.uaf_v_audit
      uaf_v_disreport : 1,
#define uaf$v_disreport uaf_u_flags.uaf_v_flags.uaf_v_disreport
      uaf_v_disreconnect : 1;
#define uaf$v_disreconnect uaf_u_flags.uaf_v_flags.uaf_v_disreconnect
    } uaf_v_flags;
  } uaf_u_flags;
#define	UAF$S_NETWORK_ACCESS_P	3
#define	UAF$B_NETWORK_ACCESS_P	472
  char	uaf$b_network_access_p[UAF$S_NETWORK_ACCESS_P];
#define	UAF$S_NETWORK_ACCESS_S	3
#define	UAF$B_NETWORK_ACCESS_S	475
  char	uaf$b_network_access_s[UAF$S_NETWORK_ACCESS_S];
#define	UAF$S_BATCH_ACCESS_P	3
#define	UAF$B_BATCH_ACCESS_P	478
  char	uaf$b_batch_access_p[UAF$S_BATCH_ACCESS_P];
#define	UAF$S_BATCH_ACCESS_S	3
#define	UAF$B_BATCH_ACCESS_S	481
  char	uaf$b_batch_access_s[UAF$S_BATCH_ACCESS_S];
#define	UAF$S_LOCAL_ACCESS_P	3
#define	UAF$B_LOCAL_ACCESS_P	484
  char	uaf$b_local_access_p[UAF$S_LOCAL_ACCESS_P];
#define	UAF$S_LOCAL_ACCESS_S	3
#define	UAF$B_LOCAL_ACCESS_S	487
  char	uaf$b_local_access_s[UAF$S_LOCAL_ACCESS_S];
#define	UAF$S_DIALUP_ACCESS_P	3
#define	UAF$B_DIALUP_ACCESS_P	490
  char	uaf$b_dialup_access_p[UAF$S_DIALUP_ACCESS_P];
#define	UAF$S_DIALUP_ACCESS_S	3
#define	UAF$B_DIALUP_ACCESS_S	493
  char	uaf$b_dialup_access_s[UAF$S_DIALUP_ACCESS_S];
#define	UAF$S_REMOTE_ACCESS_P	3
#define	UAF$B_REMOTE_ACCESS_P	496
  char	uaf$b_remote_access_p[UAF$S_REMOTE_ACCESS_P];
#define	UAF$S_REMOTE_ACCESS_S	3
#define	UAF$B_REMOTE_ACCESS_S	499
  char	uaf$b_remote_access_s[UAF$S_REMOTE_ACCESS_S];
#define	UAF$B_PRIMEDAYS	514
#define	UAF$V_MONDAY	0
#define	UAF$V_TUESDAY	1
#define	UAF$V_WEDNESDAY	2
#define	UAF$V_THURSDAY	3
#define	UAF$V_FRIDAY	4
#define	UAF$V_SATURDAY	5
#define	UAF$V_SUNDAY	6
  union {
    unsigned char	uaf_b_primedays;
#define uaf$b_primedays uaf_u_primedays.uaf_b_primedays
    unsigned char
      uaf_v_monday : 1,
#define uaf$v_monday uaf_u_primedays.uaf_v_monday
    uaf_v_tuesday : 1,
#define uaf$v_tuesday uaf_u_primedays.uaf_v_tuesday
    uaf_v_wednesday : 1,
#define uaf$v_wednesday uaf_u_primedays.uaf_v_wednesday
    uaf_v_thursday : 1,
#define uaf$v_thursday uaf_u_primedays.uaf_v_thrusday
    uaf_v_friday : 1,
#define uaf$v_friday uaf_u_primedays.uaf_v_friday
    uaf_v_saturday : 1,
#define uaf$v_saturday uaf_u_primedays.uaf_v_saturday
    uav_v_sunday : 1;
#define uaf$v_sunday uaf_u_primedays.uaf_v_sunday
  } uaf_u_primedays;
#define	UAF$B_PRI	516
  char	uaf$b_pri;
#define	UAF$B_QUEPRI	517
  char	uaf$b_quepri;
#define	UAF$W_MAXJOBS	518
  short	uaf$w_maxjobs;
#define	UAF$W_MAXACCTJOBS	520
  short	uaf$w_maxacctjobs;
#define	UAF$W_MAXDETACH	522
  short	uaf$w_maxdetach;
#define	UAF$W_PRCCNT	524
  short	uaf$w_prccnt;
#define	UAF$W_BIOLM	526
  short	uaf$w_biolm;
#define	UAF$W_DIOLM	528
  short	uaf$w_diolm;
#define	UAF$W_TQCNT	530
  short	uaf$w_twcnt;
#define	UAF$W_ASTLM	532
  short	uaf$w_astlm;
#define	UAF$W_ENQLM	534
  short	uaf$w_enqlm;
#define	UAF$W_FILLM	536
  short	uaf$w_fillm;
#define	UAF$W_SHRFILLM	538
  short	uaf$w_shrfillm;
#define	UAF$L_WSQUOTA	540
  long	uaf$l_wsquota;
#define	UAF$L_DFWSCNT	544
  long	uaf$l_dfwscnt;
#define	UAF$L_WSEXTENT	548
  long	uaf$l_wsextent;
#define	UAF$L_PGFLQUOTA	552
  long	uaf$l_pgflquota;
#define	UAF$L_CPUTIM	556
  long	uaf$l_cputim;
#define	UAF$L_BYTLM	560
  long	uaf$l_bytlm;
#define	UAF$L_PBYTLM	564
  long	uaf$l_pbytlm;
#define	UAF$L_JTQUOTA	568
  long	uaf$l_jtquota;
#define	UAF$W_PROXY_LIM	572
  short	uaf$w_proxy_lim;
#define	UAF$W_PROXIES	574
  short	uaf$w_proxies;
#define	UAF$W_ACCOUNT_LIM	576
  short	uaf$w_account_lim;
#define	UAF$W_ACCOUNTS	578
  short	uaf$w_accounts;
  char	uaf$b_fixed[UAF$C_FIXED - UAF$W_ACCOUNTS + 2];
  char	uaf$b_usrdata[UAF$C_LENGTH - UAF$C_FIXED];
};

#endif /* not UAF$K_LENGTH */
