/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: hpux.h 1.15 89/09/25$
 *
 *	@(#)hpux.h	7.1 (Berkeley) 5/8/90
 */

#include "hpux_exec.h"

/* HP-UX style UTSNAME struct used by uname syscall */

struct hpuxutsname {
	char	sysname[9];
	char	nodename[9];
	char	release[9];
	char	version[9];
	char	machine[9];
	char	idnumber[15];
};

/* HP-UX style "old" IOCTLs */

struct hpuxsgttyb {
	char	sg_ispeed;
	char	sg_ospeed;
	char	sg_erase;
	char	sg_kill;
	int	sg_flags;	/* only a short in BSD */
};

#define V7_HUPCL	00001
#define V7_XTABS	00002
#define V7_NOAL		04000

#define	HPUXTIOCGETP	_IOR('t', 8, struct hpuxsgttyb)
#define	HPUXTIOCSETP	_IOW('t', 9, struct hpuxsgttyb)

/* 6.5 job control related ioctls which need to be mapped */

#define	HPUXTIOCSLTC	_IOW('T', 23, struct ltchars)
#define	HPUXTIOCGLTC	_IOR('T', 24, struct ltchars)
#define	HPUXTIOCLBIS	_IOW('T', 25, int)
#define	HPUXTIOCLBIC	_IOW('T', 26, int)
#define	HPUXTIOCLSET	_IOW('T', 27, int)
#define	HPUXTIOCLGET	_IOR('T', 28, int)
#	define HPUXLTOSTOP	0000001
#define	HPUXTIOCSPGRP	_IOW('T', 29, int)
#define	HPUXTIOCGPGRP	_IOR('T', 30, int)
#define HPUXTIOCCONS	_IO('t', 104)

/* HP-UX directory stuff */

#define HPUXNSIZ	14
#define HPUXPSIZ	10
#define HPUXDSIZ	sizeof(struct hpuxdirect)

struct hpuxdirect {
	u_long	hpuxd_ino;
	u_short hpuxd_reclen;
	u_short	hpuxd_namlen;
	char	hpuxd_name[HPUXNSIZ];
	char	hpuxd_pad[HPUXPSIZ];
};

/* HP-UX stat structure */

#define bsdtohpuxdev(d)	((major(d) << 24) | minor(d))

struct	hpuxstat {
	long	hst_dev;
	u_long	hst_ino;
	u_short	hst_mode;
	short	hst_nlink;
	u_short	hst_uid;
	u_short	hst_gid;
	long	hst_rdev;
	off_t	hst_size;
	time_t	hst_atime;
	int	hst_spare1;
	time_t	hst_mtime;
	int	hst_spare2;
	time_t	hst_ctime;
	int	hst_spare3;
	long	hst_blksize;
	long	hst_blocks;
	u_int	hst_remote;
	long	hst_netdev;  	
	u_long	hst_netino;
	long	hst_spare4[9];
};

/*
 * Skeletal 6.X HP-UX user structure info for ptrace() mapping.
 * Yes, this is as bogus as it gets...
 */

/* 6.0/6.2 offsets */
#define oHU_AROFF	0x004
#define oHU_TSOFF	0x092
#define oHU_EDOFF	0x91E
#define oHU_FPOFF	0xA66

/* 6.5 offsets */
#define HU_AROFF	0x004
#define HU_TSOFF	0x0B2
#define HU_EDOFF	0x93A
#define HU_FPOFF	0xA86

#define HU_PAD1	(HU_AROFF)
#define HU_PAD2	(HU_TSOFF-HU_AROFF-4)
#define HU_PAD3	(HU_EDOFF-HU_TSOFF-12)
#define HU_PAD4	(HU_FPOFF-HU_EDOFF-sizeof(struct hpux_exec))

struct hpuxuser {
	u_char	whocares1[HU_PAD1];	/* +0x000 */
	int	*hpuxu_ar0;		/* +0x004 */
	u_char	whocares2[HU_PAD2];	/* +0x008 */
	int	hpuxu_tsize;		/* +0x0B2 */
	int	hpuxu_dsize;		/* +0x0B6 */
	int	hpuxu_ssize;		/* +0x0BA */
	u_char	whocares3[HU_PAD3];	/* +0x0BE */
	struct	hpux_exec hpuxu_exdata;	/* +0x93A */
	u_char	whocares4[HU_PAD4];	/* +0x95E */
	struct	hpuxfp {		/* +0xA66 */
		int hpfp_save[54];
		int hpfp_ctrl[3];
		int hpfp_reg[24];
	} hpuxu_fp;
	short	hpuxu_dragon;		/* +0xBCA */
};

/* HP-UX compat file flags */
#define HPUXFCREAT	00000400
#define	HPUXFTRUNC	00001000
#define	HPUXFEXCL	00002000
#define HPUXFSYNCIO	00100000
#define HPUXFREMOTE	01000000
#define FUSECACHE	04000000

/* HP-UX only sysV shmctl() commands */
#define SHM_LOCK	3	/* Lock segment in core */
#define SHM_UNLOCK	4	/* Unlock segment */

/* HP-UX rtprio values */
#define RTPRIO_MIN	0
#define RTPRIO_MAX	127
#define RTPRIO_NOCHG	1000
#define RTPRIO_RTOFF	1001

/* HP-UX only sigvec sv_flags values */
#define HPUXSV_RESET	000000004

/*
 * HP-UX returns SIGILL instead of SIGFPE for the CHK and TRAPV exceptions.
 * It also returns different u_code values for certain illegal instruction
 * and floating point exceptions.  Here are the proper HP-UX u_code values
 * (numbers from hpux 6.2 manual pages).
 */

/* SIGILL codes */
#define	HPUX_ILL_ILLINST_TRAP	0	/* T_ILLINST+USER */
#define	HPUX_ILL_CHK_TRAP	6	/* T_CHKINST+USER */
#define	HPUX_ILL_TRAPV_TRAP	7	/* T_TRAPVINST+USER */
#define	HPUX_ILL_PRIV_TRAP	8	/* T_PRIVINST+USER */

/* SIGFPE codes */
#define	HPUX_FPE_INTDIV_TRAP	5	/* T_ZERODIV+USER */
