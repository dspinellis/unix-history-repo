/*-
 * Copyright (c) 1982, 1988, 1991, 1993
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
 *	@(#)saio.h	8.1 (Berkeley) 6/11/93
 */

#include <sys/time.h>

#include <ufs/ffs/fs.h>
#include <ufs/ufs/dinode.h>

#include <stand.att/saioctl.h>
#include <stand.att/saerrno.h>

#define	UNIX	"/vmunix"

#ifndef NULL
#define	NULL	0
#endif

/* I/O block */
struct iob {
	int	i_flgs;		/* flags (see F_*) */
	int	i_adapt;	/* adapter or bus */
	int	i_ctlr;		/* controller */
	int	i_unit;		/* pseudo device unit */
	int	i_part;		/* disk partition */
	daddr_t	i_boff;		/* block offset on device */
	daddr_t	i_cyloff;	/* cylinder offset on device */
	off_t	i_offset;	/* seek offset in file */
	dev_t	i_dev;		/* associated device */
	daddr_t	i_bn;		/* 1st block # of next read */
	char	*i_ma;		/* memory address of I/O buffer */
	int	i_cc;		/* character count of transfer */
	int	i_error;	/* error # return */
	int	i_errcnt;	/* error count for driver retries */
	int	i_errblk;	/* block # in error for error reporting */
	char	i_buf[MAXBSIZE];/* I/O buffer */
	struct	dinode i_ino;	/* dinode, if file */
	union {			/* file system super block info */
		struct fs ui_fs;
		char dummy[SBSIZE];
	} i_un;
#define	i_fs	i_un.ui_fs
#define	i_bus	i_adapt
};

#define	SOPEN_MAX	4
extern struct iob iob[SOPEN_MAX];

/* Codes for sector header word 1 */
#define	HDR1_FMT22	0x1000	/* standard 16 bit format */
#define	HDR1_OKSCT	0xc000	/* sector ok */
#define	HDR1_SSF	0x2000	/* skip sector flag */

/* I/O flag values */
#define	F_READ		0x0001	/* file opened for reading */
#define	F_WRITE		0x0002	/* file opened for writing */
#define	F_ALLOC		0x0004	/* buffer allocated */
#define	F_FILE		0x0008	/* file instead of device */
#define	F_NBSF		0x0010	/* no bad sector forwarding */
#define	F_ECCLM		0x0020	/* limit # of bits in ecc correction */
#define	F_SSI		0x0040	/* set skip sector inhibit */
#define	F_SEVRE		0x0080	/* Severe burnin (no retries, no ECC) */

/* I/O types */
#define	F_RDDATA	0x0100	/* read data */
#define	F_WRDATA	0x0200	/* write data */
#define	F_HDR		0x0400	/* include header on next i/o */
#define	F_CHECK		0x0800	/* perform check of data read/write */
#define	F_HCHECK	0x1000	/* perform check of header and data */

#define	F_TYPEMASK	0xff00	/* I/O type mask */

/* Lseek values */
#define	L_SET		0	/* absolute offset */

/* Device switch */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy)();
	int	(*dv_open)();
	int	(*dv_close)();
	int	(*dv_ioctl)();
};

extern struct devsw devsw[];	/* device array */
extern int ndevs;		/* number of elements in devsw[] */

#ifdef COMPAT_42
/*
 * Old drive description table.
 * still used by some drivers for now.
 */
struct st {
	short	nsect;		/* # sectors/track */
	short	ntrak;		/* # tracks/surfaces/heads */
	short	nspc;		/* # sectors/cylinder */
	short	ncyl;		/* # cylinders */
	short	*off;		/* partition offset table (cylinders) */
};
#endif
