/*-
 * Copyright (c) 1982, 1986, 1990, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ioctl.h	8.6 (Berkeley) %G%
 */

#ifndef	_SYS_IOCTL_H_
#define	_SYS_IOCTL_H_

#include <sys/ttycom.h>

/*
 * Pun for SunOS prior to 3.2.  SunOS 3.2 and later support TIOCGWINSZ
 * and TIOCSWINSZ (yes, even 3.2-3.5, the fact that it wasn't documented
 * nonwithstanding).
 */
struct ttysize {
	unsigned short	ts_lines;
	unsigned short	ts_cols;
	unsigned short	ts_xxx;
	unsigned short	ts_yyy;
};
#define	TIOCGSIZE	TIOCGWINSZ
#define	TIOCSSIZE	TIOCSWINSZ

#include <sys/ioccom.h>

#define	IOC_DIRMASK	(IOC_IN|IOC_OUT|IOC_VOID)

#ifndef KERNEL

#include <sys/cdefs.h>

__BEGIN_DECLS
int	ioctl __P((int, unsigned long, ...));
__END_DECLS
#endif /* !KERNEL */
#endif /* !_SYS_IOCTL_H_ */

/*
 * Keep outside _SYS_IOCTL_H_
 * Compatability with old terminal driver
 *
 * Source level -> #define USE_OLD_TTY
 * Kernel level -> options COMPAT_43 or COMPAT_SUNOS
 */
#if defined(USE_OLD_TTY) || defined(COMPAT_43) || defined(COMPAT_SUNOS)
#include <sys/ioctl_compat.h>
#endif
