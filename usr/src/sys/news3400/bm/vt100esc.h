/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: vt100esc.h,v 4.300 91/06/09 06:15:01 root Rel41 $ SONY
 *
 *	@(#)vt100esc.h	7.1 (Berkeley) %G%
 */

/*
 *  escape sequence structure
 */
struct  esc_sequence {
	char	command;
	char	*terminators;
	int	(*esc_func)();
};

/*
 *  key pad structure
 */

struct	key_pad {
	char	kpd_numeric;
	char	kpd_applic;
};

#define	NUMERIC	0
#define	APPLIC	1
