/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)pathnames.h	5.2 (Berkeley) 6/1/90
 */

#include <paths.h>

#define	IG_FILE1	"llib-lc"
#define	IG_FILE2	"llib-port"
#define	IG_FILE3	"/usr/lib/llib-lc"
#define	IG_FILE4	"/usr/lib/llib-port"
#undef _PATH_TMP
#define	_PATH_TMP	"/tmp/ErrorXXXXXX"
