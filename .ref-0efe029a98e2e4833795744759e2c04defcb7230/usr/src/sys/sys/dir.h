/*
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dir.h	7.2 (Berkeley) %G%
 */

/*
 * The information in this file should be obtained from <dirent.h>
 * and is provided solely (and temporarily) for backward compatibility.
 */

#ifndef _DIRENT_

#include <dirent.h>

/*
 * Backwards compatibility.
 */
#define direct dirent

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#undef DIRSIZ
#define DIRSIZ(dp) \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))

#endif /* _DIRENT_ */
