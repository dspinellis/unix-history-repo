/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
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
 *	@(#)config.h	5.3 (Berkeley) %G%
 */

#define	DEFSHELL	1			/* Bourne shell */
#define	DEFSHELLDIR	"/bin"			/* Bourne shell directory */
#define	DEFSYSMK  	"/usr/share/mk/sys.mk"	/* system makefile */
#define	DEFSYSPATH	"/usr/share/mk"		/* system makefile directory */

/*
 * DEFMAXJOBS
 * DEFMAXLOCAL
 *	These control the default concurrency. On no occasion will more
 *	than DEFMAXJOBS targets be created at once (locally or remotely)
 *	DEFMAXLOCAL is the highest number of targets which will be
 *	created on the local machine at once. Note that if you set this
 *	to 0, nothing will ever happen...
 */
#define DEFMAXJOBS	4
#define DEFMAXLOCAL	1

/*
 * INCLUDES
 * LIBRARIES
 *	These control the handling of the .INCLUDES and .LIBS variables.
 *	If INCLUDES is defined, the .INCLUDES variable will be filled
 *	from the search paths of those suffixes which are marked by
 *	.INCLUDES dependency lines. Similarly for LIBRARIES and .LIBS
 *	See suff.c for more details.
 */
#define INCLUDES
#define LIBRARIES

/*
 * LOCKFILE
 *	This is the name of the lock file which is created in the current
 *	directory if the -l flag isn't given.
 * DONT_LOCK
 *	If this is defined, directory locking will be off by default. The
 *	-l flag will then turn locking on.
 */
#define LOCKFILE  	"LOCK.make"
/*#define DONT_LOCK*/

/*
 * SPECIAL_CHAR
 *	The character that leads into conditionals and include directives
 *	and the like.
 */
#define SPECIAL_CHAR '#'

/*
 * DEF_OLD_VARS
 *	If defined, variable substitution follows the make style. PMake-style
 *	substitution cannot be turned on. In addition, pmake will never
 *	generate a warning for an undefined variable.
 */
/*#define DEF_OLD_VARS*/

/*
 * NEED_FD_SET
 *	Define this if your system doesn't define the fd_set type for select
 *	masks in <sys/types.h>
 *
 * FD_SETSIZE
 *	Define this to be the maximum number of files a process can have
 *	open at once. It defaults to 256.
 *
 * NO_WAIT3
 *	Define this if your system doesn't support the non-blocking wait3
 *	system call of BSD UNIX. This is not implemented yet.
 *
 * NO_VFORK
 *	Define this if your system doesn't support (or you shouldn't use)
 *	the vfork system call found in BSD UNIX.
 *
 * LIBTOC
 *	This is the name by which the table of contents in a ranlib'ed
 *	library is known. Some systems have changed it from __.SYMDEF,
 *	for whatever reason.
 *
 * LIBSUFF
 *	Is the suffix used to denote libraries and is used by the Suff module
 *	to find the search path on which to seek any -l<xx> targets.
 *
 * RECHECK
 *	If defined, Make_Update will check a target for its current
 *	modification time after it has been re-made, setting it to the
 *	starting time of the make only if the target still doesn't exist.
 *	Unfortunately, under NFS the modification time often doesn't
 *	get updated in time, so a target will appear to not have been
 *	re-made, causing later targets to appear up-to-date. On systems
 *	that don't have this problem, you should defined this. Under
 *	NFS you probably should not, unless you aren't exporting jobs.
 *
 * POSIX
 *	If the POSIX standard for Make is to be followed. There are
 *	several areas that I dislike, hence this constant.
 */
/*#define NEED_FD_SET*/
/*#define FD_SETSIZE 32*/
/*#define NO_WAIT3*/
/*#define NO_VFORK*/
#define LIBTOC	"__.SYMDEF"
#define LIBSUFF	".a"
#define RECHECK
/*#define POSIX*/
