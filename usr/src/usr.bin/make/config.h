/*-
 * config.h --
 *	Configuration constants for the local site.
 *
 * Copyright (c) 1988 by the Regents of the University of California
 * Copyright (c) 1988 by Adam de Boor
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  Neither the University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 *	"$Id: config.h,v 1.4 89/11/14 17:54:56 adam Exp $ SPRITE (Berkeley)"
 */

/*
 * DEFSYSPATH
 *	Is the absolute path of the location of system makefiles.
 * SYSPATHDOC
 *	Documentation string to tell what the directory is.
 */
#define DEFSYSPATH	"/usr/public/lib/pmake"
#define SYSPATHDOC	"\tDirectory for system makefiles: /usr/public/lib/pmake"

/*
 * DEFSHELL
 *	The index of the default shell description in job.c. It indexes the
 *	'shells' table:
 *	    0	  	C Shell
 *	    1	  	Bourne Shell
 *	    2	  	Unknown
 * SHELLDOC
 *	Documentation string to tell what shell is used by default.
 */
#define DEFSHELL  	1
#define SHELLDOC  	"\tDefault shell: Bourne"

/*
 * DEFSHELLDIR
 *	The directory in which the C Shell and Bourne Shell are to be found.
 */
#define DEFSHELLDIR	"/bin"

/*
 * DEFSYSMK
 *	The absolute path of the default system makefile. It should probably
 *	be in DEFSYSPATH.
 * SYSMKDOC
 *	Documentation string to tell where the system makefile is.
 */
#define DEFSYSMK  	"/usr/public/lib/pmake/system.mk"
#define SYSMKDOC  	"\tSystem makefile: /usr/public/lib/pmake/system.mk"

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
