/*
 * Copyright (c) 1989, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Karels at Berkeley Software Design, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sysctl.h	7.13 (Berkeley) %G%
 */

/*
 * Definitions for sysctl call.
 * The sysctl call uses a hierarchical name for objects
 * that can be examined or modified.
 * The name is expressed as a sequence of integers.
 * Like a file path name, the meaning of each component
 * depends on its place in the hierarchy.
 * The top-level and kern identifiers are defined here,
 * and other identifiers are defined in the respective
 * subsystem header files.
 */

#define CTL_MAXNAME	12	/* largest number of components supported */

/*
 * Top-level identifiers
 */
#define	CTL_UNSPEC	0		/* unused */
#define	CTL_KERN	1		/* "high kernel": proc, limits */
#define	CTL_VM		2		/* virtual memory */
#define	CTL_FS		3		/* file system, mount type is next */
#define	CTL_NET		4		/* network, see socket.h */
#define	CTL_DEBUG	5		/* debugging parameters */
#define	CTL_HW		6		/* generic cpu/io */
#define	CTL_MACHDEP	7		/* machine dependent */
#define	CTL_MAXID	8		/* number of valid top-level ids */

#define CTL_NAMES { \
	"unspec", \
	"kern", \
	"vm", \
	"fs", \
	"net", \
	"debug", \
	"hw", \
	"machdep", \
}

/*
 * CTL_KERN identifiers
 */
#define	KERN_OSTYPE	 1		/* string: system version */
#define	KERN_OSRELEASE	 2		/* string: system release */
#define	KERN_OSREV	 3		/* int: system revision */
#define	KERN_VERSION	 4		/* string: compile time info */
#define	KERN_POSIX1	 5		/* int: POSIX.1 version */
#define	KERN_MAXPROC	 6		/* int: max simultaneous processes */
#define	KERN_MAXFILES	 7		/* int: max open files */
#define	KERN_ARGMAX	 8		/* int: max arguments to exec */
#define	KERN_HOSTNAME	 9		/* string: hostname */
#define	KERN_HOSTID	10		/* int: host identifier */
#define	KERN_CLOCKRATE	11		/* struct clockrate */
#define	KERN_VNODE	12		/* vnode structures */
#define	KERN_PROC	13		/* process entries */
#define	KERN_FILE	14		/* file entries */
#define	KERN_MAXID	15		/* number of valid kern ids */

#define CTL_KERN_NAMES { \
	"unspec", \
	"ostype", \
	"osrelease", \
	"osrevision", \
	"version", \
	"posix1version", \
	"maxproc", \
	"maxfiles", \
	"argmax", \
	"hostname", \
	"hostid", \
	"clockrate", \
	"vnode", \
	"proc", \
	"file", \
}

/* 
 * KERN_PROC subtypes
 */
#define KERN_PROC_ALL		0	/* everything */
#define	KERN_PROC_PID		1	/* by process id */
#define	KERN_PROC_PGRP		2	/* by process group id */
#define	KERN_PROC_SESSION	3	/* by session of pid */
#define	KERN_PROC_TTY		4	/* by controlling tty */
#define	KERN_PROC_UID		5	/* by effective uid */
#define	KERN_PROC_RUID		6	/* by real uid */

/*
 * CTL_HW identifiers
 */
#define	HW_MACHINE	 1		/* string: machine class */
#define	HW_MODEL	 2		/* string: specific machine model */
#define	HW_NCPU		 3		/* int: number of cpus */
#define	HW_CPUSPEED	 4		/* int: relative cpuspeed */
#define	HW_PHYSMEM	 5		/* int: total memory */
#define	HW_USERMEM	 6		/* int: non-kernel memory */
#define	HW_PAGESIZE	 7		/* int: software page size */
#define	HW_DISKNAMES	 8		/* strings: disk drive names */
#define	HW_DISKSTATS	 9		/* diskstats[] */
#define	HW_MAXID	10		/* number of valid hw ids */

#define CTL_HW_NAMES { \
	"unspec", \
	"machine", \
	"model", \
	"ncpu", \
	"cpuspeed", \
	"physmem", \
	"usermem", \
	"pagesize", \
	"disknames", \
	"diskstats", \
}

#ifdef KERNEL
/*
 * Internal sysctl function calling convention:
 *	(*sysctlfn)(name, namelen, oldval, oldlenp, newval, newlen);
 * The name parameter points at the next component of the name
 * to be interpreted.  The namelen parameter is the number of integers
 * in the name.
 */
typedef int (sysctlfn) __P((int *, u_int, void *, u_int *, void *, u_int));
int sysctl_int __P((void *, u_int *, void *, u_int, int *));
int sysctl_rdint __P((void *, u_int *, void *, int));
int sysctl_string __P((void *, u_int *, void *, u_int, char *, int));
int sysctl_rdstring __P((void *, u_int *, void *, char *));
int sysctl_rdstruct __P((void *, u_int *, void *, void *, int));

#else /* KERNEL */
#include <sys/cdefs.h>

__BEGIN_DECLS
int	sysctl __P((int *, int, void *, int *, void *, int));
__END_DECLS
#endif /* KERNEL */
