/*-
 * Copyright (c) 1994
 *      The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mntopts.h	8.3 (Berkeley) %G%
 */

struct mntopt {
	const char *m_option;	/* option name */
	int m_inverse;		/* if a negative option, eg "dev" */
	int m_flag;		/* bit to set, eg. MNT_RDONLY */
};

/* User-visible MNT_ flags. */
#define MOPT_ASYNC		{ "async",	0, MNT_ASYNC }
#define MOPT_NODEV		{ "dev",	1, MNT_NODEV }
#define MOPT_NOEXEC		{ "exec",	1, MNT_NOEXEC }
#define MOPT_NOSUID		{ "suid",	1, MNT_NOSUID }
#define MOPT_RDONLY		{ "rdonly",	0, MNT_RDONLY }
#define MOPT_SYNC		{ "sync",	0, MNT_SYNCHRONOUS }
#define MOPT_UNION		{ "union",	0, MNT_UNION }

/* Control flags. */
#define MOPT_FORCE		{ "force",	1, MNT_FORCE }
#define MOPT_UPDATE		{ "update",	0, MNT_UPDATE }

/* Support for old-style "ro", "rw" flags. */
#define MOPT_RO			{ "ro",		0, MNT_RDONLY }
#define MOPT_RW			{ "rw",		1, MNT_RDONLY }

#define MOPT_FSTAB_COMPAT						\
	MOPT_RO,							\
	MOPT_RW

/* Standard options which all mounts can understand. */
#define MOPT_STDOPTS							\
	MOPT_FSTAB_COMPAT,						\
	MOPT_NODEV,							\
	MOPT_NOEXEC,							\
	MOPT_NOSUID,							\
	MOPT_RDONLY,							\
	MOPT_UNION

void getmntopts __P((const char *, const struct mntopt *, int *));
