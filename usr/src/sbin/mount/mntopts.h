/*-
 * Copyright (c) 1994
 *      The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mntopts.h	8.4 (Berkeley) %G%
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
#define MOPT_RO			{ "ro",		0, MNT_RDONLY }
#define MOPT_RW			{ "rw",		1, MNT_RDONLY }

/* This is parsed by mount(8), but is ignored by specific mount_*(8)s. */
#define MOPT_AUTO		{ "auto",	0, 0 }

#define MOPT_FSTAB_COMPAT						\
	MOPT_RO,							\
	MOPT_RW,							\
	MOPT_AUTO

/* Standard options which all mounts can understand. */
#define MOPT_STDOPTS							\
	MOPT_FSTAB_COMPAT,						\
	MOPT_NODEV,							\
	MOPT_NOEXEC,							\
	MOPT_NOSUID,							\
	MOPT_RDONLY,							\
	MOPT_UNION

void getmntopts __P((const char *, const struct mntopt *, int *));
