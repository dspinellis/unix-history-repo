/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)unistd.h	5.12 (Berkeley) %G%
 */

#ifndef _UNISTD_H_
#define	_UNISTD_H_

/* compile-time symbolic constants */
#define	_POSIX_JOB_CONTROL	/* implementation supports job control */
/*#define	_POSIX_SAVED_IDS	/* saved set-user-ID and set-group-ID */
#define	_POSIX_VERSION		198808L

/* execution-time symbolic constants */
#define	_POSIX_CHOWN_RESTRICTED	/* chown requires appropriate privileges */
#define	_POSIX_NO_TRUNC		/* too-long path components generate errors */
				/* may disable terminal special characters */
#define	_POSIX_VDISABLE	((unsigned char)'\377')

/* access function */
#define	F_OK		0	/* test for existence of file */
#define	X_OK		0x01	/* test for execute or search permission */
#define	W_OK		0x02	/* test for write permission */
#define	R_OK		0x04	/* test for read permission */

/* whence values for lseek(2) */
#define	SEEK_SET	0	/* set file offset to offset */
#define	SEEK_CUR	1	/* set file offset to current plus offset */
#define	SEEK_END	2	/* set file offset to EOF plus offset */

#ifndef _POSIX_SOURCE
/* whence values for lseek(2); renamed by POSIX 1003.1 */
#define	L_SET		SEEK_SET
#define	L_INCR		SEEK_CUR
#define	L_XTND		SEEK_END
#endif

/* map a stream pointer to a file descriptor */
#define	STDIN_FILENO	0	/* standard input value, stdin */
#define	STDOUT_FILENO	1	/* standard output value, stdout */
#define	STDERR_FILENO	2	/* standard error value, stdout */

/* fnmatch function */
#define	FNM_PATHNAME	0x01	/* match pathnames, not filenames */
#ifndef _POSIX_SOURCE
#define	FNM_QUOTE	0x02	/* escape special chars with \ */
#endif

#ifndef NULL
#define	NULL		0	/* null pointer constant */
#endif

/* configurable pathname variables */
#define	_PC_LINK_MAX		1
#define	_PC_MAX_CANON		2
#define	_PC_MAX_INPUT		3
#define	_PC_NAME_MAX		4
#define	_PC_PATH_MAX		5
#define	_PC_PIPE_BUF		6
#define	_PC_CHOWN_RESTRICTED	7
#define	_PC_NO_TRUNC		8
#define	_PC_VDISABLE		9

/* configurable system variables */
#define	_SC_ARG_MAX		1
#define	_SC_CHILD_MAX		2
#define	_SC_CLK_TCK		3
#define	_SC_NGROUPS_MAX		4
#define	_SC_OPEN_MAX		5
#define	_SC_JOB_CONTROL		6
#define	_SC_SAVED_IDS		7
#define	_SC_VERSION		8

/* POSIX.1 2.5 specifically requires that unistd.h define size_t */
#include <sys/types.h>

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

/* ugly, but the only reasonable value for the time being */
typedef	int		ssize_t;		/* what read() returns */

#ifndef KERNEL
#include <sys/cdefs.h>

__BEGIN_DECLS
void volatile	_exit __P((int));
int		access __P((const char *, int));
unsigned int	alarm __P((unsigned int));
int		chdir __P((const char *));
int		chown __P((const char *, uid_t, gid_t));
int		close __P((int));
char		*cuserid __P((const char *));
int		dup __P((int));
int		dup2 __P((int, int));
int		execl __P((const char *, const char *, ...));
int		execle __P((const char *, const char *, ...));
int		execlp __P((const char *, const char *, ...));
int		execv __P((const char *, char * const *));
int		execve __P((const char *, char * const *, char * const *));
int		execvp __P((const char *, char * const *));
pid_t		fork __P((void));
long		fpathconf __P((int, int));	/* not yet implemented */
char		*getcwd __P((char *, size_t));
gid_t		getegid __P((void));
uid_t		geteuid __P((void));
gid_t		getgid __P((void));
int		getgroups __P((int, gid_t *));
char		*getlogin __P((void));
pid_t		getpgrp __P((void));
pid_t		getpid __P((void));
pid_t		getppid __P((void));
uid_t		getuid __P((void));
int		isatty __P((int));
int		link __P((const char *, const char *));
off_t		lseek __P((int, off_t, int));
long		pathconf __P((const char *, int));	/* not yet */
int		pause __P((void));
int		pipe __P((int *));
ssize_t		read __P((int, void *, size_t));
int		rmdir __P((const char *));
int		setgid __P((gid_t));
int		setpgid __P((pid_t, pid_t));
pid_t		setsid __P((void));
int		setuid __P((uid_t));
unsigned int	sleep __P((unsigned int));
long		sysconf __P((int));			/* not yet */
pid_t		tcgetpgrp __P((int));
pid_t		tcsetpgrp __P((int, pid_t));
char		*ttyname __P((int));
int		unlink __P((const char *));
ssize_t		write __P((int, const void *, size_t));

#ifndef	_POSIX_SOURCE
/* a number of BSD-specific declarations will go here */
#endif

#endif /* KERNEL */

__END_DECLS

#endif /* !_UNISTD_H_ */
