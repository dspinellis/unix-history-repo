/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)unistd.h	5.23 (Berkeley) %G%
 */

#ifndef _UNISTD_H_
#define	_UNISTD_H_

#include <sys/cdefs.h>
#include <sys/types.h>
#include <sys/unistd.h>

#define	 STDIN_FILENO	0	/* standard input file descriptor */
#define	STDOUT_FILENO	1	/* standard output file descriptor */
#define	STDERR_FILENO	2	/* standard error file descriptor */

#ifndef NULL
#define	NULL		0	/* null pointer constant */
#endif

__BEGIN_DECLS
__dead void
	 _exit __P((int));
int	 access __P((const char *, int));
u_int	 alarm __P((u_int));
int	 chdir __P((const char *));
int	 chown __P((const char *, uid_t, gid_t));
int	 close __P((int));
int	 dup __P((int));
int	 dup2 __P((int, int));
int	 execl __P((const char *, const char *, ...));
int	 execle __P((const char *, const char *, ...));
int	 execlp __P((const char *, const char *, ...));
int	 execv __P((const char *, char * const *));
int	 execve __P((const char *, char * const *, char * const *));
int	 execvp __P((const char *, char * const *));
pid_t	 fork __P((void));
long	 fpathconf __P((int, int));		/* not yet */
char	*getcwd __P((char *, size_t));
gid_t	 getegid __P((void));
uid_t	 geteuid __P((void));
gid_t	 getgid __P((void));
int	 getgroups __P((int, int *));		/* XXX (gid_t *) */
char	*getlogin __P((void));
pid_t	 getpgrp __P((void));
pid_t	 getpid __P((void));
pid_t	 getppid __P((void));
uid_t	 getuid __P((void));
int	 isatty __P((int));
int	 link __P((const char *, const char *));
#define	 lseek __lseek
off_t	 lseek __P((int, off_t, int));
long	 pathconf __P((const char *, int));	/* not yet */
int	 pause __P((void));
int	 pipe __P((int *));
ssize_t	 read __P((int, void *, size_t));
int	 rmdir __P((const char *));
int	 setgid __P((gid_t));
int	 setpgid __P((pid_t, pid_t));
pid_t	 setsid __P((void));
int	 setuid __P((uid_t));
u_int	 sleep __P((u_int));
long	 sysconf __P((int));			/* not yet */
pid_t	 tcgetpgrp __P((int));
int	 tcsetpgrp __P((int, pid_t));
char	*ttyname __P((int));
int	 unlink __P((const char *));
ssize_t	 write __P((int, const void *, size_t));

#ifndef	_POSIX_SOURCE

/* structure timeval required for select() */
#include <sys/time.h>

int	 acct __P((const char *));
int	 async_daemon __P((void));
char	*brk __P((const char *));
int	 chflags __P((const char *, long));
int	 chroot __P((const char *));
char	*crypt __P((const char *, const char *));
int	 des_cipher __P((const char *, char *, long, int));
int	 des_setkey __P((const char *key));
int	 encrypt __P((char *, int));
void	 endusershell __P((void));
int	 exect __P((const char *, char * const *, char * const *));
int	 fchdir __P((int));
int	 fchflags __P((int, long));
int	 fchown __P((int, int, int));
int	 fsync __P((int));
#define	 ftruncate __ftruncate
int	 ftruncate __P((int, off_t));
int	 getdtablesize __P((void));
long	 gethostid __P((void));
int	 gethostname __P((char *, int));
mode_t	 getmode __P((const void *, mode_t));
__pure int
	 getpagesize __P((void));
char	*getpass __P((const char *));
char	*getusershell __P((void));
char	*getwd __P((char *));			/* obsoleted by getcwd() */
int	 initgroups __P((const char *, int));
int	 iruserok __P((u_long, int, const char *, const char *));
int	 mknod __P((const char *, mode_t, dev_t));
int	 mkstemp __P((char *));
char	*mktemp __P((char *));
int	 nfssvc __P((int, caddr_t));
int	 nice __P((int));
void	 psignal __P((u_int, const char *));
extern char *sys_siglist[];
int	 profil __P((char *, int, int, int));
int	 rcmd __P((char **, int, const char *,
		const char *, const char *, int *));
char	*re_comp __P((const char *));
int	 re_exec __P((const char *));
int	 readlink __P((const char *, char *, int));
int	 reboot __P((int));
int	 revoke __P((const char *));
int	 rresvport __P((int *));
int	 ruserok __P((const char *, int, const char *, const char *));
char	*sbrk __P((int));
int	 select __P((int, fd_set *, fd_set *, fd_set *, struct timeval *));
int	 setegid __P((gid_t));
int	 seteuid __P((uid_t));
int	 setgroups __P((int, const int *));
void	 sethostid __P((long));
int	 sethostname __P((const char *, int));
int	 setkey __P((const char *));
int	 setlogin __P((const char *));
void	*setmode __P((const char *));
int	 setpgrp __P((pid_t pid, pid_t pgrp));	/* obsoleted by setpgid() */
int	 setregid __P((int, int));
int	 setreuid __P((int, int));
int	 setrgid __P((gid_t));
int	 setruid __P((uid_t));
void	 setusershell __P((void));
int	 swapon __P((const char *));
int	 symlink __P((const char *, const char *));
void	 sync __P((void));
int	 syscall __P((int, ...));
#define	 truncate __truncate
int	 truncate __P((const char *, off_t));
int	 ttyslot __P((void));
u_int	 ualarm __P((u_int, u_int));
void	 usleep __P((u_int));
void	*valloc __P((size_t));			/* obsoleted by malloc() */
int	 vfork __P((void));

#endif /* !_POSIX_SOURCE */
__END_DECLS

#endif /* !_UNISTD_H_ */
