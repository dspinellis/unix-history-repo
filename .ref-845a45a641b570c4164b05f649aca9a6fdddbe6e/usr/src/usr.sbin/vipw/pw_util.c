/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pw_util.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * This file is used by all the "password" programs; vipw(8), chpass(1),
 * and passwd(1).
 */

#include <sys/param.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <fcntl.h>
#include <pwd.h>
#include <errno.h>
#include <stdio.h>
#include <paths.h>
#include <string.h>
#include <stdlib.h>

extern char *progname;
extern char *tempname;

pw_init()
{
	struct rlimit rlim;

	/* Unlimited resource limits. */
	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void)setrlimit(RLIMIT_CPU, &rlim);
	(void)setrlimit(RLIMIT_FSIZE, &rlim);
	(void)setrlimit(RLIMIT_STACK, &rlim);
	(void)setrlimit(RLIMIT_DATA, &rlim);
	(void)setrlimit(RLIMIT_RSS, &rlim);

	/* Don't drop core (not really necessary, but GP's). */
	rlim.rlim_cur = rlim.rlim_max = 0;
	(void)setrlimit(RLIMIT_CORE, &rlim);

	/* Turn off signals. */
	(void)signal(SIGALRM, SIG_IGN);
	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGPIPE, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	(void)signal(SIGTERM, SIG_IGN);
	(void)signal(SIGTSTP, SIG_IGN);
	(void)signal(SIGTTOU, SIG_IGN);

	/* Create with exact permissions. */
	(void)umask(0);
}

static int lockfd;
pw_lock()
{
	/* 
	 * If the master password file doesn't exist, the system is hosed.
	 * Might as well try to build one.
	 * Open should allow flock'ing the file; see 4.4BSD.	XXX
	 */
	lockfd = open(_PATH_MASTERPASSWD, O_RDONLY, 0);
	if (lockfd < 0) {
		(void)fprintf(stderr, "%s: %s: %s\n",
		    progname, _PATH_MASTERPASSWD, strerror(errno));
		exit(1);
	}
	if (flock(lockfd, LOCK_EX|LOCK_NB)) {
		(void)fprintf(stderr,
		    "%s: the password db is busy.\n", progname);
		exit(1);
	}
	return(lockfd);
}

pw_tmp()
{
	static char path[MAXPATHLEN] = _PATH_MASTERPASSWD;
	int fd;
	char *p;

	if (p = rindex(path, '/'))
		++p;
	else
		p = path;
	(void)sprintf(p, "%s.XXXXXX", progname);
	if ((fd = mkstemp(path)) == -1) {
		(void)fprintf(stderr,
		    "%s: %s: %s\n", progname, path, strerror(errno));
		exit(1);
	}
	tempname = path;
	return(fd);
}

pw_mkdb()
{
	union wait pstat;
	pid_t pid;

	(void)printf("%s: rebuilding the database...\n", progname);
	(void)fflush(stdout);
	if (!(pid = vfork())) {
		execl(_PATH_PWD_MKDB, "pwd_mkdb", "-p", tempname, NULL);
		pw_error(_PATH_PWD_MKDB, 1, 1);
	}
	pid = waitpid(pid, (int *)&pstat, 0);
	if (pid == -1 || pstat.w_status)
		return(0);
	(void)printf("%s: done\n", progname);
	return(1);
}

pw_edit(notsetuid)
	int notsetuid;
{
	union wait pstat;
	pid_t pid;
	char *p, *editor;

	if (!(editor = getenv("EDITOR")))
		editor = _PATH_VI;
	if (p = rindex(editor, '/'))
		++p;
	else 
		p = editor;

	if (!(pid = vfork())) {
		if (notsetuid) {
			(void)setgid(getgid());
			(void)setuid(getuid());
		}
		execlp(editor, p, tempname, NULL);
		_exit(1);
	}
	pid = waitpid(pid, (int *)&pstat, 0);
	if (pid == -1 || pstat.w_status)
		pw_error(editor, 1, 1);
}

pw_prompt()
{
	register int c;

	for (;;) {
		(void)printf("re-edit the password file? [y]: ");
		(void)fflush(stdout);
		c = getchar();
		if (c != EOF && c != (int)'\n')
			while (getchar() != (int)'\n');
		if (c == (int)'n')
			pw_error((char *)NULL, 0, 0);
		break;
	}
}

pw_error(name, err, eval)
	char *name;
	int err, eval;
{
	int sverrno;

	if (err) {
		sverrno = errno;
		(void)fprintf(stderr, "%s: ", progname);
		if (name)
			(void)fprintf(stderr, "%s: ", name);
		(void)fprintf(stderr, "%s\n", strerror(sverrno));
	}
	(void)fprintf(stderr,
	    "%s: %s unchanged\n", progname, _PATH_MASTERPASSWD);
	(void)unlink(tempname);
	exit(eval);
}
