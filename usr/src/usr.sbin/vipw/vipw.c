/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vipw.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
#include <pwd.h>
#include <stdio.h>

main()
{
	extern int errno;
	register int n, fd_passwd, fd;
	struct rlimit rlim;
	struct stat s1, s2;
	char *fend, *passwd, *temp, *tend;
	char buf[8*1024], from[MAXPATHLEN], to[MAXPATHLEN];
	char *strerror(), *strcpy();

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);
	(void)signal(SIGTSTP, SIG_IGN);

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void)setrlimit(RLIMIT_CPU, &rlim);
	(void)setrlimit(RLIMIT_FSIZE, &rlim);

	(void)umask(0);

	temp = _PATH_PTMP;
	if ((fd = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0600)) < 0) {
		if (errno == EEXIST)
			fprintf(stderr, "vipw: password file busy.\n");
		else
			fprintf(stderr,
			    "vipw: %s: %s\n", temp, strerror(errno));
		exit(1);
	}
	passwd = _PATH_MASTERPASSWD;
	if ((fd_passwd = open(passwd, O_RDONLY, 0)) < 0) {
		fprintf(stderr, "vipw: %s: %s\n", passwd, strerror(errno));
		exit(1);
	}
	while ((n = read(fd_passwd, buf, sizeof(buf))) > 0)
		if (write(fd, buf, n) != n)
			goto syserr;

	if (n == -1 || close(fd_passwd) || fsync(fd) ||
	    fstat(fd, &s1) || close(fd)) {
syserr:		fprintf(stderr, "vipw: %s", strerror(errno));
		goto bad;
	}

	if (edit(temp)) {
		fprintf(stderr, "vipw: edit failed");
		goto bad;
	}

	if (!freopen(temp, "r", stdin) || fstat(fileno(stdin), &s2) ||
	    !s2.st_size) {
		fprintf(stderr, "vipw: can't read temp file");
		goto bad;
	}

	if (s1.st_mtime == s2.st_mtime) {
		fprintf(stderr, "vipw: %s unchanged.\n", passwd);
		(void)unlink(temp);
		exit(0);
	}

	if (!check())
		goto bad;

	switch(fork()) {
	case 0:
		break;
	case -1:
		fprintf(stderr, "vipw: can't fork");
		goto bad;
		/* NOTREACHED */
	default:
		exit(0);
		/* NOTREACHED */
	}

	if (makedb(temp)) {
		fprintf(stderr, "vipw: mkpasswd failed");
bad:		fprintf(stderr, "; %s unchanged.\n", passwd);
		(void)unlink(temp);
		exit(1);
	}

	/*
	 * possible race; have to rename four files, and someone could slip
	 * in between them.  LOCK_EX and rename the ``passwd.dir'' file first
	 * so that getpwent(3) can't slip in; the lock should never fail and
	 * it's unclear what to do if it does.  Rename ``ptmp'' last so that
	 * passwd/vipw/chpass can't slip in.
	 */
	(void)setpriority(PRIO_PROCESS, 0, -20);
	fend = strcpy(from, temp) + strlen(temp);
	tend = strcpy(to, passwd) + strlen(passwd);
	bcopy(".dir", fend, 5);
	bcopy(".dir", tend, 5);
	if ((fd = open(from, O_RDONLY, 0)) >= 0)
		(void)flock(fd, LOCK_EX);
	/* here we go... */
	(void)rename(from, to);
	bcopy(".pag", fend, 5);
	bcopy(".pag", tend, 5);
	(void)rename(from, to);
	bcopy(".orig", fend, 6);
	(void)rename(from, _PATH_PASSWD);
	(void)rename(temp, passwd);
	/* done! */
	exit(0);
}

check()
{
	register long id;
	register int lcnt, root;
	register char *p, *sh;
	long atol();
	char buf[1024], *getusershell(), *strsep();

	for (lcnt = 1; fgets(buf, sizeof(buf), stdin); ++lcnt) {
		/* skip lines that are too big */
		if (!index(buf, '\n')) {
			fprintf(stderr, "vipw: line too long");
			goto bad;
		}
		if (!(p = strsep(buf, ":\n")))		/* login */
			goto general;
		root = !strcmp(p, "root");
		(void)strsep((char *)NULL, ":\n");	/* passwd */
		if (!(p = strsep((char *)NULL, ":\n")))	/* uid */
			goto general;
		id = atol(p);
		if (root && id) {
			fprintf(stderr, "vipw: root uid should be 0");
			goto bad;
		}
		if (id > USHRT_MAX) {
			fprintf(stderr, "vipw: %s > max uid value (%d)",
			    p, USHRT_MAX);
			goto bad;
		}
		if (!(p = strsep((char *)NULL, ":\n")))	/* gid */
			goto general;
		id = atol(p);
		if (id > USHRT_MAX) {
			fprintf(stderr, "vipw: %s > max gid value (%d)",
			    p, USHRT_MAX);
			goto bad;
		}
		(void)strsep((char *)NULL, ":\n");	/* class */
		(void)strsep((char *)NULL, ":\n");	/* change */
		(void)strsep((char *)NULL, ":\n");	/* expire */
		(void)strsep((char *)NULL, ":\n");	/* gecos */
		(void)strsep((char *)NULL, ":\n");	/* directory */
		if (!(p = strsep((char *)NULL, ":\n")))	/* shell */
			goto general;
		if (root && *p)				/* empty == /bin/sh */
			for (;;)
				if (!(sh = getusershell())) {
					fprintf(stderr,
					    "vipw: warning, unknown root shell.");
					break;
				}
				else if (!strcmp(p, sh))
					break;
		if (strsep((char *)NULL, ":\n")) {	/* too many */
general:		fprintf(stderr, "vipw: corrupted entry");
bad:			fprintf(stderr, "; line #%d", lcnt);
			return(0);
		}
	}
	return(1);
}

makedb(file)
	char *file;
{
	int status, pid, w;

	if (!(pid = vfork())) {
		execl(_PATH_MKPASSWD, "mkpasswd", "-p", file, NULL);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1);
	return(w == -1 || status);
}

edit(file)
	char *file;
{
	int status, pid, w;
	char *p, *editor, *getenv(), *rindex();

	if (editor = getenv("EDITOR")) {
		if (p = rindex(editor, '/'))
			++p;
		else
			p = editor;
	}
	else
		p = editor = "vi";
	if (!(pid = vfork())) {
		execlp(editor, p, file, NULL);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1);
	return(w == -1 || status);
}
