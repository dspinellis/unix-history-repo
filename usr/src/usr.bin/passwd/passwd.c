/*
 * Copyright (c) 1988 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)passwd.c	4.35 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>

uid_t uid;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	struct passwd *pw;
	struct rlimit rlim;
	FILE *temp_fp;
	int fd;
	char *fend, *np, *passwd, *temp, *tend;
	char from[MAXPATHLEN], to[MAXPATHLEN];
	char *getnewpasswd();

	uid = getuid();
	switch(--argc) {
	case 0:
		if (!(pw = getpwuid(uid))) {
			fprintf(stderr, "passwd: unknown user: uid %u\n", uid);
			exit(1);
		}
		break;
	case 1:
		if (!(pw = getpwnam(argv[1]))) {
			fprintf(stderr, "passwd: unknown user %s.\n", argv[1]);
			exit(1);
		}
		if (uid && uid != pw->pw_uid) {
			fprintf(stderr, "passwd: %s\n", strerror(EACCES));
			exit(1);
		}
		break;
	default:
		fprintf(stderr, "usage: passwd [user]\n");
		exit(1);
	}

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
		if (errno == EEXIST) {
			fprintf(stderr,
			    "passwd: password file busy -- try again later.\n");
			exit(0);
		}
		fprintf(stderr, "passwd: %s: %s", temp, strerror(errno));
		goto bad;
	}
	if (!(temp_fp = fdopen(fd, "w"))) {
		fprintf(stderr, "passwd: can't write %s\n", temp);
		goto bad;
	}
	passwd = _PATH_MASTERPASSWD;
	if (!freopen(passwd, "r", stdin)) {
		fprintf(stderr, "passwd: can't read %s\n", passwd);
		goto bad;
	}

	printf("Changing password for %s.\n", pw->pw_name);
	np = getnewpasswd(pw, temp);

	if (!copy(pw->pw_name, np, temp_fp, pw))
		goto bad;

	(void)fclose(temp_fp);
	(void)fclose(stdin);

	switch(fork()) {
	case 0:
		break;
	case -1:
		fprintf(stderr, "passwd: can't fork");
		goto bad;
		/* NOTREACHED */
	default:
		exit(0);
		/* NOTREACHED */
	}

	if (makedb(temp)) {
		fprintf(stderr, "passwd: mkpasswd failed");
bad:		fprintf(stderr, "; password unchanged.\n");
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
	tend = strcpy(to, _PATH_PASSWD) + strlen(_PATH_PASSWD);
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

copy(name, np, fp, pw)
	char *name, *np;
	FILE *fp;
	struct passwd *pw;
{
	register int done;
	register char *p;
	char buf[1024];

	for (done = 0; fgets(buf, sizeof(buf), stdin);) {
		/* skip lines that are too big */
		if (!index(buf, '\n')) {
			fprintf(stderr, "passwd: line too long.\n");
			return(0);
		}
		if (done) {
			fprintf(fp, "%s", buf);
			continue;
		}
		if (!(p = index(buf, ':'))) {
			fprintf(stderr, "passwd: corrupted entry.\n");
			return(0);
		}
		*p = '\0';
		if (strcmp(buf, name)) {
			*p = ':';
			fprintf(fp, "%s", buf);
			continue;
		}
		if (!(p = index(++p, ':'))) {
			fprintf(stderr, "passwd: corrupted entry.\n");
			return(0);
		}
		/*
		 * reset change time to zero; when classes are implemented,
		 * go and get the "offset" value for this class and reset
		 * the timer.
		 */
		fprintf(fp, "%s:%s:%d:%d:%s:%ld:%ld:%s:%s:%s\n",
		    pw->pw_name, np, pw->pw_uid, pw->pw_gid,
		    pw->pw_class, 0L, pw->pw_expire, pw->pw_gecos,
		    pw->pw_dir, pw->pw_shell);
		done = 1;
	}
	return(1);
}

char *
getnewpasswd(pw, temp)
	register struct passwd *pw;
	char *temp;
{
	register char *p, *t;
	char buf[10], salt[2], *crypt(), *getpass();
	time_t time();

	if (uid && pw->pw_passwd &&
	    strcmp(crypt(getpass("Old password:"), pw->pw_passwd),
	    pw->pw_passwd)) {
		(void)printf("passwd: %s.\n", strerror(EACCES));
		(void)unlink(temp);
		exit(1);
	}

	for (buf[0] = '\0';;) {
		p = getpass("New password:");
		if (!*p) {
			(void)printf("Password unchanged.\n");
			(void)unlink(temp);
			exit(0);
		}
		if (strlen(p) <= 5) {
			printf("Please enter a longer password.\n");
			continue;
		}
		for (t = p; *t && islower(*t); ++t);
		if (!*t) {
			printf("Please don't use an all-lower case password.\nUnusual capitalization, control characters or digits are suggested.\n");
			continue;
		}
		(void)strcpy(buf, p);
		if (!strcmp(buf, getpass("Retype new password:")))
			break;
		printf("Mismatch; try again, EOF to quit.\n");
	}
	/* grab a random printable character that isn't a colon */
	(void)srandom((int)time((time_t *)NULL));
	while ((salt[0] = random() % 93 + 33) == ':');
	while ((salt[1] = random() % 93 + 33) == ':');
	return(crypt(buf, salt));
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
