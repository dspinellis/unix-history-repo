/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)vipw.c	5.2 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>

/*
 * Password file editor with locking.
 */
static char	*passwd = "/etc/passwd",
		buf[BUFSIZ];

main()
{
	register int	n, fd_passwd, fd_temp;
	static char	*temp = "/etc/ptmp";
	struct stat	s1, s2;
	char	*editor,
		*getenv();

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);

	setbuf(stderr, (char *)NULL);
	umask(0);

	if ((fd_passwd = open(passwd, O_RDONLY, 0)) < 0) {
		fprintf(stderr, "vipw: ");
		perror(passwd);
		exit(1);
	}
	if ((fd_temp = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
		extern int errno;

		if (errno == EEXIST) {
			fputs("vipw: password file busy\n", stderr);
			exit(1);
		}
		fprintf(stderr, "vipw: ");
		perror(temp);
		exit(1);
	}
	while ((n = read(fd_passwd, buf, sizeof(buf))) > 0)
		if (write(fd_temp, buf, n) != n) {
			perror("vipw: write");
			goto bad;
		}
	if (n == -1) {
		perror("vipw: read");
		goto bad;
	}
	(void)close(fd_passwd);
	if (fsync(fd_temp)) {
		perror("vipw: fsync");
		goto bad;
	}
	if (fstat(fd_temp, &s1)) {
		perror("vipw: fstat");
		goto bad;
	}
	(void)close(fd_temp);

	if (!(editor = getenv("EDITOR")))
		editor = "vi";
	(void)sprintf(buf, "%s %s", editor, temp);
	if (system(buf)) {
		perror("vipw: system");
		goto bad;
	}

	if (!freopen(temp, "r", stdin)) {
		fprintf(stderr, "vipw: can't reopen temp file, %s unchanged\n", passwd);
		goto bad;
	}
	if (fstat(fileno(stdin), &s2)) {
		fprintf(stderr, "vipw: can't stat temp file, %s unchanged\n", passwd);
		goto bad;
	}
	if (s1.st_mtime == s2.st_mtime) {
		fprintf(stderr, "vipw: %s unchanged\n", passwd);
		goto bad;
	}
	if (!s2.st_size) {
		fprintf(stderr, "vipw: bad temp file, %s unchanged\n", passwd);
		goto bad;
	}
	if (checkroot()) {
		static char	*temp_pag = "/etc/ptmp.pag",
				*temp_dir = "/etc/ptmp.dir",
				*passwd_pag = "/etc/passwd.pag",
				*passwd_dir = "/etc/passwd.dir";

		if (makedb(temp) < 0)
			fputs("vipw: mkpasswd failed.\n", stderr);
		else if (rename(temp_pag, passwd_pag) < 0) {
			fprintf(stderr, "vipw: ");
			perror(temp_pag);
		}
		else if (rename(temp_dir, passwd_dir) < 0) {
			fprintf(stderr, "vipw: ");
			perror(temp_dir);
		}
		else if (rename(temp, passwd) < 0) {
			fprintf(stderr, "vipw: ");
			perror("rename");
		}
		else
			exit(0);
		(void)unlink(temp_pag);
		(void)unlink(temp_dir);
	}
bad:	(void)unlink(temp);
	exit(1);
}

static
checkroot()
{
	register int	cnt;
	register char	*cp, *sh;
	char	*getusershell();

	while (gets(buf)) {
		if (strncmp(buf, "root:", sizeof("root:") - 1))
			continue;
						/* skip password */
		for (cp = buf + sizeof("root:") - 1; *cp && *cp != ':'; ++cp);
		if (!*cp || atoi(++cp))		/* uid exists && uid == 0 */
			break;			/* skip uid, gid, gcos, dir */
		for (cnt = 0; *cp && cnt < 4; ++cp)
			if (*cp == ':')
				++cnt;
		if (!*cp)
			break;
		while (sh = getusershell())
			if (!strcmp(cp, sh))
				return(1);
		fprintf(stderr, "vipw: illegal shell (%s) for root login, %s unchanged.\n", cp, passwd);
		return(0);
	}
	fprintf(stderr, "vipw: root login corrupted, %s unchanged.\n", passwd);
	return(0);
}

static
makedb(file)
	char	*file;
{
	int	status, pid, w;

	if (!(pid = vfork())) {
		execl("/etc/mkpasswd", "mkpasswd", file, 0);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1);
	if (w == -1 || status)
		return(-1);
	return(0);
}
