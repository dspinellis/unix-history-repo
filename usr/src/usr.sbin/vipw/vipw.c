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
static char sccsid[] = "@(#)vipw.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <machine/machparam.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <stdio.h>
#include <errno.h>

/*
 * Password file editor with locking.
 */
static char	*passwd = "/etc/passwd", buf[BUFSIZ];

main()
{
	register int n, fd_passwd, fd_temp;
	static char *temp = "/etc/ptmp";
	struct stat s1, s2;
	char *editor, *getenv();

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);

	setbuf(stderr, (char *)NULL);
	(void)umask(0);

	if ((fd_passwd = open(passwd, O_RDONLY, 0)) < 0) {
		fputs("vipw: ", stderr);
		perror(passwd);
		exit(1);
	}
	if ((fd_temp = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
		extern int errno;

		if (errno == EEXIST) {
			fputs("vipw: password file busy.\n", stderr);
			exit(1);
		}
		fputs("vipw: ", stderr);
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
		fprintf(stderr, "vipw: can't reopen temp file; %s unchanged.\n", passwd);
		goto bad;
	}
	if (fstat(fileno(stdin), &s2)) {
		fprintf(stderr, "vipw: can't stat temp file; %s unchanged.\n", passwd);
		goto bad;
	}
	if (s1.st_mtime == s2.st_mtime) {
		fprintf(stderr, "vipw: %s unchanged.\n", passwd);
		goto bad;
	}
	if (!s2.st_size) {
		fprintf(stderr, "vipw: bad temp file; %s unchanged.\n", passwd);
		goto bad;
	}
	if (check()) {
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

#define	CHN	((char *)NULL)
static
check()
{
	register char *cp, *sh;
	register long id;
	register int root;
	long atol();
	char *token(), *getusershell();

	for (root = 0; gets(buf); root = 0) {
		if (!*buf) {
			fputs("vipw: empty line.\n", stderr);
			continue;
		}
		if (!(cp = token(buf)) || !*cp)		/* login */
			goto bad;
		if (!strcmp(cp, "root"))
			root = 1;
		(void)token(CHN);			/* passwd */
		if (!(cp = token(CHN)) || !*cp)		/* uid */
			goto bad;
		id = atol(cp);
		if (root && id) {
			fprintf(stderr, "vipw: root uid should be 0; %s unchanged.\n", passwd);
			return(0);
		}
		if (id > USHRT_MAX) {
			fprintf(stderr, "vipw: %s > max uid value (%u); %s unchanged.\n", cp, USHRT_MAX, passwd);
			return(0);
		}
		if (!(cp = token(CHN)) || !*cp)		/* gid */
			goto bad;
		id = atol(cp);
		if (id > USHRT_MAX) {
			fprintf(stderr, "vipw: %s > max gid value (%u); %s unchanged.\n", cp, USHRT_MAX, passwd);
			return(0);
		}
		(void)token(CHN);			/* gcos */
		if (!token(CHN))			/* home directory */
			goto bad;
		if (!(cp = token(CHN)))			/* shell */
			goto bad;
		if (root && *cp)			/* empty == /bin/sh */
			for (;;)
				if (!(sh = getusershell())) {
					fprintf(stderr, "vipw: illegal shell (%s) for root; %s unchanged.\n", cp, passwd);
					return(0);
				}
				else if (!strcmp(cp, sh))
					break;
		if (token(CHN)) {			/* too many fields */
bad:			fprintf(stderr, "vipw: corrupted entry; %s unchanged.\n", passwd);
			return(0);
		}
	}
	return(1);
}

static
makedb(file)
	char *file;
{
	int status, pid, w;

	if (!(pid = vfork())) {
		execl("/etc/mkpasswd", "mkpasswd", file, 0);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1);
	if (w == -1 || status)
		return(-1);
	return(0);
}

static char *
token(bfr)
	char *bfr;
{
	static char *cp;
	char *start;

	if (bfr)			/* re-init string */
		cp = bfr;
	else if (!cp)			/* check if hit EOS last time */
		return(CHN);
	else if (!bfr)			/* start at next char after ':' */
		++cp;
	for (start = cp;; ++cp)
		if (!*cp) {		/* found EOS; mark it for next time */
			cp = CHN;
			break;
		}
		else if (*cp == ':') {	/* found ':'; end token */
			*cp = '\0';
			break;
		}
	return(start);			/* return token */
}
