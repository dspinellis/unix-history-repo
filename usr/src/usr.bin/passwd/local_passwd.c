/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)local_passwd.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>
#include <errno.h>
#include <stdio.h>

uid_t uid;

char *progname = "passwd";
char *tempname;

local_passwd(uname)
	char *uname;
{
	struct passwd *pw;
	int pfd, tfd;
	char *getnewpasswd();

	if (!(pw = getpwnam(uname))) {
		(void)fprintf(stderr, "passwd: unknown user %s.\n", uname);
		exit(1);
	}

	uid = getuid();
	if (uid && uid != pw->pw_uid) {
		(void)fprintf(stderr, "passwd: %s\n", strerror(EACCES));
		exit(1);
	}

	pw_init();
	pfd = pw_lock();
	tfd = pw_tmp();

	/*
	 * Get the new password.  Reset passwd change time to zero; when
	 * classes are implemented, go and get the "offset" value for this
	 * class and reset the timer.
	 */
	pw->pw_passwd = getnewpasswd(pw);
	pw->pw_change = 0;
	pw_copy(pfd, tfd, pw);

	if (!pw_mkdb())
		pw_error((char *)NULL, 0, 1);
	exit(0);
}

char *
getnewpasswd(pw)
	register struct passwd *pw;
{
	register char *p, *t;
	int tries;
	char buf[_PASSWORD_LEN+1], salt[9], *crypt(), *getpass();

	(void)printf("Changing local password for %s.\n", pw->pw_name);

	if (uid && pw->pw_passwd[0] &&
	    strcmp(crypt(getpass("Old password:"), pw->pw_passwd),
	    pw->pw_passwd)) {
		errno = EACCES;
		pw_error(NULL, 1, 1);
	}

	for (buf[0] = '\0', tries = 0;;) {
		p = getpass("New password:");
		if (!*p) {
			(void)printf("Password unchanged.\n");
			pw_error(NULL, 0, 0);
		}
		if (strlen(p) <= 5 && (uid != 0 || ++tries < 2)) {
			(void)printf("Please enter a longer password.\n");
			continue;
		}
		for (t = p; *t && islower(*t); ++t);
		if (!*t && (uid != 0 || ++tries < 2)) {
			(void)printf("Please don't use an all-lower case password.\nUnusual capitalization, control characters or digits are suggested.\n");
			continue;
		}
		(void)strcpy(buf, p);
		if (!strcmp(buf, getpass("Retype new password:")))
			break;
		(void)printf("Mismatch; try again, EOF to quit.\n");
	}
	/* grab a random printable character that isn't a colon */
	(void)srandom((int)time((time_t *)NULL));
#ifdef NEWSALT
	salt[0] = _PASSWORD_EFMT1;
	to64(&salt[1], (long)(29 * 25), 4);
	to64(&salt[5], random(), 4);
#else
	to64(&salt[0], random(), 2);
#endif
	return(crypt(buf, salt));
}

static unsigned char itoa64[] =		/* 0 ... 63 => ascii - 64 */
	"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

to64(s, v, n)
	register char *s;
	register long v;
	register int n;
{
	while (--n >= 0) {
		*s++ = itoa64[v&0x3f];
		v >>= 6;
	}
}
