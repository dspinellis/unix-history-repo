/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chpass.c	5.16 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <pwd.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "chpass.h"
#include "pathnames.h"

char *progname = "chpass";
char *tempname;
uid_t uid;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	register enum { NEWSH, LOADENTRY, EDITENTRY } op;
	register struct passwd *pw;
	struct passwd lpw;
	int ch, pfd, tfd;
	char *arg;

	op = EDITENTRY;
	while ((ch = getopt(argc, argv, "a:s:")) != EOF)
		switch(ch) {
		case 'a':
			op = LOADENTRY;
			arg = optarg;
			break;
		case 's':
			op = NEWSH;
			arg = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	uid = getuid();

	if (op == EDITENTRY || op == NEWSH)
		switch(argc) {
		case 0:
			if (!(pw = getpwuid(uid))) {
				(void)fprintf(stderr,
				    "chpass: unknown user: uid %u\n", uid);
				exit(1);
			}
			break;
		case 1:
			if (!(pw = getpwnam(*argv))) {
				(void)fprintf(stderr,
				    "chpass: unknown user %s.\n", *argv);
				exit(1);
			}
			if (uid && uid != pw->pw_uid)
				baduser();
			break;
		default:
			usage();
		}

	if (op == NEWSH) {
		/* protect p_shell -- it thinks NULL is /bin/sh */
		if (!arg[0])
			usage();
		if (p_shell(arg, pw, (ENTRY *)NULL))
			pw_error((char *)NULL, 0, 1);
	}

	if (op == LOADENTRY) {
		if (uid)
			baduser();
		pw = &lpw;
		if (!pw_scan(arg, pw))
			exit(1);
	}

	/*
	 * The file descriptor usage is a little tricky through here.
	 * 1:	We start off with two fd's, one for the master password
	 *	file, and one for the temporary file.
	 * 2:	Get an fp for the temporary file, copy the info to be
	 *	edited into it, and close the fp (closing the underlying
	 *	fd).
	 * 3:	The user edits the temporary file some number of times.
	 * 4:	Get an fp for the temporary file, and verify the contents.
	 *	We can't use the fp from step 2, because the user's editor
	 *	may have created a new instance of the file.  Close the
	 *	fp when done.
	 * 5:	Get an fp for the temporary file, truncating it as we do
	 *	so.  Get an fp for the master password file.  Copy the
	 *	master password file into the temporary file, replacing the
	 *	user record with a new one.  Close the temporary file fp
	 *	when done -- can't close the password fp, or we'd lose the
	 *	lock.
	 * 6:	Call pw_mkdb() and exit.  The exit closes the master password
	 *	fd from step 1, and the master password fp from step 5.
	 */
	pw_init();
	pfd = pw_lock();
	tfd = pw_tmp();

	if (op == EDITENTRY) 
		edit(tfd, pw);

	pw_copy(pfd, pw);

	if (!pw_mkdb())
		pw_error((char *)NULL, 0, 1);
	exit(0);
}

baduser()
{
	(void)fprintf(stderr, "chpass: %s\n", strerror(EACCES));
	exit(1);
}

usage()
{
	(void)fprintf(stderr, "usage: chpass [-a list] [-s shell] [user]\n");
	exit(1);
}
