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
static char sccsid[] = "@(#)chpass.c	5.17 (Berkeley) %G%";
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
	 * The temporary file/file descriptor usage is a little tricky here.
	 * 1:	We start off with two fd's, one for the master password
	 *	file (used to lock everything), and one for a temporary file.
	 * 2:	Display() gets an fp for the temporary file, and copies the
	 *	user's information into it.  It then gives the temporary file
	 *	to the user and closes the fp, closing the underlying fd.
	 * 3:	The user edits the temporary file some number of times.
	 * 4:	Verify() gets an fp for the temporary file, and verifies the
	 *	contents.  It can't use an fp derived from the step #2 fd,
	 *	because the user's editor may have created a new instance of
	 *	the file.  Once the file is verified, its contents are stored
	 *	in a password structure.  The verify routine closes the fp,
	 *	closing the underlying fd.
	 * 5:	Delete the temporary file.
	 * 6:	Get a new temporary file/fd.  Pw_copy() gets an fp for it
	 *	file and copies the master password file into it, replacing
	 *	the user record with a new one.  We can't use the first
	 *	temporary file for this because it was owned by the user.
	 *	Pw_copy() closes its fp, flushing the data and closing the
	 *	underlying file descriptor.  We can't close the master
	 *	password fp, or we'd lose the lock.
	 * 7:	Call pw_mkdb() (which renames the temporary file) and exit.
	 *	The exit closes the master passwd fp/fd.
	 */
	pw_init();
	pfd = pw_lock();
	tfd = pw_tmp();

	if (op == EDITENTRY) {
		display(tfd, pw);
		edit(pw);
		(void)unlink(tempname);
		tfd = pw_tmp();
	}
		
	pw_copy(pfd, tfd, pw);

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
