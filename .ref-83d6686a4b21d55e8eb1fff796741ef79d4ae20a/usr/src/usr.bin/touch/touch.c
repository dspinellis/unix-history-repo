/*
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)touch.c	4.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * Attempt to set the modify date of a file to the current date.  If the
 * file exists, read and write its first character.  If the file doesn't
 * exist, create it, unless -c option prevents it.  If the file is read-only,
 * -f forces chmod'ing and touch'ing.
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>

static int	dontcreate;	/* set if -c option */
static int	force;		/* set if -f option */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	int ch, retval;

	dontcreate = force = retval = 0;
	while ((ch = getopt(argc, argv, "cf")) != EOF)
		switch((char)ch) {
		case 'c':
			dontcreate = 1;
			break;
		case 'f':
			force = 1;
			break;
		case '?':
		default:
			usage();
		}
	if (!*(argv += optind))
		usage();
	do {
		retval |= touch(*argv);
	} while (*++argv);
	exit(retval);
}

touch(filename)
	char *filename;
{
	struct stat statbuffer;

	if (stat(filename, &statbuffer) == -1) {
		if (!dontcreate)
			return(readwrite(filename, 0L));
		fprintf(stderr, "touch: %s: does not exist\n", filename);
		return(1);
	}
	if ((statbuffer.st_mode & S_IFMT) != S_IFREG) {
		fprintf(stderr, "touch: %s: can only touch regular files\n",
		    filename);
		return(1);
	}
	if (!access(filename, R_OK | W_OK))
		return(readwrite(filename,statbuffer.st_size));
	if (force) {
		int retval;

		if (chmod(filename, 0666)) {
			fprintf(stderr, "touch: %s: couldn't chmod: ",
			    filename);
			perror((char *)NULL);
			return(1);
		}
		retval = readwrite(filename, statbuffer.st_size);
		if (chmod(filename, statbuffer.st_mode)) {
			fprintf(stderr, "touch: %s: couldn't chmod back: ",
			    filename);
			perror((char *)NULL);
			return(1);
		}
		return(retval);
	}
	fprintf(stderr, "touch: %s: cannot touch\n", filename);
	return(1);
}

readwrite(filename, size)
	char *filename;
	off_t size;
{
	int filedescriptor;
	char first;
	off_t lseek();

	if (size) {
		filedescriptor = open(filename, O_RDWR, 0);
		if (filedescriptor == -1) {
error:			fprintf(stderr, "touch: %s: ", filename);
			perror((char *)NULL);
			return(1);
		}
		if (read(filedescriptor, &first, 1) != 1)
			goto error;
		if (lseek(filedescriptor, 0L, 0) == -1)
			goto error;
		if (write(filedescriptor, &first, 1) != 1)
			goto error;
	} else {
		filedescriptor = creat(filename, 0666);
		if (filedescriptor == -1)
			goto error;
	}
	if (close(filedescriptor) == -1)
		goto error;
	return(0);
}

usage()
{
	fprintf(stderr, "usage: touch [-cf] file ...\n");
	exit(1);
}
