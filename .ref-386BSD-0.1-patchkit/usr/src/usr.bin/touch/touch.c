/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)touch.c	4.8 (Berkeley) 6/1/90";
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
