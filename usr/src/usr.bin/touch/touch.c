#ifndef lint
static	char sccsid[] = "@(#)touch.c	4.5 (Berkeley) %G%";
#endif

/*
 *	attempt to set the modify date of a file to the current date.
 *	if the file exists, read and write its first character.
 *	if the file doesn't exist, create it, unless -c option prevents it.
 *	if the file is read-only, -f forces chmod'ing and touch'ing.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

int	dontcreate;	/* set if -c option */
int	force;		/* set if -f option */

char *whoami = "touch";

main(argc,argv)
	int	argc;
	char	**argv;
{
	extern int optind;
	int ch;

	dontcreate = force = 0;
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
			fprintf(stderr, "usage: %s [-cf] file ...\n", whoami);
			exit(1);
		}
	for (argv += optind; *argv; ++argv)
		touch(*argv);
}

touch(filename)
	char	*filename;
{
	struct stat	statbuffer;

	if (stat(filename,&statbuffer) == -1) {
		if (!dontcreate) {
			readwrite(filename,0L);
		} else {
			fprintf(stderr, "%s: %s: does not exist\n",
				whoami, filename);
		}
		return;
	}
	if ((statbuffer.st_mode & S_IFMT) != S_IFREG) {
		fprintf(stderr, "%s: %s: can only touch regular files\n",
			whoami, filename);
		return;
	}
	if (!access(filename,R_OK | W_OK)) {
		readwrite(filename,statbuffer.st_size);
		return;
	}
	if (force) {
		if (chmod(filename,0666)) {
			fprintf(stderr, "%s: %s: couldn't chmod: ",
				whoami, filename);
			perror("");
			return;
		}
		readwrite(filename,statbuffer.st_size);
		if (chmod(filename,statbuffer.st_mode)) {
			fprintf(stderr, "%s: %s: couldn't chmod back: ",
				whoami, filename);
			perror("");
			return;
		}
	} else {
		fprintf(stderr, "%s: %s: cannot touch\n", whoami, filename);
	}
}

readwrite(filename,size)
	char	*filename;
	off_t	size;
{
	int	filedescriptor;
	char	first;
	off_t	lseek();

	if (size) {
		filedescriptor = open(filename,2);
		if (filedescriptor == -1) {
error:
			fprintf(stderr, "%s: %s: ", whoami, filename);
			perror("");
			return;
		}
		if (read(filedescriptor, &first, 1) != 1) {
			goto error;
		}
		if (lseek(filedescriptor,0l,0) == -1) {
			goto error;
		}
		if (write(filedescriptor, &first, 1) != 1) {
			goto error;
		}
	} else {
		filedescriptor = creat(filename,0666);
		if (filedescriptor == -1) {
			goto error;
		}
	}
	if (close(filedescriptor) == -1) {
		goto error;
	}
}
