#ifndef lint
static	char sccsid[] = "@(#)touch.c	4.6 (Berkeley) 9/14/87";
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

main(argc, argv)
	int	argc;
	char	**argv;
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
	char	*filename;
{
	struct stat	statbuffer;

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
		int	retval;

		if (chmod(filename,0666)) {
			fprintf(stderr, "touch: %s: couldn't chmod: ", filename);
			perror((char *)NULL);
			return(1);
		}
		retval = readwrite(filename,statbuffer.st_size);
		if (chmod(filename,statbuffer.st_mode)) {
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
	char	*filename;
	off_t	size;
{
	int	filedescriptor;
	char	first;
	off_t	lseek();

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
	fputs("usage: touch [-cf] file ...\n", stderr);
	exit(1);
}
