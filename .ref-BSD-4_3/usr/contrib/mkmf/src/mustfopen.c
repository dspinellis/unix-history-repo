/* $Header: mustfopen.c,v 1.2 85/03/17 12:51:04 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * mustfopen() opens a file in the manner of fopen(3). However, if the file
 * cannot be accessed, exit(1) is called.
 */
#include <stdio.h>

extern char *PGN;		/* program name */

FILE *
mustfopen(filename,mode)
	char *filename;
	char *mode;
{
	FILE *stream;			/* file stream */

	if ((stream = fopen(filename,mode)) == NULL)
		{
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "can't open %s\n",filename);
		exit(1);
		}
	return(stream);
}
