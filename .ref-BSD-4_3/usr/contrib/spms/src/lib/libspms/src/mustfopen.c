/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * mustfopen() opens a file in the manner of fopen(3). However, if the file
 * cannot be accessed, exit(1) is called.
 */
#include <stdio.h>

FILE *
mustfopen(filename,mode)
	char *filename;
	char *mode;
{
	FILE *stream;			/* file stream */

	if ((stream = fopen(filename,mode)) == NULL)
		fatal("can't open %s",filename);
	return(stream);
}
