/*	creat.c	4.1	83/02/23	*/

/*
 * Backwards compatible creat call.
 */
#include <sys/file.h>

creat(file, mode)
	char *file;
	int mode;
{

	return (open(file, FWRONLY|FCREATE|FTRUNCATE, mode));
}
