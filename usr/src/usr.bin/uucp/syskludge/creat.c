/* @(#)creat.c	4.3 (Berkeley) 1/3/83 */

#include <sys/file.h>

creat(file, mode)
	char *file;
	int mode;
{

	return (open(file, FCREATE|FWRONLY|FTRUNCATE, mode));
}
