/* cpydata.c - copy from one fd to another */

#include "../h/mh.h"
#include <stdio.h>


void cpydata (in, out, ifile, ofile)
register int    in,
                out;
register char  *ifile,
               *ofile;
{
    register int    i;
    char    buffer[BUFSIZ];

    while ((i = read (in, buffer, sizeof buffer)) > 0)
	if (write (out, buffer, i) != i)
	    adios (ofile, "error writing");

    if (i == NOTOK)
	adios (ifile, "error reading");
}
