/* date.c */

#include "tws.h"
#include <stdio.h>


/* ARGSUSED */

main (argc, argv)
int argc;
char **argv;
{
    struct tws *t;
    char buf[1024];

    while (gets(buf)) {
	t = dparsetime (buf);
	printf ("%s\n", dasctime (t));
    }
    exit (0);
}
