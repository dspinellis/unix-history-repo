/* date.c */

#include "tws.h"
#include <stdio.h>


/* ARGSUSED */

main (argc, argv)
int argc;
char **argv;
{
    printf ("%s\n", dtimenow ());

    exit (0);
}
