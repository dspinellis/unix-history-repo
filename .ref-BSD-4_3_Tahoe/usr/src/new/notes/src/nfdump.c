#define	MAINLINE
#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfdump.c,v 1.7.0.1 85/10/09 18:07:40 notes Rel $";
#endif	RCSIDENT

main (argc, argv)
int     argc;
char  **argv;
{
    int     i;
    if (argc != 2)
    {
	printf ("Usage: %s notesfile\n", argv[0]);
	exit (1);
    }

    startup (argc, argv);

    /* 
     * the "-" is vestigal from the days when this routine 
     * accepted a file. we force it out stdout now.
     * it would be nice to have /dev/stdout type names
     * or to merely change the arg to be a file descriptor
     */
    i = dumpnf (argv[1], "-");
    fprintf (stderr, "dumpnf returned %d\n", i);
    exit (0);
}
