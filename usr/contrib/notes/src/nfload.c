#define	MAINLINE
#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfload.c,v 1.7 85/01/18 15:41:19 notes Rel $";
#endif	RCSIDENT

/*
 *	quick hack at loading a notesfile.
 *
 *	The ASCII format comes down stdin.
 */

main (argc, argv)
int     argc;
char  **argv;
{
    int     i;
    int     start;
    char   *basedir;

    if (argc < 2)
    {
	printf ("Usage: %s [-Ddirectory] notesfile \n", argv[0]);
	exit (1);
    }

    startup (argc, argv);

    start = 1;
    basedir = Mstdir;

    if (!strncmp (argv[start], "-D", 2))		/* specified dir */
    {
	basedir = &argv[start][2];
	start++;
    }

    i = loadnf (basedir, argv[start], stdin);		/* load it */
    printf ("loadnf returns %d\n", i);
    exit (GOOD);
}
