#ifdef	RCSIDENT
static char rcsid[] = "$Header: mknf.c,v 1.7 85/01/18 15:19:42 notes Rel $";
#endif	RCSIDENT


/*
 *	This program will initialize an empty notefile. It leaves the
 *	caller as sole director of the notefile and also as the only
 *	person with access to the notefile. 
 *
 *	Since a notefile does suck up a little disk space, the use of
 *	this program is limited to the user whose uid matches the
 *	Notesuid constant. 
 *
 *	Original coding:	Rob Kolstad	Winter 1980
 *	Modified:		Ray Essick	November 1981
 */

#define	MAINLINE
#include 	"parms.h"
#include	"structs.h"

main (argc, argv)
char  **argv;						/* create a new notesfile */
{

    char   *q;
    int     k;						/* arg counter */
    int     j,
            Aflag,
            Oflag,
            Nflag;					/* option flags */
    char    basedir[WDLEN];
    char    endname[WDLEN];

    startup (argc, argv);				/* common initialization */

    if (globuid != Notesuid)
    {
	printf ("You are not allowed to build notefiles\n");
	exit (BAD);
    }

    if (argc == 1)
    {
	printf ("Usage: %s [-aon] topic1 [...]\n", argv[0]);
	exit (BAD);
    }

    Aflag = 0;
    Oflag = 0;
    Nflag = 0;

    for (k = 1; k < argc; k++)
    {

	if (argv[k][0] == '-')				/* options!!! */
	{
	    j = 1;
	    while (argv[k][j])
		switch (argv[k][j++])
		{
		    case 'a': 				/* anon notes ok */
			Aflag = 1;
			break;

		    case 'o': 				/* open notesfile */
			Oflag = 1;
			break;

		    case 'n': 				/* network available */
			Nflag = 1;
			break;

		    default: 				/* bad news */
			fprintf (stderr, "Bad switch: `%c'\n", argv[k][--j]);
			exit (BAD);
		}
	    continue;					/* on to the next arguement */
	}

	printf ("%s:\t", argv[k]);
	if (argv[k][0] == '/')				/* absolute path */
	{
	    q = rindex (argv[k], '/');			/* find trailing slash */
	    *q++ = '\0';				/* break into pieces */
	    strcpy (basedir, argv[k]);
	    strcpy (endname, q);
	    *--q = '/';					/* reconnect */
	}
	else
	{
	    strcpy (basedir, Mstdir);			/* default */
	    strcpy (endname, argv[k]);
	}

/*
 *	Actually make the notesfile in buildnf()
 */
	switch (buildnf (endname, basedir, Aflag, Oflag, Nflag))
	{
	    case 0: 					/* success */
		printf ("complete\n");
		break;

	    case (-1): 					/* invalid name */
	    case (-2): 					/* exists */
	    default: 					/* should be like this */
		break;					/* buildnf does newline */
	}

    }
    exit (GOOD);
}
