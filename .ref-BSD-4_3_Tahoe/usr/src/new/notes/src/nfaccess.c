#define		MAINLINE
#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfaccess.c,v 1.7 85/01/18 15:40:27 notes Rel $";
#endif	RCSIDENT

/*
 *	This program works sort of chmod by adding/deleting
 *	permissions to the specified access lists.
 *
 *	Any normal user can run this program. It only allows changes
 *	to notesfiles the user is a director in.
 *
 *	Ray Essick		February 1984
 */



#define		MAXADD		1			/* simultaneous adds */
struct perm_f   Newmodes[MAXADD];			/* inserted modes */
static int  nmodes = 0;					/* active slots */

#include	<pwd.h>
#include	<grp.h>
extern struct passwd   *getpwnam ();
extern struct group *getgrnam ();


main (argc, argv)
int     argc;
char  **argv;
{
    struct io_f io;
    int     argn;
    struct nflist_f *nfptr;

    startup (argc, argv);				/* common init code */

    if (parsemode (argv[1], &Newmodes[nmodes++], 1))	/* grab a mode */
    {
	usage (argv[0]);				/* incorrect */
    }

    for (argn = 2; argn < argc; argn++)
    {
	expand (argv[argn]);				/* load it */
    }

    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
    {
	if (init (&io, nfptr -> nf_name) < 0)		/* open */
	{
	    printf ("%s:	couldn't open\n", nfptr -> nf_name);
	    continue;
	}
	if (!allow (&io, DRCTOK))			/* a director? */
	{
	    printf ("%s:	Not a director\n", nfptr -> nf_name);
	    closenf (&io);
	    continue;
	}

	addmodes (&io, nmodes, &Newmodes, 1);		/* add permissions */
	closenf (&io);
    }
    exit (0);						/* all done */
}

usage (name)						/* how to invoke */
char   *name;
{
    fprintf (stderr,
	    "Usage: %s <permission> <notesfile> [<notesfile>...]\n",
	    name);
    exit (1);
}
