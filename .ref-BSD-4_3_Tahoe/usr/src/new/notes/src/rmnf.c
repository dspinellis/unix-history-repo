#define	MAINLINE
#ifdef	RCSIDENT
static char rcsid[] = "$Header: rmnf.c,v 1.7 85/01/18 15:39:00 notes Rel $";
#endif	RCSIDENT

/*
 *	rmnf - remove notefiles 
 *	rmnf removes the notefiles specified on the control card.
 *	It also goes through and cleans out the sequencer files
 *	that had entries for that notefile.
 *
 *	Since this can sort of destroy a lot of information, we
 *	ask for confirmation of the removal.
 *
 *	Original Coding: Ray Essick	December 1981
 */
#include "parms.h"
#include "structs.h"

main (argc, argv)
char  **argv;
{
    int     i;
    int     c;
    int     start = 1;					/* argv to start at */
    int     forced = 0;					/* -f flag */
    struct nflist_f *nfptr;

    startup (argc, argv);				/* common initialization */

    if (globuid != Notesuid)
    {
	printf ("You are not allowed to remove notefiles\n");
	exit (BAD);
    }

    if (argc == 1)
    {
	printf ("Usage: %s [-f] notefile [notefile ...]\n", argv[0]);
	exit (BAD);
    }

    if (!strcmp (argv[start], "-f"))			/* don't ask him */
    {
	forced++;
	start++;
    }

    for (i = start; i < argc; i++)
    {
	if (argv[i][0] == '/')				/* explicit names */
	{
	    printf ("%s: Won't remove %s\n", argv[0], argv[i]);
	    continue;
	}
	expand (argv[i]);				/* expand it */
    }

    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
    {
	if (chkpath (nfptr -> nf_name) == -1)
	{
	    printf ("%s: Bad notefile name\n", nfptr -> nf_name);
	    continue;
	}
	if (forced == 0)
	{
	    printf ("Really remove notesfile %s? ", nfptr -> nf_name);
	    c = getchar ();				/* grab 1 from the tty */
	    if (c != '\n')
		while (getchar () != '\n');		/* suck to eol */
	    if (c != 'y')
		continue;				/* don't delete */
	}
	x (chdir (Mstdir) < 0, "rmnf: bad chdir to MSTDIR");
	if (chdir (nfptr -> nf_name) < 0)
	{
	    printf ("%s: Can't delete or non-existent\n", nfptr -> nf_name);
	    continue;
	}

#ifndef	FASTFORK
	dounix ("rm -f *", 0, 0);			/* remove the files */
#else
	dounix (0, 0, hisshell, "-c", "rm -f *", 0, 0);
#endif

	x (chdir ("..") < 0, "rmnf: bad chdir ..");

#ifndef	FASTFORK
	{
	    char    cmdline[CMDLEN];
	    sprintf (cmdline, "rmdir %s", nfptr -> nf_name);/* remove directory */
	    dounix (cmdline, 0, 0);			/* execute command */
	}
#else
	dounix (0, 0, "/bin/rmdir", nfptr -> nf_name, 0, 0, 0);
#endif

	printf ("%s: Deleted\n", nfptr -> nf_name);

/*	now we get to remove the sequencer entries 
 *	This is a little complicated for sitting and typing from the tope
 *	of my head, so I will write it out later and type it in.
 *	Have to do stuff which include opening the sequencer directory
 *	and reading all the names from it to get the file names..
 *	Then have to sequence through each file doing
 *	 while ( more) { read; if (not one deleting) {write;count++} }
 *	Also if don't write any - unlink the file.
 *
 */
    }
    exit (GOOD);
}
