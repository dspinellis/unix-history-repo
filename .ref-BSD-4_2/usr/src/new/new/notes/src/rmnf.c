static char *sccsid = "@(#)rmnf.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "globs.h"
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

main (argc, argv)
int argc;
char  **argv;
{
    int     i;
    int     force = 0;
    int     start = 1;
    char    c,
            cmdline[NNLEN + 20];
    extern  char *myshell;

#include "main.i"			/* common init code and such */

    if (globuid != NOTESUID) {
	printf("You are not allowed to remove notefiles\n");
	exit(BAD);
    }

    if (argc == 1) {
	printf("Usage: %s [-f] notefile [notefile ...]\n", argv[0]);
	exit(BAD);
    }

    if ((argv[1][0] == '-') && (argv[1][1] == 'f')) {
	force = 1;
	start++;
    }

    for (i = start; i < argc; i++) {
	if (chkpath(argv[i]) == -1) {
	    printf("%s: Bad notefile name\n", argv[i]);
	    continue;
	}
	if (force == 0) {
	    printf("Really remove %s? ", argv[i]);
	    c = getchar();			/* grab 1 from the tty */
	    if (c != '\n') {
		while (getchar() != '\n');	/* suck to eol */
	    }
	    if (c != 'y') {
		continue;			/* don't delete */
	    }
	}
	x (chdir(MSTDIR) < 0, "rmnf: bad chdir to MSTDIR");
	if (chdir(argv[i]) < 0) {
	    printf("%s: Can't delete or non-existent\n", argv[i]);
	    continue;
	}
	if ((myshell = getenv("SHELL")) == NULL)
		myshell = SHELL;

	dounix(0, 0, myshell, "-c", "rm -f *", 0, 0);

	x (chdir("..") < 0, "rmnf: bad chdir ..");

#ifdef BSD4.1c
	x (rmdir(argv[i]) < 0, "rmnf: rmdir failed");
#else
	dounix(0, 0, "/bin/rmdir", argv[i], 0, 0, 0);
#endif BSD4.1c

	printf("%s: Deleted\n", argv[i]);

/*	now we get to remove the sequencer entries 
 *	This is a little complicated for sitting and typing from the top
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
