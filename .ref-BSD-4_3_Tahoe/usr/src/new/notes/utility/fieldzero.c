#define	MAINLINE

/*
 *	fieldchange
 *
 *	This simple program runs through the specified notesfiles
 *	and sets some fields in the descriptor to default
 *	values.  You'll have to pick which fields you actually
 *	want to change, this will depend on how recent your code is.
 *
 *
 *	Ray Essick	September 6, 1983
 *
 * $Header: /mntb/3/srg/notes/work/utility/RCS/fieldzero.c,v 1.6 84/03/07 19:04:34 notes Exp $
 */

#include "../src/parms.h"
#include "../src/structs.h"

main (argc, argv)
char  **argv;
{
    int     i,
            j,
            k;
    int     c;
    int     start,
            verbose;
    char   *p,
           *q,
           *r;
    char    cmdline[CMDLEN];
    struct io_f io;

    startup (argc, argv);				/* common initialization */


    verbose = 0;
    start = 1;
    if (!strcmp (argv[1], "-v"))
    {
	verbose++;
	start++;
    }

    for (i = start; i < argc; i++)			/* for each notesfile  */
    {
	if (init (&io, argv[i]) < 0)
	{
	    printf ("bong %s\n", argv[i]);
	    continue;
	}
	if (verbose)
	    printf ("%s\n", argv[i]);

	locknf (&io, 'n');
	getdscr (&io, &io.descr);			/* grab up to date */
/*
 *	the next few statements initialize a number of the
 *	new fields in the notesfile descriptor.  You may not want
 *	to use all of them at this time.
 */
	io.descr.d_archtime = 0;			/* expiration threshold */
	io.descr.d_workset = 0;				/* min notes to keep in nf */
	io.descr.d_dmesgstat = DIRDFLT;			/* expire w/wo dirmsg */
	io.descr.d_archkeep = KEEPDFLT;			/* delete or archive */
	io.descr.d_adopted = 0;				/* adopted orphans */

	io.descr.d_longnote = MAXMSG;			/* long messages */

	putdscr (&io, &io.descr);
	unlocknf (&io, 'n');
	finish (&io);

    }
    exit (GOOD);
}
