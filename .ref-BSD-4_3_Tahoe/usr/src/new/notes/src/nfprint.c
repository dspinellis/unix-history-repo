#define	MAINLINE

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfprint.c,v 1.7 85/01/18 15:28:54 notes Rel $";
#endif	RCSIDENT

/*
 *	nfprint - print out the contents of a notefile.
 *
 *	this program produces line printable output for the 
 *	notefile specified. Included is a table of contents
 *	detailing where the notes are.
 *
 *	Call:
 *		nfprint [-lnn] notefile list > output
 *
 *	list is an orders list on notenumbers.
 *
 *	Original Coding:	Ray Essick	January 1982
 */

/*---------------------------------------------------------------------------
 * COMPION (sep 84-8-21)
 * added code to allow excluding or limiting to messages with director
 * flag set (used here to toggle messages between 'pending' and
 * 'completed' status)
 *---------------------------------------------------------------------------
 */

#include "parms.h"
#include "structs.h"

#define	PLENGTH	66					/* length of a page */

int     length;						/* length of a page */
int     left;						/* lines left on the current page */
int     page;						/* which page we are on */
int     justtitles;					/* no text */
int     usepr = 1;					/* paginate */
/* COMPION (sep 84-8-21) added next two declares */
int     exclude_director;				/* print no director messages */
int     director_only;					/* print only director messages */

main (argc, argv)
char  **argv;
{
    struct io_f io;
    FILE * toc;						/* table of contents scratch file */
    FILE * lprfile;
    FILE * popen ();
    char   *p;
    char    buf[WDLEN];
    char    cmdline[WDLEN];
    struct note_f   note;
    register int    i;
    char    dfltrange[10];				/* hold default list */
    int     bufptr,
            start,
            end;
    int     singlepage;
    char    fn[WDLEN];					/* file name */
    int     argp;

    startup (argc, argv);				/* common initialization */

    if (argc == 1)					/* tell him how */
    {
	fprintf (stderr,
		"Usage: %s [-lnn] [-c] [-p] [-{d,nd}] [-t] notesfile list\n",
		argv[0]);
	exit (BAD);
    }


    page = 1;
    singlepage = 0;					/* no page break between notes */
    justtitles = 0;					/* include text */
    usepr = 1;						/* default to pr */
							/* COMPION (sep 84-8-21) next two init lines added */
    exclude_director = 0;				/* don't exclude director messages */
    director_only = 0;					/* don't limit to director messages */
    length = PLENGTH;
    argp = 1;						/* arg parsing */
    while (argv[argp][0] == '-')
    {
	switch (argv[argp][1])
	{
	    case 'c': 					/* use cat instead */
		usepr = 0;
		break;
	    case 'l': 					/* page length */
		length = atoi (&argv[argp][2]);
		break;
/* COMPION (sep 84-8-21) next two cases added */
	    case 'd': 
		director_only++;
		break;
	    case 'n': 
		if (argv[argp][2] == 'd')
		{
		    exclude_director++;
		}
		break;
	    case 'p': 					/* start all notes on fresh page */
		singlepage++;
		break;

	    case 't': 					/* titles only */
		justtitles++;
		break;

	    default: 
		fprintf (stderr, "Bad switch `%c'\n", argv[argp][1]);
		exit (BAD);
	}

	argp++;						/* jump to next one */
    }
    if (init (&io, argv[argp]) < 0)			/* get the notesfile */
    {
	exit (NONF);
    }
    if (allow (&io, READOK) == 0)
    {
	fprintf (stderr, "You are not allowed to read %s\n", argv[argp]);
	exit (BAD);
    }

    sprintf (fn, "/tmp/nf%d", getpid ());		/* build toc file */
    x ((toc = fopen (fn, "w")) == NULL, "nfprint: no scratch file");

    p = buf;
    for (i = 0; i < NNLEN; i++)
	*p++ = io.descr.d_title[i];			/* move title */
    *p = '\0';						/* and null terminate */
    if (usepr)						/* paginate with pr */
	sprintf (cmdline, "pr -l%d -h '(%s) %s'", length, System, buf);
    else
	sprintf (cmdline, "cat -");			/* just use cat */
    x ((lprfile = popen (cmdline, "w")) == NULL, "nfprint: can't run pr");


    length -= 10;					/* pr uses 5/5 header/footer */
    left = length;					/* empty page */
    if (argp == (argc - 1))				/* last arg ... */
    {
	sprintf (dfltrange, "%d-%d", 1, io.descr.d_nnote);
	argv[argp--] = dfltrange;			/* set up as an arg */
    }
    argp++;						/* advance to next arg */
    for (; argp < argc; argp++)
    {
	bufptr = 0;
	while (listget (argv[argp], &bufptr, &start, &end))
	{
	    if (start > end)
		continue;				/* wrong order */
	    if (start > io.descr.d_nnote)
		continue;				/* too far out */
	    if (start < 1)
		start = 1;
	    if (end > io.descr.d_nnote)
		end = io.descr.d_nnote;			/* max out */
	    for (i = start; i <= end; i++)
	    {
		getnrec (&io, i, &note);
		if (note.n_stat & DELETED)
		    continue;				/* its not really there */
/*---------------------------------------------------------------------------
 * COMPION (sep 84-8-21)
 * added test to allow or inclusion/exclusion based on director message
 * flag
 *---------------------------------------------------------------------------
 */
		if ((exclude_director && (note.n_stat & DIRMES))
			|| (director_only && !(note.n_stat & DIRMES)))
		{
		    continue;
		}
		if (singlepage && left != length)	/* want page breaks? */
		    pagebreak (lprfile);
		lprnote (&io, lprfile, toc, i, &note, justtitles);
	    }
	}
    }
    x (fclose (toc) == EOF, "nfprint: bad fclose 1");
    x ((toc = fopen (fn, "r")) == NULL, "nfprint: bad reopen of toc");
    if (!justtitles)					/* only if w/ text */
	pagebreak (lprfile);				/* force page break */
    while ((i = getc (toc)) != EOF)
	putc (i, lprfile);
    fclose (toc);
    x (unlink (fn) < 0, "nfprint: couldnt unlink scratch file");
    pclose (lprfile);					/* flush the pipe */
							/* and wait too! */
    exit (GOOD);
}
