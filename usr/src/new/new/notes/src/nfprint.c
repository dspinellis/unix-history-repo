static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "globs.h"
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

#define	PLENGTH	66			/* length of a page */

int     length;				/* length of a page */
int     left;				/* lines left on the current page */
int     page;				/* which page we are on */
main (argc, argv)
char  **argv;
{
    struct io_f io;
    FILE * toc;				/* table of contents scratch file */
    FILE * lprfile;
    FILE * popen ();
    char   *p;
    char    buf[CMDLEN];
    char    cmdline[CMDLEN];
    struct note_f   note;
    register int    i;
    char    dfltrange[10];		/* hold default list */
    int     bufptr,
            start,
            end;
    int     singlepage;
    char    fn[WDLEN];			/* file name */
    int     argp;

#include "main.i"			/* common init code and such */

    if (argc == 1) {			/* tell him how */
	fprintf (stderr, "Usage: %s [-lnn] [-p] notefile list\n", argv[0]);
	exit (BAD);
    }

    page = 1;
    singlepage = 0;			/* no page break between notes */
    length = PLENGTH;
    argp = 1;				/* arg parsing */
    while (argv[argp][0] == '-') {
	switch (argv[argp][1]) {
	    case 'l': 			/* page length */
		length = atoi (&argv[argp][2]);
		break;

	    case 'p': 			/* start all notes on fresh page */
		singlepage++;
		break;

	    default: 
		fprintf (stderr, "Bad switch `%c'\n", argv[argp][1]);
		exit (BAD);
	}

	argp++;				/* jump to next one */
    }
    if (init (&io, argv[argp]) < 0) {	/* get the notesfile */
	exit (NONF);
    }
    if (allow (&io, READOK) == 0) {
	fprintf(stderr, "You are not allowed to read %s\n", argv[argp]);
	exit(BAD);
    }

    sprintf (fn, "/tmp/nf%d", getpid());	/* build toc file */
    x ((toc = fopen(fn, "w")) == NULL, "nfprint: no scratch file");

    p = buf;
    for (i = 0; (i < NNLEN) && (io.descr.d_title[i] != ' '); i++) {
	*p++ = io.descr.d_title[i];		/* move title */
    }
    *p = '\0';					/* and null terminate */
    sprintf (cmdline, "pr -l%d -h '%s'", length, buf);
    x ((lprfile = popen(cmdline, "w")) == NULL, "nfprint: can't run pr");


    length -= 10;			/* pr uses 5 & 5 for header/footer */
    left = length;			/* empty page */
    if (argp == (argc - 1)) {		/* last arg ... */
	sprintf(dfltrange, "%d-%d", 1, io.descr.d_nnote);
	argv[argp--] = dfltrange;		/* set up as an arg */
    }
    argp++;					/* advance to next arg */
    for (; argp < argc; argp++) {
	bufptr = 0;
	while (listget(argv[argp], &bufptr, &start, &end)) {
	    if (start > end) {
		continue;			/* wrong order */
	    }
	    if (start > io.descr.d_nnote) {
		continue;			/* too far out */
	    }
	    if (start < 1) {
		start = 1;
	    }
	    if (end > io.descr.d_nnote) {
		end = io.descr.d_nnote;		/* max out */
	    }
	    for (i = start; i <= end; i++) {
		getnrec (&io, i, &note);
		if (note.n_stat & DELETED) {
		    continue;			/* its not really there */
		}
		if (singlepage && left != length) {      /* want page breaks? */
		    pagebreak(lprfile);
		}
		lprnote(&io, lprfile, toc, i, &note);
	    }
	}
    }
    x (fclose(toc) == EOF, "nfprint: bad fclose 1");
    x ((toc = fopen(fn, "r")) == NULL, "nfprint: bad reopen of toc");
    pagebreak(lprfile);			/* force a page break */
    while ((i = getc(toc)) != EOF)
	putc(i, lprfile);
    fclose(toc);
    x (unlink(fn) < 0, "nfprint: couldnt unlink scratch file");
    /* fix by RLS */
    pclose(lprfile);				/* flush the pipe */
    exit(GOOD);
}
