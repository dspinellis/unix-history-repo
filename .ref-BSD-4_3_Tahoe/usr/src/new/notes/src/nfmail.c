#include "parms.h"
#include "structs.h"
#include <sysexits.h>					/* bsd only? */

#ifdef	RCSIDENT
static char *rcsid = "$Header: nfmail.c,v 1.7.0.8 85/10/20 11:09:09 notes Rel $";
#endif	RCSIDENT

/*
 *	nfmail
 *
 *	A simple program which goes through standard input, which 
 *	should be a formatted mail article with headers, and
 *	parses out the "Subject:" line.  We then turn around and
 *	use it to invoke "nfpipe" and send the letter to the appropriate
 *	notesfile (specified on the nfmail command line).
 *
 *	Original coding:	Wayne Hamilton,	U of Illinois CSO (I think)
 *	Modified:		Stuart Cracraft, SRI International
 *
 */


char   *getadr ();
FILE * popen ();

#define	IGNORESIZE 256

static char title[BUFSIZ] = "No Subject Line";

/*
 *	next three variables declared in "parsepath".
 */
extern char fromsys[SYSSZ + 1];				/* gave it to us */
extern char origsys[SYSSZ + 1];				/* started here */
extern char authname[NAMESZ + 1];			/* author */

char    Nfpipe[BUFSIZ];					/* nfpipe pathname */
char    tmpname[BUFSIZ];				/* scratch file */

char    system_rc[] = "/usr/lib/Mail.rc";

#define	MAX_IGNORE	32
char    ignore[MAX_IGNORE][IGNORESIZE];
int     ignore_cnt = 0;

int     AnchorSearch = TRUE;

main (argc, argv)
char  **argv;
{
    register    FILE * Ftmp;
    char    command[BUFSIZ],
            from[BUFSIZ],
            oldfrom[BUFSIZ],
            buf[BUFSIZ];
    int     gotsubj = FALSE,
            gotfrom = FALSE,
            gotoldfrom = FALSE;
    int     stripheader = FALSE;			/* leave headers in */
    int     letterstatus = 0;				/* director msg? */
    int     tossit;
    char   *myrc = 0;
    char   *home;
    int     i;
    char   *p,
           *q,
           *skipwhite ();

    struct io_f io;
    struct daddr_f  where;
    struct when_f   entered;
    struct id_f respid;
    struct auth_f   auth;
    struct when_f   whentime;
    int     notenum;
    int     status;
    struct note_f   note;

    startup (argc, argv);
    argc--;						/* blast command */
    argv++;
    from[0] = oldfrom[0] = '\0';			/* zero them */

    while (argc != 0)
    {

	if (strncmp (*argv, "-s\0", 3) == 0)		/* strip headers */
	{
	    argc--;
	    argv++;
	    stripheader = TRUE;
	    continue;
	}
	if (strncmp (*argv, "-F\0", 3) == 0)		/* floating match */
	{
	    argv++;
	    argc--;					/* to next arg */
	    AnchorSearch = FALSE;			/* floating search */
	    continue;
	}
	if (strncmp (*argv, "-d\0", 3) == 0)		/* enable dirmsg */
	{
	    argc--;
	    argv++;
	    letterstatus |= DIRMES;
	    continue;
	}
	if (strncmp (*argv, "-m\0", 3) == 0)		/* specify .mailrc */
	{
	    argc--;
	    argv++;
	    if (argc != 0)
	    {
		getignore (*argv);
	    }
	    else
	    {
		fprintf (stderr, "Need to specifiy -m file\n");
		goto usage;
	    }
	    argc--;
	    argv++;
	    continue;					/* next arg */
	}
	break;						/* not an arg */
    }


    if (!argc)
    {
usage: 
	fprintf (stderr, "Usage: %s [-F] [-s] [-m .mailrc-file] <notesfile>\n",
		Invokedas);
	exit (EX_USAGE);
    }

/*
 *	build ourselves a scratch file.  If we can't, then pass the 
 *	mail on with a default title.
 */

    sprintf (tmpname, "/tmp/nfm%05d", getpid ());
    sprintf (Nfpipe, "%s/nfpipe", BIN);
    if ((Ftmp = fopen (tmpname, "w")) == NULL)
    {
	fprintf (stderr, "nfmail: can't fopen temp file, but the mail gets thru\n");
	sprintf (command, "%s %s -t \"Mail to %s\"", Nfpipe, *argv, *argv);
	dopipe (command, stdin);
	unlink (tmpname);				/* ... remove scratch file */
	exit (EX_OK);					/* and leave */
    }

/*
 *	Step through the system Mail.rc file and pilfer the ignore commands.
 *	Then, process the .mailrc file in the home directory if there is one.
 */
    getignore (system_rc);

/*
 *	read through the mail looking for the subject line.
 */

    while (gets (buf) != NULL)
    {
	if (!buf[0])
	    break;					/* header's end */
	if (buf[0] == '\t')				/* continuation */
	    goto doit;					/* use same "tossit" */

	tossit = stripheader;
	if (!strncmp (buf, "Subject: ", 9))		/* check for title */
	{
	    if (!gotsubj)				/* only first one */
	    {
		strcpy (title, buf + 9);
		gotsubj = TRUE;
	    }
	    tossit = FALSE;
	    goto doit;					/* skip other tests */
	}
	if (!strncmp (buf, "From: ", 6))		/* author */
	{						/* grab user name */
	    if (!gotfrom)				/* only once */
	    {
		strcpy (from, buf + 6);
		gotfrom = TRUE;
	    }
	    tossit = FALSE;				/* keep all from lines */
	    goto doit;
	}
	if (!strncmp (buf, "From", 4) || !strncmp (buf, ">From", 5))
	{
	    if (!gotoldfrom)
	    {
		strcpy (oldfrom, buf + 5);		/* save it */
		gotoldfrom++;
	    }
	    tossit = FALSE;				/* save all addresses */
	}
	else
	    if (stripheader && !shouldignore (buf))
		tossit = FALSE;				/* "ignore" only when stripping */

doit: 							/* for continuation lines */
	if (tossit == FALSE)
	    fprintf (Ftmp, "%s\n", buf);		/* send the header line also */
    }							/* of header parsing loop */

    putc ('\n', Ftmp);					/* blank after headers */
    copy (stdin, Ftmp);
    fclose (Ftmp);

    if ((Ftmp = fopen (tmpname, "r")) == NULL)
    {
	unlink (tmpname);				/* ... remove scratch file */
	fprintf (stderr, "nfmail: can't re-fopen temp file %s\n", tmpname);
	exit (EX_UNAVAILABLE);
    }

/*
 *	Now that we have collected the letter and parsed such banalities
 *	as the title and the author and stripped any header lines that we
 *	don't care to hear about, it's time to put the letter into
 *	the notesfile.  We use routines scammed from our news/notes gateway
 *	code to look at the title and determine if it's a response to
 *	a previous letter.  This allows us to have the correct linkage
 *	for mail sent to a notesfile....
 */

    if ((i = init (&io, *argv)) < 0)
    {
	unlink (tmpname);				/* zap scratch file */
	fprintf (stderr, "%s: can't open notesfile %s (retcode %d)\n",
		Invokedas, *argv, i);
	/* 
	 * Should have a better scheme for knowing why can't open
	 */
	exit (EX_UNAVAILABLE);				/* bad nf or such */
    }
    p = title;
    while (*p && (*p == ' ' || *p == '\t'))		/* leading trash */
	p++;						/* skip */
    if (!strncmp (p, "re: ", 4) ||			/* it looks like */
	    !strncmp (p, "Re: ", 4) ||			/* a response */
	    !strncmp (p, "RE: ", 4))
    {
	do
	{
	    for (p += 3; *p == ' ' || *p == '\t'; p++);	/* drop spaces */
	} while (!strncmp (p, "re: ", 4) ||
		!strncmp (p, "Re: ", 4) ||
		!strncmp (p, "RE: ", 4));
	strncpy (io.xstring, p, TITLEN);		/* load it */
	io.xstring[TITLEN - 1] = '\0';			/* and terminate it */
	notenum = findtitle (&io, io.descr.d_nnote, AnchorSearch);/* start at back */
    }
    else
    {
	notenum = 0;					/* has to be new */
    }

/*
 *	OK. By now, we have a "notenum" if the article can be pegged
 *	as a response to one of our notes.
 *	Otherwise, notenum==0 and we'll have to turn it into
 *	a base note.
 */

    gettime (&whentime);
    gettime (&entered);
    /* 
     *	load the user's name 
     */
    if (from[0] != '\0')				/* got one */
    {
	p = q = from;
	while ((p = index (p, '<')) != (char *) NULL)
	    q = ++p;					/* get innermost <..> */
	p = index (q, '>');
	if (p != (char *) NULL)
	    *p = '\0';					/* zap */
	parsepath (q, (char *) NULL);			/* actually break it */
    }
    else
    {
	if (oldfrom[0] != '\0')
	{
	    parsepath (oldfrom, (char *) NULL);		/* try for something */
	}
	else
	{
	    strcpy (authname, "MAILER-DAEMON");		/* general catch-all */
	    origsys[0] = '\0';				/* local */
	}
    }
    strncpy (auth.aname, authname, NAMESZ);		/* user */
    if (origsys[0] == '\0')
	strncpy (auth.asystem, Authsystem, HOMESYSSZ);	/* local host */
    else
	strncpy (auth.asystem, origsys, HOMESYSSZ);	/* system */
    auth.aname[NAMESZ - 1] = auth.asystem[HOMESYSSZ - 1] = '\0';/* chop */
    auth.aid = Anonuid;					/* uid (none) */
#ifdef	DEBUG
    printf ("parse path returns the following:\n");
    printf ("authname: %s\n", authname);
    printf ("origsys: %s\n", origsys);
    printf ("fromsys: %s\n", fromsys);
#endif	DEBUG
    if (notenum > 0)
    {
	pagein (&io, Ftmp, &where);
	i = putresp (&io, &where, putresp, notenum, &entered, &auth, &note,
		LOCKIT, &respid, ADDID, System, ADDTIME, &whentime);
    }
    else
    {
	for (p = &title[0]; *p && (*p == ' ' || *p == '\t');)
	    p++;					/* strip blanks */
	for (i = 0; i < TITLEN; i++)			/* shift down */
	{
	    if ((title[i] = *p++) == '\0')		/* want assignment */
		break;					/* end */
	}
	title[TITLEN - 1] = '\0';			/* terminate for sure */
	pagein (&io, Ftmp, &where);
	gettime (&note.n_date);
	notenum = putnote (&io, &where, title, letterstatus, &note,
		&auth, NOPOLICY, LOCKIT, ADDID, System, ADDTIME);
    }

    finish (&io);					/* update numbers and close */
    fclose (Ftmp);					/* close and ... */
    unlink (tmpname);					/* ... remove scratch file */
    exit (EX_OK);
}


char   *skipwhite (p)
char   *p;
{
    while (*p == ' ' || *p == '\t' || *p == '\n')
	p++;
    return (p);
}


/*
 *	Get all the "ignore" commands from the file. Do nothing if the file
 *	does not exist.
 */
getignore (name)
char   *name;
{
    FILE * f;
    char    buff[IGNORESIZE];
    char   *p,
           *q;

    if ((f = fopen (name, "r")) == 0)
	return (0);

    while (!feof (f))
    {
	p = buff;
	fgets (buff, IGNORESIZE, f);
	p = skipwhite (p);

	if (strncmp (p, "ignore", 6) == 0)
	{
	    p = skipwhite (p + 6);

/*
 *	Collect the tags of the ignore command
 */

	    while (*p != 0)
	    {
		if (ignore_cnt >= MAX_IGNORE)
		{
		    fprintf (stderr, "%s: too many ignore tags\n", Invokedas);
		    exit (EX_DATAERR);
		}
		p = skipwhite (p);
		for (q = ignore[ignore_cnt];
			*p != ' ' && *p != '\t' && *p != '\n' && *p != 0;
			*(q++) = *(p++)
		    );
		*q = 0;
		if (!shouldignore (ignore[ignore_cnt]))
		{
		    ignore_cnt++;
		}
		p = skipwhite (p);
	    }
	}
    }

    fclose (f);
    return (0);
}



/*
 *	Should we ignore this line?
 */
shouldignore (p)
char   *p;
{
    int     i;

    for (i = 0; i < ignore_cnt; i++)
	if (strncmp (p, ignore[i], strlen (ignore[i])) == 0)
	    return (1);
    return (0);
}

/*
 *	simple command feeds what is left of the file "File" into
 *	a pipe feeding stdin of "command".
 *
 */

dopipe (command, File) char *command;
FILE * File;
{
    register    FILE * Pipe;

    if ((Pipe = popen (command, "w")) == NULL)
    {
	fprintf (stderr, "%s: can't popen (%s)!?\n", Invokedas, command);
	exit (EX_UNAVAILABLE);
    }

    copy (File, Pipe);
    pclose (Pipe);
}

/*
 *	copy rest of file "File" to "To".
 */

copy (From, To) FILE * From, *To;
{
    register int    c;

    while ((c = getc (From)) != EOF)
	putc (c, To);
}
