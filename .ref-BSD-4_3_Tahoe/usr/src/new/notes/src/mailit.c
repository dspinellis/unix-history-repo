#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: mailit.c,v 1.7.0.3 85/07/25 14:07:21 notes Rel $";
#endif	RCSIDENT

/*
 *	mailit
 *
 *	takes the text record specified, along with the notefile name,
 *	the author name, (other things added later), and builds a 
 *	temp file. The user is prompted for a list of the people to
 *	send the file to. After an edit session, the mail is sent.
 *
 *	Returns:	-1 if no letter sent
 *			 0 if letter sent (or if it thinks so)
 *
 *	Original author: Ray Essick	June 1981.
 *	modified:	Ray Essick	December 1981.
 *	modified again:	Thanks to Malcolm Slaney of Purdue EE dept.
 *		added the SUPERMAILER processing. May 25, 1982
 *
 */

mailit (io, where, author, date, title, toauth, wtext)
struct io_f *io;
struct daddr_f *where;
struct auth_f  *author;
struct when_f  *date;
char   *title;
/* toauth - true if mail to author of note */
/* wtext - true is mail with text of note */
{
    char    buf[128];					/* > 5 + 80 + 3 + 15 */
    char    whoto[WDLEN + 1];				/* destination */
    char   *command;					/* mailer to use */
    char    fn[20];					/* hold scratch file name */
    int     f;
    char   *p;
#ifdef	SUPERMAILER
    char    subject[TITLEN + 20];			/* subject line for super mailer */
#endif
    int     i;
    FILE * txtfile;

    if (toauth)
    {
	if (strcmp (author -> aname, "Anonymous") == 0)
	{
	    printf ("Can't send to Anonymous\n");
	    fflush (stdout);
	    sleep (2);
	    return (-1);
	}
    }

    if (toauth)
    {
	if (strcmp (System, author -> asystem) != 0)
	{
#ifdef	USERHOST
	    sprintf (whoto, "%s@%s", author -> aname, author -> asystem);
#else
	    sprintf (whoto, "%s!%s", author -> asystem, author -> aname);
#endif	USERHOST
	}
	else
	    sprintf (whoto, "%s", author -> aname);
    }
    else
    {
	at (0, 1);
	printf ("\nSend to whom? ");
	if (gline (whoto, WDLEN) == 1)			/* will flush stdout */
	    return (-1);				/* no letter sent */

    }

#ifdef	SUPERMAILER
    strcpy (subject, title);
    for (p = subject; *p; p++)
	if (*p == '"' || *p == '`')
	    *p = '\'';					/* eliminate escape troubles */
#endif	SUPERMAILER

    sprintf (fn, "/tmp/nfm%d", getpid ());
    x ((txtfile = fopen (fn, "w")) == NULL, "mailit: creat tmp");
    x (chmod (fn, 0666) < 0, "mailit: chmod tmp");

    if (wtext)						/* add text if specified */
    {
	preptxt (io, txtfile, author, date, where, title);
    }

    fclose (txtfile);

    if ((command = getenv ("MAILER")) == NULL)		/* override it? */
	command = MAILER;				/* use default */
    at (0, 1);
#ifdef	SUPERMAILER					/* dumb mail interface */
    if (wtext)						/* do it hard way if with text */
    {
#endif
	printf ("Edit letter:\n");
	fflush (stdout);				/* clean out buffer */
#ifndef	FASTFORK
	sprintf (buf, "%s %s", hised, fn);		/* build edit command */
	dounix (buf, 1, 1);				/* call editor */
#else
	dounix (1, 1, hised, fn, 0, 0, 0);
#endif


	f = 0;						/* assume normal termination */
	while (1)
	{
	    sprintf (buf, "Send this to %s? (y/n):", whoto);
	    fflush (stdout);
	    if (askyn (buf) == 'y')
	    {
		printf ("\nSending...\n");
		fflush (stdout);
#ifdef SUPERMAILER
		sprintf (buf, "%s -s \"%s\" %s < %s", command, subject, whoto, fn);
#else
		sprintf (buf, "%s %s < %s", command, whoto, fn);
#endif
							/* make the command */
#ifndef	FASTFORK
		f = dounix (buf, 1, 0);			/* mail the thing */
#else
		f = dounix (1, 0, DFLTSH, "-c", buf, 0, 0);
/*
 *	Can't let him use the C shell, since we don't escape the '!' in
 *	remote addresses. RBE 6/21/82
 */
#endif
		break;					/* out of loop */
	    }
	    else					/* not to him... */
	    {
		/* 
		 * let him pick a new addressee
		 */
		printf ("\nSend to whom? ");
		if (gline (whoto, WDLEN) == 1)		/* will flush stdout */
		    break;				/* don't send */

	    }
	}
#ifdef	SUPERMAILER					/* simple case for SUPERMAILER */
    }
    else
    {
	printf ("%s %s\n", command, whoto);
	fflush (stdout);				/* empty buffers */
	sprintf (buf, "%s %s", command, whoto);
#ifndef	FASTFORK
	f = dounix (buf, 1, 1);				/* set the uid & tty */
#else
	f = dounix (1, 1, DFLTSH, "-c", buf, 0, 0);	/* do it */
#endif	FASTFORK
    }
#endif	SUPERMAILER

#ifdef	SUPERMAILER
    if (f != 0)						/* pause for error message */
    {
	fflush (stdout);
	sleep (2);
    }
    unlink (fn);
#else
    if (f != 0)						/* check error message */
    {
	fprintf ("Couldn't deliver mail; draft left in %s\n", fn);
	fflush (stdout);
	sleep (2);
    }
    else
	unlink (fn);
#endif	SUPERMAILER

    return 0;
}
