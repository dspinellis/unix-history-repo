#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: adnote.c,v 1.7.0.3 85/10/06 01:40:37 notes Rel $";
#endif	RCSIDENT

/*
 *	addnote(io, preface, pstr, tprompt, title)
 *	get a note from the terminal and store it in notefile specified by
 *	io. If fid >=0, then prepend text of that file (as positioned) in the
 *	scratch file so the new writer can include it in his note.
 *	Returns:	-1 if no write (empty note, no permissions)
 *			 else the notes number in the notefile.
 *
 *	Originally in index.c, but moved here so other routines could use it.
 *
 *	modifications:	Ray Essick	December 1981
 */

addnote (io, preface, pstr, tpstr, title, editflag)
struct io_f *io;
FILE * preface;
char   *pstr;						/* edit prompt */
char   *tpstr;						/* title prompt */
char   *title;						/* title (if specified) */
int     editflag;					/* interactive? */
{
    struct auth_f   auth;
    struct daddr_f  where;
    struct note_f   note;
    char    ntitle[TITLEN + 1],
            anon;
    int     stat,
            retcode,
            i;

    getdscr (io, &io -> descr);				/* get up-to-date */
    if (!allow (io, WRITOK))				/* check writability */
    {
	at (0, PROMPTMSGX);
	printf ("Sorry, you are not allowed to write");
	fflush (stdout);				/* force to tty */
	sleep (2);
	return (-1);
    }

#ifndef	WRITEARCH					/* allowing writes */
    if (io -> descr.d_stat & ISARCH && !allow (io, DRCTOK))
    {
	at (0, PROMPTMSGX);
	printf ("Sorry, you can not write in an archive");
	fflush (stdout);
	sleep (2);
	return (-1);
    }
#endif	WRITEARCH
    at (0, PROMPTMSGX);
    if (editflag == EDIT)
	printf ("\n%s\n", pstr);			/* prompt him */
    if (gettext (io, &where, preface, editflag) == 0)	/* and get the text */
    {
	return (-1);
    }
    stat = 0;
    anon = 'n';
    if ((editflag == EDIT) && (io -> descr.d_stat & ANONOK))
    {							/* see if wants anon */
	anon = askyn ("Do you wish this to be anonymous (y/n): ");
	printf ("\r                                           \r");
	if (anon == 'y')				/* verify true */
	{
	    anon = askyn ("Do you REALLY wish this to be anonymous (y/n): ");
	    printf ("\r                                                  \r");
	}
    }
    if ((anon == 'n') && (io -> access == (WRITOK + RESPOK)))
    {							/* only if non-anon */
	stat |= WRITONLY;
    }
    if (editflag && allow (io, DRCTOK))			/* director mesg */
    {
	if (askyn ("Director message (y/n): ") == 'y')
	    stat |= DIRMES;
	printf ("\r                         \r");
    }

    if (title == NULL)					/* if no title specified */
	while (1)
	{						/* force him to type a title */
	    at (0, PROMPTMSGX);
	    printf ("%s", tpstr);
	    i = gline (ntitle, TITLEN - 1);
	    if (i != 1)					/* empty string */
		break;
	}
    else						/* he specified a title */
    {
	strncpy (ntitle, title, TITLEN);
	ntitle[TITLEN] = '\0';				/* sure it's terminated */
    }
    strclean (ntitle);					/* zap control characters */
    gettime (&note.n_date);				/* get date of writing */
    getname (&auth, anon == 'y');			/* get author */
    locknf (io, 'n');					/* lock up for the duration */
    retcode = putnote (io, &where, ntitle, stat, &note, &auth, NOPOLICY, NOLOCKIT, COPYID, System, 1);

    unlocknf (io, 'n');					/* all done critical */
    return (retcode);
}
