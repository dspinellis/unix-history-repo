#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: preptxt.c,v 1.7 85/01/18 15:35:18 notes Rel $";
#endif	RCSIDENT

/*
 *	preptxt()
 *	put the standard headers and text into the specified file
 *
 *	Original coding:	Ray Essick	December 1981
 */

preptxt (io, zfile, author, date, where, title)
struct io_f *io;
FILE * zfile;
struct auth_f  *author;
struct when_f  *date;
struct daddr_f *where;
char   *title;
{
    char    buf[DATELEN + 6];				/* formatted date */
    char    auth[NAMESZ + SYSSZ + 2];			/* formatted author */
    int     linecount;
    char    ttl[TITLEN + 1];				/* title + \0 */
    int     i;

    sprdate (date, buf);				/* format the date */
    if (!strcmp (author -> aname, "Anonymous"))
	strcpy (auth, "Anonymous");
    else
    {
#ifdef	USERHOST
	sprintf (auth, "%s@%s", author -> aname, author -> asystem);
#else
	sprintf (auth, "%s!%s", author -> asystem, author -> aname);
#endif	USERHOST
    }

#ifdef	notdef
    fprintf (zfile, "/**** %s:%s / %s / %s ****/\n",
	    System, io -> nf, auth, buf);
    linecount = 1;					/* 1 line saved */
#else
/*
 *	A more verbose but prettier header
 */
    fprintf (zfile, "/* Written %s by %s in %s:%s */\n",
	    buf, auth, System, io -> nf);
    linecount = 1;
    if (title)
    {
	fprintf (zfile, "/* ---------- \"%s\" ---------- */\n", title);
	linecount++;					/* 1 more line saved */
    }
#endif

    linecount += pageout (io, where, zfile);		/* write to the file */

#ifdef	notdef
    fprintf (zfile, "/* ---------- */\n");		/* a trailer */
#else
/*
 *	prettier stuff; goes together with a prettier header message
 *	done above.
 */
    fprintf (zfile, "/* End of text from %s:%s */\n", System, io -> nf);
#endif

    linecount++;					/* count trailer */
    return linecount;
}
