#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: gtext.c,v 1.7 85/01/18 15:12:11 notes Rel $";
#endif	RCSIDENT

/*
 *	get the text for a note/response
 *
 *	Calls unix editor with a unique file name 
 *	Also makes sure the returned text is of 
 *	appropriate size
 *
 *	Ray Essick 10/23/80
 *	Modified : rbk	10/26/80
 *	modified again:	rbe 12 nov 81	fix to version 7 and general shtuff
 *	modified a third time to add insert-text for user
 *		Ray Essick	December 1981
 */

long    gettext (io, where, preface, editflag)
struct io_f *io;
struct daddr_f *where;					/* where we left it */
FILE * preface;						/* text included in buffer */
int     editflag;					/* EDIT if want editor, else NOEDIT */
{
    FILE * scr, *fopen ();
    register int    c;
    long    count;
    char    fn[20];					/* scratch file name */

    sprintf (fn, "/tmp/nf%d", getpid ());
    x ((scr = fopen (fn, "w")) == NULL, "gettext: create scratch");
    x (chmod (fn, 0666) < 0, "gettext: chmod tmp");
    if (preface != NULL)
    {
	while ((c = getc (preface)) != EOF)
	    putc (c, scr);				/* move included text */
    }
    fclose (scr);
    fflush (stdout);					/* clean it out */

    if (editflag == EDIT)
    {
#ifndef	FASTFORK
	{
	    char    cmd[CMDLEN];			/* build editor call */
	    sprintf (cmd, "%s %s", hised, fn);
	    dounix (cmd, 1, 1);				/* get the text */
	}
#else
	dounix (1, 1, hised, fn, 0, 0, 0);		/* call his editor */
#endif
    }							/* end of editflag test */

    if ((scr = fopen (fn, "r")) == NULL)		/* no text to read */
    {
	unlink (fn);					/* might just be protections */
	return ((long) 0);
    }

    count = pagein (io, scr, where);			/* move text in */
    fclose (scr);					/* close the scratch file and */
    x (unlink (fn) < 0, "gettext: unlink");		/* unlink it */
    return ((long) count);				/* chars moved */
}
