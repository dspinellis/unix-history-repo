static char *sccsid = "@(#)gtext.c	1.2 2/1/83";

#include "parms.h"
#include "structs.h"
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


gettext (io, where, preface)
struct io_f *io;
struct daddr_f *where;				/* where we left it */
FILE * preface;					/* text included in buffer */
{
    FILE * scr, *fopen ();
    int     c;
    char    cmd[CMDLEN];			/* build the editor call */
    char    fn[WDLEN];				/* scratch file name */
    extern char *myeditor;

    sprintf (fn, "/tmp/nf%d", getpid ());
    x ((scr = fopen (fn, "w")) == NULL, "gettext: create scratch");
    x (chmod (fn, 0666) < 0, "gettext: chmod tmp");
    if (preface != NULL) {
	while ((c = getc (preface)) != EOF) {
	    putc (c, scr);				/* move included text */
	}
    }
    fclose (scr);

    c = dounix (1, 1, myeditor, fn, 0, 0, 0);	/* call his editor */
    if (c != 0)
    	wfchar();
    x ((scr = fopen (fn, "r")) == NULL, "gettext: scratch file");

    c = pagein (io, scr, where);			/* move text in */
    fclose(scr);
    x (unlink (fn) < 0, "gettext: unlink");
    return(c);				/* return count of characters moved */
}
