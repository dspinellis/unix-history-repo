#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: adresp.c,v 1.7.0.1 85/09/09 18:30:38 notes Rel $";
#endif	RCSIDENT

/*
 *	addresp(io, fid, notenum)
 *		grab text from the terminal, with the appropriate
 *	prepending from fid, and stick it in the notefile as a response
 *	to notenumber notenum. OF course the policy note can't have
 *	responses so we check that.
 *	
 *	Original coding:	Ray Essick	January 1982
 */

addresp (io, preface, notenum, editflag)
struct io_f *io;
FILE * preface;
{
    char    anon;
    char    c;
    int     status;
    int     resp;
    struct when_f   now;
    struct auth_f   auth;
    struct id_f unique;
    struct daddr_f  where;
    struct note_f   note;

#ifndef	WRITEARCH					/* allowing writes */
    if (io -> descr.d_stat & ISARCH && !allow (io, DRCTOK))
    {
	at (0, PROMPTMSGX);
	printf ("Sorry, you can not write in an archive");
	fflush (stdout);
	sleep (2);
	return (0);
    }
#endif	WRITEARCH
    resp = 0;
    at (0, PROMPTMSGX);
    if (editflag == EDIT)
	printf ("\nEdit Response Text:\n");
    if (gettext (io, &where, preface, editflag) != 0)
    {
	anon = 'n';
	if ((editflag == EDIT) && io -> descr.d_stat & ANONOK)
	{
	    anon = askyn ("Do you wish this response to be anonymous (y/n): ");
	    printf ("\r                                                     \r");
	    if (anon == 'y')				/* verify */
	    {
		anon = askyn ("Do you REALLY wish this response to be anonymous (y/n): ");
		printf ("\r                                                            \r");
	    }
	}
	status = 0;
	if ((editflag == EDIT) && allow (io, DRCTOK))	/* director mesg */
	{
	    c = askyn ("Director message (y/n): ");
	    printf ("\r                            \r");
	    if (c == 'y')
		status |= DIRMES;
	}

	gettime (&now);					/* get time of writing */
	getname (&auth, anon == 'y');			/* and author */
	locknf (io, 'n');				/* lock up notesfile */
	if ((resp = putresp (io, &where, status, notenum, &now, &auth, &note,
			NOLOCKIT, &unique, ADDID, System, ADDTIME, &now)) == 0)
	{
	    printf ("\nSorry, this note has just been deleted.\n");
	    fflush (stdout);				/* force it out */
	    sleep (2);
	}
	unlocknf (io, 'n');				/* free now */
    }
    return resp;					/* tell him which one */
}
