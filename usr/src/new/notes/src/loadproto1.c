#include	"parms.h"
#include	"structs.h"
#include	"dump.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: loadproto1.c,v 1.7 85/01/18 15:43:15 notes Rel $";
#endif	RCSIDENT

/*
 *	load notes and responses in protocol 1
 *
 *	Ray Essick	March 1984
 */

ldnote1 (io, whofrom, extensive, lockit, infile)
struct io_f *io;					/* notesfile */
char   *whofrom;					/* sending system */
FILE * infile;						/* incoming stream */
{
    struct note_f   note;				/* note descriptor */

    initnote (&note);					/* zero fields */

    while (fgets (line, sizeof line, infile) != NULL)
    {
	if ((varno = rfcparse (line, &notenanmes)) == -1)/* wierd */
	    continue;					/* ignore it */
	field = index (line, ':');			/* find data */
	field++;					/* skip colon */
	field++;					/* skip space */
	switch (varno)
	{
	    case NOTE_TITLE: 
	    case NOTE_AUTHOR: 
	    case NOTE_AUTHOR_UID: 
	    case NOTE_ID: 
	    case NOTE_WRITTEN: 
	    case NOTE_RECEIVED: 
	    case NOTE_MODIFIED: 
	    case NOTE_FROMSYS: 
	    case NOTE_STATUS: 
	    case NOTE_LENGTH: 
	    default: 					/* strange */
	}
    }
}


ldresp1 (io, whofrom, extensive, lockit, infile)
struct io_f *io;					/* notesfile */
char   *whofrom;					/* sending system */
FILE * infile;						/* incoming stream */
{
    while (fgets (line, sizeof line, infile) != NULL)
    {
	if ((varno = rfcparse (line, &notenanmes)) == -1)/* wierd */
	    continue;					/* ignore it */
	field = index (line, ':');			/* find data */
	field++;					/* skip colon */
	field++;					/* skip space */
	switch (varno)
	{
	    case RESP_TITLE: 				/* ignored */
	    case RESP_PARENT: 
	    case RESP_AUTHOR: 
	    case RESP_AUTHOR_UID: 
	    case RESP_ID: 
	    case RESP_WRITTEN: 
	    case RESP_RECEIVED: 
	    case RESP_FROMSYS: 
	    case RESP_STATUS: 
	    case RESP_LENGTH: 
	    default: 					/* strange */
	}
    }
}
