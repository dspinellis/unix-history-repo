static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
/*
 *	addresp(io, fid, notenum)
 *		grab text from the terminal, with the appropriate
 *	prepending from fid, and stick it in the notefile as a response
 *	to notenumber notenum. OF course the policy note can't have
 *	responses so we check that.
 *	
 *	Original coding:	Ray Essick	January 1982
 */

addresp(io, preface, notenum)
struct io_f *io;
FILE * preface;
{
	int    anon;
	int     c;
	int     status;
	int     resp;
	struct when_f   now;
	struct auth_f   auth;
	struct id_f unique;
	struct daddr_f  where;
	struct note_f   note;

	resp = 0;
	prompt("Edit Response Text:");
	putchar('\n');
	if (gettext(io, &where, preface) != 0) {
		anon = 'n';
		if (io->descr.d_stat & ANONOK) {
			prompt("Do you wish this response to be anonymous? ");
			anon = askyn();
		}
		status = 0;
		if (allow(io, DRCTOK))	{		/* director mesg */
			prompt("Director message? ");
			c = askyn();
			if (c == 'y')
				status |= DIRMES;
		}

		gettime(&now);			/* get time of writing */
		getname(&auth, anon == 'y');		/* and author */
		lock(io, 'n');				/* lock up notesfile */
		if ((resp = putresp(io, &where, status, notenum, &now, &auth,
		    &note, NOLOCKIT, &unique, ADDID, SYSTEM, ADDTIME, &now))
		    == 0) {
			warn("Sorry, this note has just been deleted.");
			wfchar();
		}
		unlock(io, 'n');			/* free now */
	}
	return(resp);
}
