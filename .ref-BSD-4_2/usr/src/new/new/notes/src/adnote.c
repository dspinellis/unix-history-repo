static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
/*
 *	addnote(io, fid, pstr)
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

addnote(io, preface, pstr, tpstr)
struct io_f *io;
FILE *preface;
char *pstr;						/* edit pstr */
char *tpstr;					/* title pstr */
{
	struct auth_f   auth;
	struct daddr_f  where;
	struct note_f   note;
	char ntitle[TITLEN + 1];
	int anon;
	int stat, retcode, i;

	getdscr(io, &io->descr);			/* get up-to-date */
	if (allow(io, WRITOK) == 0) {			/* check writability */
		warn("Sorry, you are not allowed to write");
		fflush(stdout);				/* force to tty */
		sleep (2);
		return(-1);
	}
	prompt(pstr);
	putchar('\n');
	if (gettext(io, &where, preface) == 0)
		return(-1);
	stat = 0;
	if (io->access == (WRITOK + RESPOK))
		stat |= WRITONLY;
	if (allow(io, DRCTOK)) {			/* director mesg */
		prompt("Director message? ");
		if (askyn() == 'y')
			stat |= DIRMES;
	}
	anon = 'n';
	if ((io->descr.d_stat & ANONOK) && (stat & WRITONLY) == 0) {
		/* anon only if permitted and wants */
		prompt("Do you wish this to be anonymous? ");
		anon = askyn();
	}

	while (1) {				/* force him to type a title */
		prompt(tpstr);
		i = gline(ntitle, TITLEN);
		if (i == 1)
			continue;
		for (i--; i < TITLEN; i++)
			ntitle[i] = ' ';
		break;
	}
	gettime(&note.n_date);			/* get date of writing */
	getname(&auth, anon == 'y');		/* get author */
	lock(io,'n');				/* lock up for the duration */
	retcode = putnote(io, &where, ntitle, stat, &note, &auth, NOPOLICY,
			NOLOCKIT, COPYID, SYSTEM, 1);

	unlock(io, 'n');			/* all done critical */

	return(retcode);
}
