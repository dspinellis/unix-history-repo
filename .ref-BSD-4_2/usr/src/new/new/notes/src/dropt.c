static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"

#define BUFSZ	128

/*
 *	this file processes the director options.
 *
 *	call: contains the io pointer to the file.
 *
 *	allows continued access only if the user if a director.
 *
 *	the functions of the director options includes:
 *	1) granting/denial of director priviledges
 *	2) granting/denial of regular access priviledges
 *	3) changing the director message
 *	4) writing a policy note
 *
 *	Returns: -1 normally
 *		 -4 if the user hit cntrl d ( to total exit)
 *
 *	original author/outliner : Ray Essick may 29, 1981
 *	modified: Lou Salkind
 */

direct(io)
struct io_f *io;
{
	int     i;				/* scratch */
	int     c;
	char    title[DMLEN + 1];		/* hold new director message */
	char    ntitle[NNLEN + 1];		/* hold note file title */
	struct note_f   note;
	struct auth_f   auth;			/* author of policy note */
	struct daddr_f  where;
	char   *r, buff[BUFSZ + 1];
	int     buffptr;
	int     start, end;
	struct notesenv oldenv;
	int retstat;
	int	nnotes, nresps;

	if (allow(io, DRCTOK) == 0) {
		warn("Sorry, you are not a director");
		return(-1);
	}

	replot = 1;
	oldenv = curenv;
	setjmp(jenv);
	while (1) {
		if (replot) {
			replot = 0;
			erase();
			center(io->descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
			center(io->descr.d_drmes, DMLEN, 2, 40 - DMLEN / 2);
			at(3, 14);
			printf("Anonymous: ");
			/* at(3, 25); */
			printf(io->descr.d_stat & ANONOK ? "ON " : "OFF");
			at(3, 31);
			printf("Notefile: ");
			/* at(3, 41); */
			printf(io->descr.d_stat & OPEN ? "OPEN  " : "CLOSED");
			at(3, 51);
			printf("Networked: ");
			/* at(3, 62); */
			printf(io->descr.d_stat & NETWRKD ? "YES" : "NO ");
		}
		cmdprompt();
		c = gchar();		/* get the command character */
		switch (c) {
		case 'r':
		case '\f':
			replot = 1;
			break;
		case '?': 
		case 'h': 
			help(DIRHLP);
			break;

		case 'k':
		case 'q': 		/* leave director options */
			retstat = -1;
			goto out;

		case '\004':        	/* control D */
			retstat = QUITFAST;
			goto out;

		case '!': 			/* give him a shell */
			gshell();
			break;

		case 'p': 			/* run access lists */
			if (access(io) == QUITFAST) {
				retstat = QUITFAST;
				goto out;
			}
			break;			/* skipt out of the loop */

		case 'a': 			/* toggle anonymous option */
			lock(io, 'n');
			getdscr(io, &io->descr);
			/* get up to date descriptor */
			if (io->descr.d_stat & ANONOK)
				io->descr.d_stat &= NOT ANONOK;
			else
				io->descr.d_stat |= ANONOK;
			putdscr(io, &io->descr);
			unlock(io, 'n');
			at(3, 25);
			printf(io->descr.d_stat & ANONOK ? "ON " : "OFF");
			break;

		case 'c': 			/* compress the notefile */
			if (io->descr.d_stat & OPEN) {
				warn("Notefile must be closed to compress");
				continue;
			} 
			prompt("Compressing...");
			fflush(stdout);
			if (compress(io, LOCKIT, &nnotes, &nresps) >= 0)
				prompt("Left %d notes and %d responses", nnotes,
				nresps);
			else
				warn("notesfile compressed behind your back");
			break;

		case 'o': 				/* toggle open status */
			lock(io, 'n');
			getdscr(io, &io->descr);
			if (io->descr.d_stat & OPEN)
				io->descr.d_stat &= NOT OPEN;
			else
				io->descr.d_stat |= OPEN;
			putdscr(io, &io->descr);
			unlock(io, 'n');
			at(3, 41);
			printf(io->descr.d_stat & OPEN ? "OPEN  " : "CLOSED");
			break;

		case 'n': 			/* toggle network status */
			lock(io, 'n');
			getdscr(io, &io->descr);
			if (io->descr.d_stat & NETWRKD)
				io->descr.d_stat &= NOT NETWRKD;
			else
				io->descr.d_stat |= NETWRKD;
			putdscr(io, &io->descr);
			unlock(io, 'n');
			at(3, 62);
			printf(io->descr.d_stat & NETWRKD ? "YES" : "NO ");
			break;

		case 'm': 		/* collect a new director message */
			prompt("New director message: ");
			i = gline(title, DMLEN);
			if (i <= 1)
				break;			/* no new message */
			for (i--; i < DMLEN; i++)
				title[i] = ' ';
				/* space fill the message */

			lock(io, 'n');
			getdscr(io, &io->descr);
			/* get up-to-date version */
			for (i = 0; i < DMLEN; i++)
				io->descr.d_drmes[i] = title[i];
			putdscr(io, &io->descr);
			unlock(io, 'n');
			replot = 1;
			break;

		case 't': 			/* write title for note file */
			prompt("New title for notefile: ");
			i = gline(ntitle, NNLEN);
			if (i <= 1)
				break;			/* no new message */
			for (i--; i < NNLEN; i++)
				ntitle[i] = ' ';
			lock(io, 'n');
			/* get up-to-date version */
			getdscr (io, &io->descr);
			for (i = 0; i < NNLEN; i++)
				io->descr.d_title[i] = ntitle[i];
			putdscr (io, &io->descr);
			unlock(io, 'n');
			replot = 1;
			break;

		case 'w': 		/* let him write a new policy note */
			if (io->descr.d_plcy) {
				prompt("Rewrite policy? ");
				if (askyn () == 'n') {
					c = 'e';
					break;
				}
			}
			prompt("Edit New Policy Text:");
			if (gettext (io, &where, NULL) == 0) {
				c = 'e';
				break;
			}
			r = title;
			for (i = strmove ("POLICY NOTE", r); i < TITLEN; i++)
				title[i] = ' ';			/* spave fill */
			gettime(&note.n_date);		/* date of writing */
			getname(&auth, 0);		/* get author */
			putnote(io, &where, title, 0, &note, &auth, 1, 1, 1,
				SYSTEM, 1);
			dspnote(io, &note, 0);		/* show it to him */
			break;

		case 'z':		/* zap a lot of notes/responses */
			prompt("Enter list of notes to delete: ");
			i = gline(buff, BUFSZ);			/* grab line */
			if (i <= 1)
				continue;
			prompt("Really delete %s? ", buff);
			if (askyn () != 'y')
				break;			/* chicken out */
			buffptr = 0;
			at(14, 1);
			clear_eol();
			while (listget (buff, &buffptr, &start, &end)) {
				if (start == end)
					printf ("%d ", start);
				else
					printf ("%d-%d ", start, end);
				mdelete (io, start, end);	/* zap those */
			}
			continue;

		default: 
			warn("? for help, q to quit");
			continue;

		}
	}
out:
	ignsigs++;
	curenv = oldenv;
	ignsigs--;
	return(retstat);
}
