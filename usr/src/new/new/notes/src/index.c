static char *sccsid = "%W%";

/*
 * INPUT KEY PROCESSING FOR INDEX PHASE 
 *
 *	Process all keystrokes while the index page is on the screen.
 *	invokes director options also (if user qualified)
 *	Returns: >=0 read note with that number ( 0 is policy)
 *		 -1	reprint the index page
 *		 -2	leave notefiles, update sequencer time
 *		 -3	leave notefile, dont update sequencer
 *		 -4	Universal leave notefiles
 *			(-2 and -3 leave single notefile, -4 leaves
 *			entire package, aborting rest of notefiles.
 *
 *	Original Coding:	Rob Kolstad	Winter 1980
 *	modifications:		Ray Essick	December 1981
 *	modifications:		Lou Salkind	March 1983
 */

#include "parms.h"
#include "structs.h"
#include "newsgate.h"
#include <signal.h>
#include <sgtty.h>

indx(io, firstdis, lastdis, respnum)
struct io_f *io;
int *firstdis, *lastdis, *respnum;
{
	struct io_f io2;			/* for nested notefiles */
	struct when_f whendump;
	char nfname[NNLEN + 1];			/* for nested nfs */
	char cmdline[CMDLEN];
	int num;				/* note number */
	int i;
	int c;
	int znote, zresp;			/* for asearch */
	int temp;
	int retstat = -1;
	struct notesenv oldenv;

	*respnum = 0;				/* init response */
	ignsigs = 0;
	replot = 1;
	oldenv = curenv;

	setjmp(jenv);
	while (1) {
		if (replot) {
			replot = 0;
			prntind(io, firstdis, lastdis);
		}
		cmdprompt();
		c = gchar();
		switch (c) {				/* what to do? */
		case 'z': /* update sequencer and exit, RLS */
			retstat = QUITUPD;
			goto out;

		case 'u': /* unsubscribe from this notesgroup, RLS */
			if (unsubscribe(io->nf) < 0)
				continue;
			retstat = QUITSEQ;
			goto out;

		case '?': 
		case 'h': 
			help(INDXHLP);		/* put the help on screen */
			replot = 1;
			continue;

		case 'r': 			/* replot the index page */
		case '\014': 	/* everyone else uses ^L, might as well */
			replot = 1;
			continue;

		case 'W': 		/* this too shall write a note */
		case 'w': 		/* write a note */
			temp = addnote(io, NULL, "Edit Note text:", "Note Title: ");
			if (temp == -1) {
				replot = 1;
				continue;
			}

#ifdef NEWS
			/*
			 * fix to allow the user to specify whether
			 * the article should be local or net'ed
			 */
			if ((io->descr.d_stat & NETWRKD) == 0) {  
				/*
				    prompt("(Not networked)");
				    fflush(stdout);
				    sleep(2);
				*/
				retstat = temp;
				goto out;
			}
			prompt("Send to news? ");
			if (askyn() == 'y') {
#ifdef DEMANDNEWS
				/* send it to the news */
				sprintf(cmdline, "%s/%s/newsoutput", MSTDIR, UTILITY);
				dounix(0, 0, cmdline, io->nf, 0, 0, 0);
#endif DEMANDNEWS
			} 
			else {
				/* don't send it to the network */
				gettime(&whendump);
				fixlast(&whendump, io->nf, 1 , NEWSSYS);
			}
#endif NEWS
			retstat = temp;
			goto out;

		case 'B': 			/* bitch, bitch, bitch */
			if (init(&io2, GRIPES) < 0) {	/* no gripe file */
				warn("Gripe file not available");
				continue;
			}
			temp = addnote(&io2, NULL, "Edit Gripe text:", "Gripe Header: ");
			/* let him put the note in */
			finish(&io2);		/* close up the gripe file */
			if (temp == -1) {
				replot = 1;
				continue;
			}
			replot = 1;
			goto out;

		case '-': 				/* back up a little */
		case '\b': 				/* add backspace also */
			if (*firstdis != 1) {
				*firstdis -= nindex - 1;
				replot = 1;
			} else
				warn("On first page");
			continue;

		case '=': 				/* back up a lot */
			if (*firstdis != 1) {
				*firstdis = 1;
				replot = 1;
			} else
				warn("On first page");
			continue;

		case '+': 
		case '\r': 
		case '\n': 
		case ' ': 
			if (*lastdis < io->descr.d_nnote) {
				*firstdis = *lastdis;
				replot = 1;
			} else
				warn("On last page");
			continue;

		case '*': 			/* skip to last page */
			if (*lastdis < io->descr.d_nnote) {
				*firstdis = io->descr.d_nnote - nindex + 1;
				replot = 1;
			} else
				warn("Already on last page");
			continue;

		case 'q': 
		case 'k': 			/* so can use just right hand */
			retstat = QUITSEQ;
			goto out;

		case '\04': 				/* control D */
			retstat = QUITFAST;
			goto out;

		case 'Q': 		/* exit without update of sequencer */
		case 'K': 		/* so can use just right hand */
			retstat = QUITNOSEQ;
			goto out;

		case 'n': 			/* nest notesfiles - a stack */
			prompt("New notesfile: ");
			if (gline(nfname, NNLEN) == 1)
				continue;
			closenf(io);				/* save fids */
			i = control(nfname, NOSEQ);
			if (opennf(io, io->nf) < 0) {
				warn("Couldn't reopen notesfile");
				wfchar();
				retstat = QUITNOSEQ;
				goto out;
			}
			if (i == QUITNEX || i == QUITBAD) {
				warn("Can not open notesfile `%s'", nfname);
				continue;
			}
			if (i == QUITFAST || i == QUITUPD) {
				retstat = i;
				goto out;
			}
			replot = 1;
			continue;

		case 'p': 				/* to read note 0 */
			if (io->descr.d_plcy) {
				retstat = 0;
				goto out;
			} else {
				warn("There is no policy note");
				continue;		/* grab another key */
			}

		case 'd': 			/* to director options */
			if (allow(io, DRCTOK)) {
				if ((i = direct(io)) == -1) {
					replot = 1;
					continue;
				}
				retstat = i;
				goto out;
			} 
			/*
			 * fix the allow the user to see things
			 * the director can see
			 */
			prompt("Anonymous: %s 	  Networked: %s",
			(io->descr.d_stat & ANONOK) ? "YES" : "NO",
			(io->descr.d_stat & NETWRKD) ? "YES" : "NO");
			continue;

		case 'x': 
		case 'X': 
			i = tsearch(io, *lastdis, c == 'x');
			/* assume lies before here */
			if (i > 0) {
				retstat = i;
				goto out;
			}
			continue;		/* otherwise get another key */

		case 'a': 
		case 'A': 		/* author search from current spot */
			znote = *lastdis;
			zresp = 0;		/* start at the correct place */
			i = asearch (io, &znote, &zresp, (c == 'a'));
			/* look */
			if (i > 0) {
				*respnum = zresp;/* return correct value */
				retstat = znote;
				goto out;
			}
			continue;		/* get another command */

		case '1': 
		case '2': 
		case '3': 
		case '4': 
		case '5': 
		case '6': 
		case '7': 
		case '8': 
		case '9': 
			prompt("Read note > ");
			if ((num = getnum (c)) == 0)
				continue;
			retstat = num;
			goto out;

		case 'j': 
		case 'J':			/* goto first unread article */
		case 'l':			/* universal seq, RLS */
		case 'L':
			i = nxtnote(io, 0, &io->stime);
			if (i != -1) {
				retstat = i;
				goto out;
			}
			if (c == 'l' || c == 'L') {
				retstat = QUITSEQ;
				goto out;
			}
			replot = 1;
			continue;

		case 'o':			/* modify sequencer time */
			gdate(&io->stime);	/* let him hack on the time */
			continue;			/* and go back */

		case 'O': 			/* set it for today's notes */
			gettime(&io->stime);		/* grab current date */
			io->stime.w_hours = 0;		/* beginning of day */
			io->stime.w_mins = 0;
			prompt("Set to read notes since: ");
			prdate(&io->stime);
			continue;		/* and get the next command */


		case '!': 
			gshell();
			/* give him a shell in right directory */
			replot = 1;
			continue;

		default: 
			warn("? for help, q to quit");
			continue;
		}
	}
out:
	ignsigs++;
	curenv = oldenv;
	ignsigs = 0;
	return(retstat);
}
