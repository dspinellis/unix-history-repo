static char *sccsid = "%W%";

/*
 *	Code in this module ( control.c) is designed to process
 *	a single notefile. Its paramters include the name of
 *	the notefile to process and whether to use the sequencer.
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	December, 1981.
 */

#include "parms.h"
#include "structs.h"

control(nfname, seqon)
char *nfname;
{
	struct io_f io;
	struct note_f   note;
	struct when_f   entered;	/* entry into notefile package */
	struct auth_f   whoami;		/* for sequencer */

	int	ret;
	int     firstdis, lastdis;	/* display counters */
	int     readnum;		/* where to start reading notes */
	int     respnum;		/* response to be on */
	/* currently 0 always */

	gettime(&entered);		/* get the entry time */
	if ((ret = init(&io, nfname)) < 0)
		return(ret);
	if (allow(&io, READOK) == 0) {
		if (seqon == NOSEQ) {	/* if sequencing, jus skip the thing */
			printf("You aren't allowed to read %s\n", nfname);
			if (io.descr.d_plcy) {
				prompt("Do you wish to see the policy note? ");
				if (askyn() == 'y') {
					/* show the policy */
					getnrec(&io, 0, &note);
					dspnote(&io, &note, 0);
				}
			}
			if (allow(&io, WRITOK)) {
				printf("You may leave a note in the notefile\n");
				prompt("Do you wish to leave a note? ");
				if (askyn() == 'y') {
					addnote(&io, NULL, "Edit Note Text:",
						"Note title: ");
					printf("Your note has been registered\n");
				}
			} else
				wfchar();
		}
		finish(&io);
		return(-1);
	}

	if ((io.descr.d_stat & OPEN) == 0)
		readnum = QUITBAD;

	if (io.descr.d_stat & OPEN || allow (&io, DRCTOK)) {
		getname(&whoami, 0);		/* grab his name */
		getlast(&io.stime, nfname, seqon, whoami.aname);
		/* find out last time he was here */
		firstdis = io.descr.d_nnote - nindex + 1;
		respnum = 0;			/* make sure go to base note */

		/*
		 * if sequencer is off, stime is jan 1 1970,
		 * so will enter notefile
		 */
		if (inorder(&io.descr.d_lastm, &io.stime) &&
		    (seqon == NORMSEQ || seqon == INDXSEQ)) {
			finish(&io);
			return(-1);
		}
		if (seqon == NORMSEQ || seqon == EXTSEQ) {
			if ((readnum = nxtnote(&io, 0, &io.stime)) > 0)
				goto seqenter;
		}
		while (1) {
			readnum = indx(&io, &firstdis, &lastdis, &respnum);
			if (readnum < -1)
				break;
	seqenter:
			/* if sequencer is on, we start here */
			readnum = readem(&io, readnum, &firstdis, respnum);
			if (readnum < -1)
				break;
		}
		if (readnum == QUITSEQ || readnum == QUITUPD)
			/* update his access list */
			fixlast(&entered, nfname, seqon, whoami.aname);
		/* COULD CHECK FOR FAST TERMINATION HERE AND LONGJMP */
	}
	finish(&io);
	return(readnum);		/* return the termination flag */
}
