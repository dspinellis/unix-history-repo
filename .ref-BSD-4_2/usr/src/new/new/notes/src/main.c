static char *sccsid = "%W%";

/*
 *	this  main program will take in a notefile name, and
 *	then proceed to handle all of the processing for it. This includes
 *	calling all of the appropriate routines. It should stay in the 
 *	package pretty much as is. It may not be the master routine, but
 *	it will be the chief 'driver' while within a particular note.
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	June, 1981.
 *	Modified more:	Ray Essick	May, 1982.
 *	Modified even more:  Rick L Spickelmier   UCB  1982
 *	Modified even more:  Lou Salkind   	  NYU  1983
 *
 *	This program is code-sharable. 
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "parms.h"
#include "structs.h"
#include "globs.h"		/* everything that belongs in a mainline */

char *rindex();

main (argc, argv)
int argc;
char  **argv;
{

	int i;
	int autoseq = 0;
	int notesargs = 0;
	char *p;
	char cmdline[CMDLEN];

#include "main.i"			/* common init code and such */

	if (globuid == ANONUID) {
		printf(
"Sorry, you have the wrong uid for notesfiles, talk to the notes guru\n"
		);
		exit (BAD);
	}

	/* find out if we are autoseq or not */
	if (p = rindex(argv[0], '/'))
		p++;
	else
		p = argv[0];
	if (strcmp(p, AUTOSEQ) == 0)
		autoseq++;

	/*
	 * Get Various Environmental Variables:
	 * PAGER, SHELL, EDITOR, MAILER, WRITE
	 */
	if ((mypager = getenv("PAGER")) == NULL)
		mypager = PAGER;
	if ((myshell = getenv("SHELL")) == NULL)
		myshell = SHELL;
	if ((myeditor = getenv("NFED")) == NULL) {
		if ((myeditor = getenv("EDITOR")) == NULL)
			myeditor = EDITOR;
	}
	if ((mymailer = getenv("MAILER")) == NULL)
	    mymailer = MAILER;
	if ((mywrite = getenv("WRITE")) == NULL)
	    mywrite = WRITE;
	myhome = getenv("HOME");
	myterm = getenv("TERM");

	if (autoseq)
	    setseq(NORMSEQ);

	/*
	 * COMMAND LINE DECODER
	 */
	for (i = 1; i < argc; i++)
	if (argv[i][0] == '-') {		/* option */
		switch (argv[i][1]) {
		case 's': 			/* -S-equencer */
		case 'x': 			/* e-X-tended sequencer */
		case 'i': 			/* indexing sequencer */
		case 'n': 			/* -N-o sequencer */
			expand(argv[i]);
			break;

		case 't': 			/* overiding tty type */
			/* CHECK AGAINST ARGC HERE */
			myterm = argv[++i];
			break;

		case 'f': 			/* -f file option */
			/* CHECK AGAINST ARGC HERE */
			notesrc = argv[++i];
			break;

		default: 
			printf("Bad switch `%c'\n", argv[i][1]);
			cleanup(BAD);
		}
	} else {
		/* PROCESS FILE ARGUMENT */
		expand(argv[i]);
		notesargs++;
	}

	if (myterm == NULL) {
		printf("Please set your TERM environmental variable\n");
		printf("or specify [-t ttytype] as a command line option\n");
		exit(BAD);
	}

	/*
	 * CHECK FOR NOTESRC FILE
	 */
	if (notesrc) {
		if (readrc(notesrc)) {
			perror(notesrc);
			cleanup(BAD);
		}
	} else if (notesargs == 0) {
		sprintf(cmdline, "%s/%s", myhome? myhome : ".", NOTESRC);
		notesrc = strsave(cmdline);
		if (readrc(notesrc) < 0) {
			/*
			 * the user didn't specify a notesfile on the command
			 * and doesn't have a default notesrc file.  Error.
			 */
			printf(
	"Usage: %s [-s] [-t ttytype] [-f file] topic [...]\n", argv[0]);
			wfchar();
			sprintf(cmdline, "%s/%s/%s", MSTDIR, UTILITY, AVAILHLP);
			dounix(1, 1, mypager, cmdline, 0, 0, 0);
			cleanup(BAD);
		}
	}

	if (cursget() < 0) {
		printf("Unknown terminal type: %s\n", myterm);
		exit(BAD);
	}
	if (setjmp(jenv))
		cleanup(GOOD);
	catchem();
	ttystrt();				/* CBREAK mode */

	/*
	 * Arguments all processed.  Read the notesfiles.
	 */
	for (i = 0; i < last_group; i++) {
		if (group[i].lookat == 0)
			continue;
		switch (control(group[i].name, group[i].seqtyp)) {
		case QUITFAST:
		case QUITUPD:
			cleanup(GOOD);
		case QUITBAD:
			break;
		case QUITNEX:
			printf("No such notesfile `%s'\n", group[i].name);
			break;
		case -1:
			printf("%s...", group[i].name);
			fflush(stdout);
			break;
		default:
			at(0, 1);
			clear_eol();
			break;
		}
	}
	cleanup(GOOD);
}

cleanup(Status)
{
	/* at(0, 1); */
	putchar('\n');
	ttystop();				/* back to normal mode */
	exit(Status);
}
