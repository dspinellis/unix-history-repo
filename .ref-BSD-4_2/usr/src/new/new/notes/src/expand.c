static char *sccsid = "%W% %G";

/*
 *	Routines to expand notesgroups specifications
 *
 *	Rick L Spickelmier, UC Berkeley
 *	Lou Salkind, NYU
 */

#include <ctype.h>
#include <signal.h>
#include "structs.h"
#include "parms.h"

static int seqtyp = 0;

/*
 * set the sequencer type (external interface)
 */
setseq(i)
{
	seqtyp = i;
}

/*
 * add a notesfile to the active list
 */
addgrp(string)
char *string;
{
	int i;

	/* see if it already exists */
	for (i = 0; i < last_group; i++) {
		if (strcmpn(group[i].name, string, 14) == 0) {
			group[i].lookat = 1;
			group[i].seqtyp = seqtyp;
			return;
		}
	}
	if (last_group >= MAXGROUPS) {
		printf("addgrp: ignoring %s\n", string);
		return;
	}
	group[last_group].lookat = 1;
	group[last_group].seqtyp = seqtyp;
	group[last_group++].name = strsave(string);
}

/*
 * delete the notesfile from the active list
 */
delete(string)
char *string;
{
	int i;

	for (i = 0; i < last_group; i++) {
		if (strcmpn(group[i].name, string, 14) == 0) { 
			group[i].lookat = 0;
			return;
		}
	}
}

/*
 * given a command line argument, expand it into
 * the appropriate sequence command or notesfile
 * specification
 */
expand(argp)
char *argp;
{
	char *endp;

again:
	while (isspace(*argp))
		argp++;
	if (*argp == '\0')
		return;
	endp = argp;
	while (*endp) {
		if (isspace(*endp) || *endp == ',') {
			*endp++ = '\0';
			break;
		}
		endp++;
	}
	if (argp[0] == '-') {
		/* sequencing options */
		switch (argp[1]) {
		case 's': 		/* -S-equencer */
			seqtyp = NORMSEQ;
			break;
		case 'x': 		/* e-X-tended sequencer */
			seqtyp = EXTSEQ;
			break;
		case 'i': 		/* indexing sequencer */
			seqtyp = INDXSEQ;
			break;
		case 'n': 		/* -N-o sequencer */
			seqtyp = NOSEQ;
			break;
		}
	} else {
		/* newsgroup specification */
		if (argp[0] == '!') {
			/* handle not's */
			if (patcheck(&argp[1]))
				/* wildcard */
				dopat(&argp[1], delete);
			else 
				delete(&argp[1]);
		} else {
			if (patcheck(argp))
				dopat(argp, addgrp);
			else
				addgrp(argp);
		}
	}
	argp = endp;
	goto again;
	/*NOT REACHED*/
}

/*
 * read a file which contains the command line arguments
 */
readrc(s)
char *s;
{
	FILE *f;
	char buf[BUFSIZ];

	if ((f = fopen(s, "r")) == NULL)
		return(-1);
	while (fgets(buf, sizeof buf -1, f))
		expand(buf);
	fclose(f);
	return(0);
}

/*
 * unsubscribe to a notesfile
 */
unsubscribe(nfile)
char *nfile;
{
	int pid, rpid;
	int retstat;
	FILE *fp;
	int (*oint)();
	int (*oquit)();
	int (*ohup)();

	/*
	 * append the notesgroup name to thes
	 * end of the notesrc file
	 */
	if (notesrc == NULL) {
		warn("No notesrc file");
		return(-1);
	}
	/* fork off a child that can write the users notesrc */
	if ((pid = fork()) == 0) {
		x(setuid(globuid) < 0, "readem: setuid/forked");
		if ((fp = fopen(notesrc, "a")) == NULL)
			_exit(1);
		fprintf(fp, "!%s\n", nfile);  /* append a not */
		fclose(fp);
		_exit(0);
	}
	ohup = signal(SIGHUP, SIG_IGN);
	oint = signal(SIGINT, SIG_IGN);
	oquit = signal(SIGQUIT, SIG_IGN);
	/* wait here for the son to finish */
	while ((rpid = wait(&retstat)) != pid && rpid != -1)
		continue;
	signal(SIGHUP, ohup);
	signal(SIGINT, oint);
	signal(SIGQUIT, oquit);
	if (rpid == -1) {
		warn("unsubscribe: error in wait");
		return(-1);
	}
	if (retstat) {
		warn("Can not open %s", notesrc);
		return(-1);
	}
	return(0);
}
