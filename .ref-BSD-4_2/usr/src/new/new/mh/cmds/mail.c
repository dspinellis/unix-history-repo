#include "mh.h"
#include <stdio.h>
#include <signal.h>

#define NUMTOS  10              /* Number of to's & cc's accepted */

char    tmpfil[32];
int     exitstat = 1;
char    *subject, *cc, *body;

struct swit switches[] = {
	"body",         0,      /* 0 */
	"cc",           0,      /* 1 */
	"subject",      0,      /* 2 */
	"help",         4,      /* 3 */
	0,              0
};

main(argc, argv)
int argc;
char *argv[];
{
	register FILE *out;
	register int i, cnt;
	register char *cp;
	int pid, status, top, ccp, sig(), somebody = 0;
	char buf[BUFSIZ], *tos[NUMTOS], *ccs[NUMTOS], **argp;

	top = 0;
	ccp = -1;               /* -1 -> collecting TOs */
	if(argc == 1) {         /* Just call inc to read mail */
		execl("/bin/inc", "inc", 0);
		execl("/usr/bin/inc", "inc", 0);
		perror("inc");
		done(exitstat);
	}
	argp = argv + 1;
	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);  /* ambiguous */
				done(exitstat);
							/* unknown */
			case -1:fprintf(stderr, "send: -%s unknown\n", cp);
				done(exitstat);
							/* -body */
			case 0: if((body = *argp++) == 0) {
		     missing:           fprintf(stderr, "Mail: Missing %s arg\n", cp);
					done(exitstat);
				}
				continue;
							/* -cc */
			case 1: ccp = 0;                /* Now collecting ccs */
				continue;
							/* -subject */
			case 2: if((subject = *argp++) == 0)
					goto missing;
				continue;
							/* -help */
			case 3: help("mail [switches] users ...",
				     switches);
				done(0);
			}
		else {
			if(ccp >= 0) {                  /* If getting ccs..*/
				if(ccp < NUMTOS)
					ccs[ccp++] = cp;
				else {
					fprintf(stderr, "Mail: Too many ccs\n");
					done(exitstat);
				}
			} else {                        /* Else, to */
				if(top < NUMTOS)
					tos[top++] = cp;
				else {
					fprintf(stderr, "Mail: Too many tos\n");
					done(exitstat);
				}
			}
		}
	}
					/* Create a mail temp file */
	sprintf(tmpfil, "/tmp/%s", makename("mail", ".tmp"));
	if((out = fopen(tmpfil, "w")) == NULL) {
		perror(tmpfil);
		done(exitstat);
	}
	signal(SIGINT, sig);            /* Clean up if user <del>s out */
	fprintf(out, "To: ");           /* Create to list */
	for(i = 0; i < top;) {
		fprintf(out, "%s", tos[i]);
		if(++i < top)
			fprintf(out, ", ");
	}
	fprintf(out, "\n");
	if(ccp > 0) {
		fprintf(out, "Cc: ");   /* Create cc list if needed */
		for(i = 0; i < ccp;) {
			fprintf(out, "%s", ccs[i]);
			if(++i < ccp)
				fprintf(out, ", ");
		}
		fprintf(out, "\n");
	}
	if(subject)                     /* Create subject if needed */
		fprintf(out, "Subject: %s\n", subject);

	fprintf(out, "\n");

	if(body) {                      /* Use body if I have it, */
		somebody++;
		fprintf(out, "%s\n", body);
	} else {                        /* Otherwise, get a body */
		while((cnt = read(0, buf, sizeof buf)) > 0) {
			somebody++;
			if(!fwrite(buf, cnt, 1, out)) {
				perror(tmpfil);
				sig();
			}
		}
	}

	if(ferror(out)) {               /* Check that all wrote well */
		fprintf(stderr, "Error writing tmp file\n");
		sig();
	}
	fclose(out);
	if(!somebody)                   /* If NO body, then don't send */
		sig();                  /*  To be compatible with BELL mail */
	while((i = fork()) == -1) {     /* Now, deliver the mail */
		fprintf(stderr, "Waiting for a fork...\n");
		sleep(2);
	}
	if(i == 0) {                    /* Call deliverer in child */
		execl(mh_deliver, mh_deliver, tmpfil, 0);
		perror(mh_deliver);
		done(exitstat);
	}
	signal(SIGINT, SIG_IGN);
	while((pid = wait(&status)) != -1 && pid != i) ;
	if(status) {                    /* And save mail if delivery failed */
		signal(SIGINT, SIG_DFL);
		fprintf(stderr, "Letter saved in dead.letter\n");
		execl("/bin/mv", "mv", tmpfil, "dead.letter", 0);
		execl("/usr/bin/mv", "mv", tmpfil, "dead.letter", 0);
		perror("/bin/mv");
		done(exitstat);
	}
	exitstat = 0;
	sig();
}

sig()
{
	unlink(tmpfil);
	done(exitstat);
}
