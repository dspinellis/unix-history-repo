#ifndef lint
static char sccsid[] = "@(#)send.c	4.6 7/7/83";
#endif

#include "mh.h"
#include <sys/wait.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include <signal.h>
#include <strings.h>

#define	SENDMAIL	"/usr/lib/sendmail"
#define FCCS    	10      	    /* Max number of fcc's allowed */

struct swit switches[] = {
	"debug",         -1,      /* 0 */
	"draft",	  0,	  /* 1 */
	"format",         0,      /* 2 */
	"noformat",       0,      /* 3 */
	"msgid",          0,      /* 4 */
	"nomsgid",        0,      /* 5 */
	"verbose",        0,      /* 6 */
	"noverbose",      0,      /* 7 */
	"help",           4,      /* 8 */
	"wait",		  0,      /* 9 */
	"nowait",	  0,      /* 10 */
	0,                0
};

int     verbose, format, msgid, debug, myuid;
int	waitfor = 1;
char    *anoyes[];       		/* Std no/yes gans array */
int     donecd;
long    now;
char    tmpfil[32], fccfold[FCCS][128];
int     fccind;
char	*rindex();

main(argc, argv)
char *argv[];
{
	register char *drft, *cp, *addrs;
	register int i;
	int pid;
	struct stat stbuf;
	char **ap, *msg;
	char *arguments[50], **argp;
	char buf[BUFSIZ], name[NAMESZ];
	int state, compnum, fd;
	FILE *in, *out;
	char *getname();

#ifdef NEWS
	m_news();
#endif
	drft = 0;
	cp = r1bindex(argv[0], '/');
	if ((cp = m_find(cp)) != NULL) {
		ap = brkstring(cp = getcpy(cp), " ", "\n");
		ap = copyip(ap, arguments);
	} else
		ap = arguments;
	copyip(argv+1, ap);
	argp = arguments;

	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);       /* ambiguous */
				done(1);
							/* unknown */
			case -1:fprintf(stderr, "send: -%s unknown\n", cp);
				done(1);
			case 0: verbose++; debug++; continue; /* -debug */
			case 1: drft = m_maildir(draft);      /* -draft */
				continue;
			case 2: fprintf(stderr, "Send: -format not yet.\n");
				done(1);
			case 3: format = 0; continue;         /* -noformat */
			case 4: msgid = 1;  continue;         /* -msgid */
			case 5: msgid = 0;  continue;         /* -nomsgid */
			case 6: verbose = 1; continue;        /* -verbose */
			case 7: verbose = 0; continue;        /* -noverbose */
			case 8: help("send [file] [switches]", switches);
				done(1);
			case 9: waitfor = 1;  continue;         /* -wait */
			case 10:waitfor = 0;  continue;         /* -nowait */
			}
		if(drft) {
			fprintf(stderr, "send: Only 1 message at a time!\n");
			done(1);
		} else
			drft = cp;
	}
	if(!drft) {
		drft = m_maildir(draft);
		if(stat(drft, &stbuf) == -1) {
			fprintf(stderr,
				"Draft file: %s doesn't exist.\n", drft);
			done(1);
		}
		cp = concat("Use \"", drft, "\"? ", 0);
		if(!gans(cp, anoyes))
			done(0);
	} else {
		if(stat(drft, &stbuf) == -1) {
			fprintf(stderr,
				"Draft file: %s doesn't exist.\n", drft);
			done(1);
		}
	}
	m_update();

	/* open files... */
	if((in = fopen(drft, "r")) == NULL) {
		fprintf(stderr, "Send: Can't open ");
		perror(drft);
		done(1);
	}
	copy(makename("locs", ".tmp"), copy("/tmp/", tmpfil));
	if(!debug) {
		if((out = fopen(tmpfil, "w")) == NULL) {
			fprintf(stderr, "Can't create %s\n", tmpfil);
			done(1);
		}
		chmod(tmpfil, 0744);
	} else
		out = stdout;

	myuid = getuid();
	putdate(0, out);           /* Tack on the date */
	/* putfrom(out);*/	   /* let sendmail do this */
	putmsgid(out);             /* and message id if desired */

	/*
	** Delivery phase
	*/
	for(compnum = 1, state = FLD;;) {
		state = m_getfld(state, name, buf, sizeof buf, in);
		switch (state)
		{
		case FLD:
		case FLDEOF:
		case FLDPLUS:
			compnum++;
			cp = buf;
			while(*cp == ' ' || *cp == '\t')
				cp++;
			fprintf(out, "%s: %s", name, cp);
			if(uleq(name, "fcc")) {
				if(fccind >= FCCS) {
					fprintf(stderr, "Send: too many fcc's.\n");
					done(1);
				}
				copy(cp, fccfold[fccind]);
				if(cp = rindex(fccfold[fccind], '\n'))
					*cp = 0;
				fccind++;
			}
			while(state == FLDPLUS || state == FLDEOF) {
				state = m_getfld(state, name, buf, sizeof buf, in);
				fputs(buf, out);
			}
			if(state == FLDEOF)
				goto process;
			break;

		case BODY:
		case BODYEOF:
			fprintf(out, "\n%s", buf);
			while(state == BODY) {
				state=m_getfld(state,name,buf,sizeof buf,in);
				fputs(buf, out);
			}

		case FILEEOF:
			goto process;

		case LENERR:
		case FMTERR:
			fprintf(stderr, "??Message Format Error ");
			fprintf(stderr, "in Component #%d.\n", compnum);
			done(1);

		default:
			fprintf(stderr, "Getfld returned %d\n", state);
			done(1);
		}
	}
process:
	if(!debug)
		fclose(out);
	else
		printf("-----\n");
	fclose(in);

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	if(!debug) {                    /* Send the mail */
		fd = open(tmpfil, 0);
		sendmail(fd);
		close(fd);
		if(fccind) {
			for(state = 0; state < fccind; state++)
				fcc(tmpfil, fccfold[state]);
		}
		unlink(tmpfil);
	}
	if (!debug)
		backup(drft);
	done(donecd);
}

sendmail(fd)
{
	int pid;
	char *stashname[10];
	char **stashp;

	stashp = stashname;
	*stashp++ = "sendmail";
	*stashp++ = "-i";
	*stashp++ = "-t";
	if (verbose)
		*stashp++ = "-v";
	*stashp = 0;

	while ((pid = fork()) == -1)
		sleep(2);
	if (pid == 0) {
		close(0);
		dup(fd);
		close(fd);
		execv(SENDMAIL, stashname);
		perror(SENDMAIL);
		_exit(1);
	}
	else if (waitfor)
		(void) wait(0);
}

putfrom(out)
	register FILE *out;
{
	register struct passwd *pw;

	pw = getpwuid(myuid);
	if (pw == NULL)
	{
		fprintf(stderr, "Send: WHO ARE YOU?\n");
		done(1);
	}
	fprintf(out, "From: %s\n", pw->pw_name);
}

putmsgid(sp)
	FILE *sp;
{
	char hostname[32];
	auto int i = sizeof hostname;

	if(!msgid)
		return;
	if(!now)
		time(&now);
	gethostname(hostname, &i);
	fprintf(sp,
		"Message-Id: <%u.%u.%ld@%s>\n", getpid(), myuid, now, hostname);
}

fcc(file, folder)
	char *file, *folder;
{
	int child, pid;
	union wait status;
	char fold[128];

	if(verbose) {
		printf("Fcc: %s...", folder);
		fflush(stdout);
	}
	while ((child = fork()) == -1)
		sleep(5);
	if(child == 0) {
		if(*file != '+')
			strcpy(fold, "+");
		strcat(fold, folder);
		execl(fileproc, "file", "-link", "-file", file, fold, 0);
		exit(-1);
	} else
		while((pid = wait(&status)) != -1 && pid != child);
	if (status.w_status)
		fprintf(stderr, "Send: Error on fcc to %s\n", folder);
	else if(verbose)
		putchar('\n');
}

backup(file)
char *file;
{
	char buf[128];
	register char *cp;

	buf[0] = 0;
	if(cp = rindex(file, '/'))
		sprintf(buf, "%.*s", (++cp)-file, file);
	else
		cp = file;
#ifdef UCB
	strcat(buf, "#");
#else
	strcat(buf, ",");
#endif
	strcat(buf, cp);
	unlink(buf);
	if(link(file, buf) < 0 || unlink(file) < 0) {
		fprintf(stderr, "Send: Backup rename failure ");
		perror(buf);
		done(1);
	}
}
