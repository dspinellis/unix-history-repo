/* $Header: acttimes.c,v 1.1 91/11/02 18:38:29 davison Exp $
**
** $Log:	acttimes.c,v $
*/

/* This program will maintain the file active.times if your news software
** doesn't already do this for you.  The file contains a list of newsgroup
** names followed by the time of creation (in seconds since 1970) and the
** address of the creator.  Since we can't tell who actually created the
** group, they will all indicate acttimes@DOMAIN.
**
** Place this file in your NNTP support directory and change your
** common/conf.h to undef NGDATE_FILE and STAT_FILE, and define
** ACTIVE_TIMES_FILE.  To make, you can replace every mention of
** "mkgrdates" in the support/Makefile with "acttimes" and then
** type "make".  
**
** To use this without having NNTP around, undefine the NNTP_SUPPORT
** define, edit the other defines that follow to indicate your setup,
** and compile it with something like "cc -O -o acttimes acttimes.c".
**
** Use either "acttimes -d" to start a daemon process that wakes up every
** 10 minutes (by default) to check if the active file is a different
** size, or put "acttimes" into your cron file to be run periodically.
*/

#define NNTP_SUPPORT		/* comment out if not using NNTP */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#ifdef NNTP_SUPPORT
#include "../common/conf.h"
#endif
#ifdef USG
#include <time.h>
#else
#include <sys/time.h>
#endif

/* ---------- Start of configuration defines ---------- */

#ifndef USG
#define TERMIO			/* Is this is termio system? */
#endif

/* NNTP sites have the following already defined in ../common/conf.h */

#ifndef DOMAIN			/* our domain name */
#define DOMAIN	"local"
#endif

#ifndef ACTIVE_FILE		/* the active file for your news system */
#define ACTIVE_FILE "/usr/lib/news/active"
#endif

#ifndef ACTIVE_TIMES_FILE	/* the name of the file to update */
#define ACTIVE_TIMES_FILE "/usr/lib/news/active.times"
#endif

#ifndef SIGRET			/* set this to "int" if you have problems */
#define SIGRET void
#endif

#ifndef MAXPATHLEN		/* you'll probably want to leave this alone */
#define	MAXPATHLEN	1024
#endif

/*#define index   strchr	/* uncomment these if you need them */
/*#define rindex  strrchr	/* (i.e. if index is undefined) */

/* ---------- End of configuration defines ---------- */

#ifdef TERMIO
#include <termio.h>
#else
#include <sgtty.h>
#endif

#define TIMER_FIRST 1
#define TIMER_DEFAULT (10 * 60)

#define strnEQ(x,y,n) (!strncmp((x),(y),(n)))

extern errno;

char *index(), *rindex(), *malloc();
SIGRET alarm_handler(), quit_handler();
void active_times(), free_lines(), wrap_it_up();

typedef struct _active_line {
    struct _active_line *link;
    char *name;
    char type;
} ACTIVE_LINE;

ACTIVE_LINE *line_root = NULL, *last_line = NULL, *pline = NULL;
long last_actsize;
int daemon_delay = 0, kill_daemon = 0, old_groups = 0;

FILE *fp_lock;

struct stat filestat;

char buf[MAXPATHLEN];
char lockfile[MAXPATHLEN];

main(argc, argv)
int argc;
char *argv[];
{
    int fd;
    long pid;
    char *cp;

    while (--argc) {
	if (**++argv == '-') {
	    while (*++*argv) {
		switch (**argv) {
		case 'd':		/* run in daemon mode */
		    if (*++*argv <= '9' && **argv >= '0') {
			daemon_delay = atoi(*argv) * 60;
			while (*++*argv <= '9' && **argv >= '0') {
			    ;
			}
		    } else {
			daemon_delay = TIMER_DEFAULT;
		    }
		    --*argv;
		    break;
		case 'k':		/* kill running acttimes */
		    kill_daemon++;
		    break;
		default:
		    fprintf(stderr, "Unknown option: '%c'\n", **argv);
		    exit(1);
		}
	    }
	} else {
	    fprintf(stderr,
		"Usage:  acttimes [-d<mins>]\nOr:     acttimes -k\n");
	    exit(1);
	}
    }

    /* Set up a nice friendly umask. */
    umask(002);

    /* Make sure we're not already running by creating a lock file. */
    strcpy(lockfile, ACTIVE_TIMES_FILE);
    if ((cp = rindex(lockfile, '/')) != 0) {
	cp++;
    } else {
	cp = lockfile;
    }
    *cp = '\0';
    sprintf(buf, "%sLOCK.%ld", lockfile, (long)getpid());
    if ((fp_lock = fopen(buf, "w")) == 0) {
	fprintf(stderr, "Unable to create lock temporary `%s'.\n", buf);
	exit(1);
    }
    fprintf(fp_lock, "%ld\n", (long)getpid());
    fclose(fp_lock);

    /* Try to link to lock file. */
    strcat(lockfile, "LOCKacttimes");
  dolink:
    if (link(buf, lockfile) < 0) {
      long otherpid;
	/* Try to avoid possible race with daemon starting up. */
	sleep (5);
	if ((fp_lock = fopen(lockfile, "r")) == 0) {
	    fprintf(stderr, "unable to open %s\n", lockfile);
	    unlink(buf);
	    exit(1);
	}
	if (fscanf(fp_lock, "%ld", &otherpid) != 1) { 
	    fprintf(stderr, "unable to read pid from %s\n", lockfile);
	    unlink(buf);
	    fclose(fp_lock);
	    exit(1);
	}
	fclose(fp_lock);
	if (kill(otherpid, kill_daemon ? SIGTERM : 0) == -1
	 && errno == ESRCH) {
	    if (unlink(lockfile) == -1) {
		fprintf(stderr, "unable to unlink lockfile %s\n", lockfile);
		unlink(buf);
		exit(1);
	    }
	    if (!kill_daemon) {
		goto dolink;
	    }
	}
	unlink(buf);
	if (kill_daemon) {
	    fprintf(stderr, "killing currently running acttimes.\n");
	    exit(0);
	} else {
	    fprintf(stderr, "acttimes is already running.\n");
	    exit(1);
	}
    }

    unlink(buf);			/* remove temporary LOCK.<pid> file */

    if (kill_daemon) {
	fprintf(stderr, "acttimes is not running.\n");
	wrap_it_up(1);
    }

#ifdef SIGHUP
    if( signal( SIGHUP, SIG_IGN ) != SIG_IGN ) {
	signal( SIGHUP, quit_handler );
    }
#endif
    if( signal( SIGINT, SIG_IGN ) != SIG_IGN ) {
	signal( SIGINT, quit_handler );
    }
#ifdef SIGQUIT
    if( signal( SIGQUIT, SIG_IGN ) != SIG_IGN ) {
	signal( SIGQUIT, quit_handler );
    }
#endif
    signal( SIGTERM, quit_handler );
#ifdef SIGTTIN
    signal( SIGTTIN, SIG_IGN );
    signal( SIGTTOU, SIG_IGN );
#endif
    signal( SIGALRM, SIG_IGN );

    /* If we're not in daemon mode, run through once and quit. */
    if (!daemon_delay) {
	active_times();
    } else {
	/* For daemon mode, we cut ourself off from anything tty-related and
	** run in the background (involves forks, but no knives).
	*/
	close(0);
	if (open("/dev/null", 2) != 0) {
	    fprintf(stderr, "unable to open /dev/null!\n");
	    wrap_it_up(1);
	}
	close(1);
	close(2);
	dup(0);
	dup(0);
	while ((pid = fork()) < 0) {
	    sleep(2);
	}
	if (pid) {
	    exit(0);
	}
#ifdef TIOCNOTTY
	if ((fd = open("/dev/tty", 1)) >= 0) {
	    ioctl(fd, TIOCNOTTY, (int*)0);
	    close(fd);
	}
#else
	(void) setpgrp();
	while ((pid = fork()) < 0) {
	    sleep(2);
	}
	if (pid) {
	    exit(0);
	}
#endif
	/* Put our pid in the lock file for death detection */
	if( (fp_lock = fopen(lockfile, "w")) != 0) {
	    fprintf(fp_lock, "%ld\n", (long)getpid());
	    fclose(fp_lock);
	}

	signal(SIGALRM, alarm_handler);

	/* Start timer -- first interval is shorter than all others */
	alarm(TIMER_FIRST);
	for (;;) {
	    pause();		/* let alarm go off */
	    alarm(0);

	    if (stat(ACTIVE_FILE, &filestat) < 0) {
		fprintf(stderr, "Unable to stat active file -- quitting.\n");
		wrap_it_up(1);
	    }
	    if (filestat.st_size != last_actsize) {
		last_actsize = filestat.st_size;
		active_times();
	    }
	    alarm(daemon_delay);
	} /* for */
    }/* if */

    wrap_it_up(0);
}

SIGRET
alarm_handler()
{
    signal(SIGALRM, alarm_handler);
}

SIGRET
quit_handler()
{
    wrap_it_up(0);
}

void
wrap_it_up(ret)
{
    unlink(lockfile);
    exit(ret);
}

void
active_times()
{
    FILE *fp_active, *fp_date_r, *fp_date_w;
    register char *cp;

    if ((fp_active = fopen(ACTIVE_FILE, "r")) == NULL) {
	if (!daemon_delay) {
	    fprintf(stderr, "Unable to open active file.\n");
	}
	return;
    }
    if ((fp_date_r = fopen(ACTIVE_TIMES_FILE, "r")) == NULL) {
	if (!daemon_delay) {
	    fprintf(stderr, "Creating active.times file.\n");
	}
	old_groups = 1;
    } else {
	old_groups = 0;
    }
    sprintf(buf, "%s.n", ACTIVE_TIMES_FILE);
    if ((fp_date_w = fopen(buf, "w")) == NULL) {
	if (!daemon_delay) {
	    fprintf(stderr, "Unable to create active.times file.\n");
	}
	return;
    }

    /* Loop through entire active file and remember each line. */
    while (fgets(buf, sizeof buf, fp_active)) {
	if (!(cp = index(buf, ' ')) || cp[1] == '\0') {
	    continue;
	}
	cp[1] = '\0';		/* include trailing space */
	if (!(cp = rindex(cp + 2, ' '))) {
	    continue;
	}
	pline = (ACTIVE_LINE*)malloc(sizeof (ACTIVE_LINE));
	if (!pline) {
	    if (line_root) {
		last_line->link = NULL;
		free_lines();
	    }
	  bug_out:
	    fclose(fp_active);
	    fclose(fp_date_r);
	    fclose(fp_date_w);
	    return;
	}
	pline->name = malloc(strlen(buf) + 1);
	if (!pline->name) {
	    if (line_root) {
		last_line->link = NULL;
		pline->name = NULL;
		free_lines();
	    } else {
		free(pline);
	    }
	    goto bug_out;
	}
	strcpy(pline->name, buf);
	pline->type = cp[1];
	if (!last_line) {
	    line_root = pline;
	} else {
	    last_line->link = pline;
	}
	last_line = pline;
    }
    last_line->link = NULL;
    fclose(fp_active);

    if (fp_date_r) {
	/* Loop through date file, copying existing groups to new file. */
	while (fgets(buf, sizeof buf, fp_date_r)) {
	    last_line = NULL;
	    for (pline = line_root; pline; pline = pline->link) {
		if (strnEQ(buf, pline->name, strlen(pline->name))) {
		    fputs(buf, fp_date_w);
		    free(pline->name);
		    if (last_line) {
			last_line->link = pline->link;
		    } else {
			line_root = pline->link;
		    }
		    free(pline);
		    break;
		}
		last_line = pline;
	    }/* for */
	}
    }
    /* Remaining entries from active file are new groups. */
    for (pline = line_root; pline; pline = last_line) {
	if (pline->type != 'x' && pline->type != '=') {
	    /* If it's not 'x'ed out, write it out with the current time. */
	    fprintf(fp_date_w, "%s%ld acttimes@%s\n", pline->name,
		old_groups ? 30010440L : time(NULL), DOMAIN);
	}
	free(pline->name);
	last_line = pline->link;
	free(pline);
    }
    fclose(fp_date_w);
    fclose(fp_date_r);
    line_root = NULL;

    /* rm active.times.o */
    sprintf(buf,"%s.o", ACTIVE_TIMES_FILE);
    unlink(buf);
    /* mv active.times active.times.o */
    link(ACTIVE_TIMES_FILE, buf);
    unlink(ACTIVE_TIMES_FILE);
    /* mv active.times.n active.times */
    sprintf(buf, "%s.n", ACTIVE_TIMES_FILE);
    link(buf, ACTIVE_TIMES_FILE);
    unlink(buf);
}

void
free_lines()
{
    for (pline = line_root; pline; pline = last_line) {
	if (pline->name) {
	    free(pline->name);
	}
	last_line = pline->link;
	free(pline);
    }
    line_root = NULL;
}
