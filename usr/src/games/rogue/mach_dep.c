/*
 * Various installation dependent routines
 *
 * @(#)find.c	4.37 (Berkeley) 5/23/83
 */

/*
 * The various tuneable defines are:
 *
 *	SCOREFILE	Where/if the score file should live.
 *	ALLSCORES	Score file is top ten scores, not top ten
 *			players.  This is only useful when only a few
 *			people will be playing; otherwise the score file
 *			gets hogged by just a few people.
 *	NUMSCORES	Number of scores in the score file (default 10).
 *	NUMNAME		String version of NUMSCORES (first character
 *			should be capitalized) (default "Ten").
 *	MAXLOAD		What (if any) the maximum load average should be
 *			when people are playing.  Since it is divided
 *			by 10, to specify a load limit of 4.0, MAXLOAD
 *			should be "40".	 If defined, then
 *		LOADAV		Should it use it's own routine to get
 *				the load average?
 *		NAMELIST	If so, where does the system namelist
 *				hide?
 *	MAXUSERS	What (if any) the maximum user count should be
 *			when people are playing.  If defined, then
 *		UCOUNT		Should it use it's own routine to count
 *				users?
 *		UTMP		If so, where does the user list hide?
 *	CHECKTIME	How often/if it should check during the game
 *			for high load average.
 */

#include <curses.h>
#include "extern.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef SCOREFILE

static char *Lockfile = "/tmp/.fredlock";

# ifndef NUMSCORES
#	define	NUMSCORES	10
#	define	NUMNAME		"Ten"
# endif

unsigned int numscores = NUMSCORES;
char *Numname = NUMNAME;

# ifdef ALLSCORES
bool Allscore = TRUE;
# else  ALLSCORES
bool Allscore = FALSE;
# endif ALLSCORES

#endif SCOREFILE

#ifdef CHECKTIME
static int Num_checks;		/* times we've gone over in checkout() */
#endif CHECKTIME

/*
 * init_check:
 *	Check out too see if it is proper to play the game now
 */
init_check()
{
#if defined(MAXLOAD) || defined(MAXUSERS)
    if (too_much())
    {
	printf("Sorry, %s, but the system is too loaded now.\n", Whoami);
	printf("Try again later.  Meanwhile, why not enjoy a%s %s?\n",
	    vowelstr(Fruit), Fruit);
	if (author())
	    printf("However, since you're a good guy, it's up to you\n");
	else
	    exit(1);
    }
#endif
}

/*
 * open_score:
 *	Open up the score file for future use, and then
 *	setuid(getuid()) in case we are running setuid.
 */
open_score()
{
#ifdef SCOREFILE
    Fd = open(SCOREFILE, 2);
    if (Fd < 0)
	perror(SCOREFILE);
#else
    Fd = -1;
#endif
    setuid(getuid());
    setgid(getgid());
}

/*
 * setup:
 *	Get starting setup for all games
 */
setup()
{
    int  auto_save(), quit(), endit(), tstp();
#ifdef CHECKTIME
    int  checkout();
#endif

    signal(SIGHUP, auto_save);
#ifndef DUMP
    signal(SIGILL, auto_save);
    signal(SIGTRAP, auto_save);
    signal(SIGIOT, auto_save);
    signal(SIGEMT, auto_save);
    signal(SIGFPE, auto_save);
    signal(SIGBUS, auto_save);
    signal(SIGSEGV, auto_save);
    signal(SIGSYS, auto_save);
    signal(SIGTERM, auto_save);
#endif

    signal(SIGINT, quit);
#ifndef DUMP
    signal(SIGQUIT, endit);
#endif
#ifdef CHECKTIME
    signal(SIGALRM, checkout);
    alarm(CHECKTIME * 60);
    Num_checks = 0;
#endif
    crmode();				/* Cbreak mode */
    noecho();				/* Echo off */
#ifdef TIOCGLTC
    getltchars();			/* get the local tty chars */
#endif
}

/*
 * getltchars:
 *	Get the local tty chars for later use
 */
getltchars()
{
#ifdef TIOCGLTC
    ioctl(1, TIOCGLTC, &Ltc);
    Got_ltc = TRUE;
    Orig_dsusp = Ltc.t_dsuspc;
    Ltc.t_dsuspc = Ltc.t_suspc;
    ioctl(1, TIOCSLTC, &Ltc);
#endif
}

/*
 * start_score:
 *	Start the scoring sequence
 */
start_score()
{
#ifdef CHECKTIME
    signal(SIGALRM, SIG_IGN);
#endif
}

/*
 * symlink:
 *	See if the file has a symbolic link
 */
symlink(sp)
char *sp;
{
#ifdef S_IFLNK
    struct stat sbuf2;

    if (lstat(sp, &sbuf2) < 0)
	return FALSE;
    else
	return ((sbuf2.st_mode & S_IFMT) != S_IFREG);
#else
    return FALSE;
#endif
}

#if defined(MAXLOAD) || defined(MAXUSERS)
/*
 * too_much:
 *	See if the system is being used too much for this game
 */
too_much()
{
#ifdef MAXLOAD
    double avec[3];
#else
    register int cnt;
#endif

#ifdef MAXLOAD
    loadav(avec);
    if (avec[1] > (MAXLOAD / 10.0))
	return TRUE;
#endif
#ifdef MAXUSERS
    if (ucount() > MAXUSERS)
	return TRUE;
#endif
    return FALSE;
}

/*
 * author:
 *	See if a user is an author of the program
 */
author()
{
#ifdef MASTER
    if (Wizard)
	return TRUE;
#endif
    switch (getuid())
    {
	case -1:
	    return TRUE;
	default:
	    return FALSE;
    }
}
#endif

#ifdef CHECKTIME
/*
 * checkout:
 *	Check each CHECKTIME seconds to see if the load is too high
 */
checkout()
{
    static char *msgs[] = {
	"The load is too high to be playing.  Please leave in %0.1f minutes",
	"Please save your game.  You have %0.1f minutes",
	"Last warning.  You have %0.1f minutes to leave",
    };
    int checktime;

    signal(SIGALRM, checkout);
    if (too_much())
    {
	if (author())
	{
	    Num_checks = 1;
	    chmsg("The load is rather high, O exaulted one");
	}
	else if (Num_checks++ == 3)
	    fatal("Sorry.  You took too long.  You are dead\n");
	checktime = (CHECKTIME * 60) / Num_checks;
	alarm(checktime);
	chmsg(msgs[Num_checks - 1], ((double) checktime / 60.0));
    }
    else
    {
	if (Num_checks)
	{
	    Num_checks = 0;
	    chmsg("The load has dropped back down.  You have a reprieve");
	}
	alarm(CHECKTIME * 60);
    }
}

/*
 * chmsg:
 *	checkout()'s version of msg.  If we are in the middle of a
 *	shell, do a printf instead of a msg to avoid the refresh.
 */
/* VARARGS1 */
chmsg(fmt, arg)
char *fmt;
int arg;
{
    if (!In_shell)
	msg(fmt, arg);
    else
    {
	printf(fmt, arg);
	putchar('\n');
	fflush(stdout);
    }
}
#endif

#ifdef LOADAV
/*
 * loadav:
 *	Looking up load average in core (for system where the loadav()
 *	system call isn't defined
 */

#include <nlist.h>

struct nlist avenrun = {
    "_avenrun"
};

loadav(avg)
register double *avg;
{
    register int kmem;

    if ((kmem = open("/dev/kmem", 0)) < 0)
	goto bad;
    nlist(NAMELIST, &avenrun);
    if (avenrun.n_type == 0)
    {
	close(kmem);
bad:
	avg[0] = 0.0;
	avg[1] = 0.0;
	avg[2] = 0.0;
	return;
    }

    lseek(kmem, (long) avenrun.n_value, 0);
    read(kmem, (char *) avg, 3 * sizeof (double));
    close(kmem);
}
#endif

#ifdef UCOUNT
/*
 * ucount:
 *	Count number of users on the system
 */
#include <utmp.h>

struct utmp buf;

ucount()
{
    register struct utmp *up;
    register FILE *utmp;
    register int count;

    if ((utmp = fopen(UTMP, "r")) == NULL)
	return 0;

    up = &buf;
    count = 0;

    while (fread(up, 1, sizeof (*up), utmp) > 0)
	if (buf.ut_name[0] != '\0')
	    count++;
    fclose(utmp);
    return count;
}
#endif

/*
 * lock_sc:
 *	lock the score file.  If it takes too long, ask the user if
 *	they care to wait.  Return TRUE if the lock is successful.
 */
lock_sc()
{
#ifdef SCOREFILE
    register int cnt;
    static struct stat sbuf;
    time_t time();

over:
    close(8);	/* just in case there are no files left */
    if (creat(Lockfile, 0000) >= 0)
	return TRUE;
    for (cnt = 0; cnt < 5; cnt++)
    {
	sleep(1);
	if (creat(Lockfile, 0000) >= 0)
	    return TRUE;
    }
    if (stat(Lockfile, &sbuf) < 0)
    {
	creat(Lockfile, 0000);
	return TRUE;
    }
    if (time(NULL) - sbuf.st_mtime > 10)
    {
	if (unlink(Lockfile) < 0)
	    return FALSE;
	goto over;
    }
    else
    {
	printf("The score file is very busy.  Do you want to wait longer\n");
	printf("for it to become free so your score can get posted?\n");
	printf("If so, type \"y\"\n");
	fgets(Prbuf, MAXSTR, stdin);
	if (Prbuf[0] == 'y')
	    for (;;)
	    {
		if (creat(Lockfile, 0000) >= 0)
		    return TRUE;
		if (stat(Lockfile, &sbuf) < 0)
		{
		    creat(Lockfile, 0000);
		    return TRUE;
		}
		if (time(NULL) - sbuf.st_mtime > 10)
		{
		    if (unlink(Lockfile) < 0)
			return FALSE;
		}
		sleep(1);
	    }
	else
	    return FALSE;
    }
#endif
}

/*
 * unlock_sc:
 *	Unlock the score file
 */
unlock_sc()
{
#ifdef SCOREFILE
    unlink(Lockfile);
#endif
}

/*
 * flush_type:
 *	Flush typeahead for traps, etc.
 */
flush_type()
{
    crmode();
}
