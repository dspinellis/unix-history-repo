#ifndef lint
static	char *sccsid = "@(#)pl_1.c	1.9 83/07/20";
#endif

#include "player.h"
#include <sys/types.h>
#include <wait.h>

int choke(), child();

/*ARGSUSED*/
main(argc, argv)
int argc;
char **argv;
{
	register struct ship *sp;
	int aheadfirst, ma;
	int ta;
	char nodrive = 0, randomize = 0, debug = 0;
	char *badstring();
	extern char _sobuf[];

	setbuf(stdout, _sobuf);

	while (*++argv && **argv == '-')
		switch (*++*argv) {
		case 'd':
			nodrive = 1;
			break;
		case 'D':
			debug++;
			break;
		case 'x':
			randomize = 1;
			break;
		default:
			printf("Unknown flag '%s'\n",*argv);
			break;
		}
	if (*argv)
		game = atoi(*argv);
	else
		game = -1;
	initialize(nodrive, randomize, debug);
	Signal("Aye aye, Sir", (struct ship *)0);
	for (;;) {
		prompt();
		switch (sgetch(0)) {
		case 'm':
			if (mc->crew3 && !snagged(ms)
			    && windspeed != 0) {
				ta = maxturns(ms);
				aheadfirst = ta & 0100000;
				ma = maxmove(ms, mf->dir, 0);
				ta &= 077777;
				acceptmove(ma, ta, aheadfirst);
			} else
				Signal("Unable to move", (struct ship *)0);
			break;
		case 's':
			acceptsignal();
			break;
		case 'g':
			grapungrap();
			break;
		case 'u':
			unfoulplayer();
			break;
		case 'v':
			Signal("%s", (struct ship *)0, version);
			break;
		case 'b':
			doboarding();
			break;
		case 'f':
			acceptcombat();
			break;
		case 'l':
			loadplayer();
			break;
		case 'c':
			changesail();
			break;
		case 'r':
			repair();
			break;
		case 'B':
			Signal("'Hands to stations!'", (struct ship *)0);
			unboard(ms, ms, 1);	/* cancel DBP's */
			unboard(ms, ms, 0);	/* cancel offense */
			break;
		case '\f':
			centerview();
			board();
			screen();
			break;
		case 'L':
			mf->loadL = L_EMPTY;
			mf->loadR = L_EMPTY;
			mf->readyL = R_EMPTY;
			mf->readyR = R_EMPTY;
			Signal("Broadsides unloaded", (struct ship *)0);
			break;
		case 'q':
			Signal("Type 'Q' to quit", (struct ship *)0);
			break;
		case 'Q':
			leave(LEAVE_QUIT);
			break;
		case 'I':
			foreachship(sp)
				eyeball(sp);
			break;
		case 'i':
			eyeball(closestenemy(ms, 0, 1));
			break;
		case 'C':
			centerview();
			draw_view();
			break;
		case 'U':
			upview();
			draw_view();
			break;
		case 'D':
		case 'N':
			downview();
			draw_view();
			break;
		case 'H':
			leftview();
			draw_view();
			break;
		case 'J':
			rightview();
			draw_view();
			break;
		case 'F':
			lookout();
			break;
		case 'S':
			dont_adjust = !dont_adjust;
			break;
		}
		signalflags();
		lost();
	}
}

initialize(nodriver, randomize, debug)
char randomize, nodriver, debug;
{
	register struct File *fp;
	register struct ship *sp;
	char captain[80], file[25];
	char message[60];
	int load;
	int people = 0;
	register int n;
	char *nameptr;
	int nat[NNATION];

	(void) srand(getpid());

	if (game < 0) {
		(void) puts("Choose a scenario:\n");
		(void) puts("\n\tNUMBER\tSHIPS\tIN PLAY\tTITLE");
		for (n = 0; n < NSCENE; n++) {
			printf("\t%d):\t%d", n, scene[n].vessels);
			(void) sprintf(file, "/tmp/.%d", n);
			if (access(file, 0) >= 0)
				printf("\tYES");
			else
				printf("\tno");
			printf("\t%s\n", scene[n].name);
		}
reprint:
		printf("\nScenario number? ");
		(void) fflush(stdout);
		(void) scanf("%d", &game);
		while (getchar() != '\n')
			;
	}
	if (game < 0 || game >= NSCENE) {
		(void) puts("Very funny.");
		exit(1);
	}
	cc = &scene[game];
	ls = cc->ship + cc->vessels;

	(void) sprintf(file, "/tmp/.%d", game);
	if (access(file, 0) < 0) {
		int omask;
#ifdef SETUID
		omask = umask(077);
#else
		omask = umask(011);
#endif
		syncfile = fopen(file, "w+");
		(void) umask(omask);
	} else {
		syncfile = fopen(file, "r+");
		people = 1;
	}
	lastsync = 0;

	for (n = 0; n < NNATION; n++)
		nat[n] = 0;
	foreachship(sp) {
		sp->file = (struct File *) calloc(1, sizeof (struct File));
		if (sp->file == NULL) {
			(void) puts("OUT OF MEMORY");
			exit(0);
		}
		sp->file->stern = nat[sp->nationality]++;
	}
	if (people > 0) {
		(void) puts("Synchronizing with the other players...");
		(void) fflush(stdout);
		Sync();
		foreachship(sp) {
			if (sp->file->captain[0]
			    || sp->file->struck || sp->file->captured != 0)
				break;
		}
		if (sp >= ls) {
			(void) puts("All ships taken in that scenario.");
			foreachship(sp)
				free((char *)sp->file);
			people = 0;
			(void) fclose(syncfile);
			goto reprint;
		}
		player = sp - cc->ship;
	} else
		player = 0;

	while (randomize) {
		printf("%s\n\n", cc->name);
		foreachship(sp) {
			printf("  %2d:  %-10s %-15s  (%-2d pts)   %s\n",
				sp - SHIP(0),
				countryname[sp->nationality],
				sp->shipname,
				sp->specs->pts,
				saywhat(sp, 1));
		}
		printf("\nWhich ship do you want (0-%d)? ", cc->vessels-1);
		(void) fflush(stdout);
		if (scanf("%d", &player) != 1 || player < 0
		    || player >= cc->vessels) {
			while (getchar() != '\n')
				;
			(void) puts("Say what?");
		} else {
			while (getchar() != '\n')
				;
			Sync();
			fp = SHIP(player)->file;
			if (fp->captain[0] || fp->struck || fp->captured != 0)
				(void) puts("Sorry, that ship is taken.");
			else
				break;
		}
	}

	ms = SHIP(player);
	mf = ms->file;
	mc = ms->specs;

	(void) signal(SIGHUP, choke);
	(void) signal(SIGINT, choke);
	(void) signal(SIGQUIT, choke);
	(void) signal(SIGCHLD, child);

	Write(W_CAPTAIN, ms, 1, (int) "begin", 0, 0, 0);
	if (people)
		Write(W_PEOPLE, SHIP(0), 0, cc->people + 1, 0, 0, 0);
	Sync();
	printf("Your ship is the %s, a %d gun %s (%s crew).\n",
		ms->shipname, mc->guns, classname[mc->class],
		qualname[mc->qual]);
	if ((nameptr = (char *) getenv("SAILNAME")) && *nameptr)
		(void) strncpy(captain, nameptr, sizeof captain);
	else {
		(void) printf("Your name, Captain? ");
		(void) fflush(stdout);
		(void) gets(captain);
		if (!*captain)
			(void) strcpy(captain, "no name");
	}
	captain[sizeof captain - 1] = '\0';
	for (n = 0; n < 2; n++) {
		printf("\nInitial broadside %s (grape, chain, round, double): ",
			n ? "right" : "left");
		(void) fflush(stdout);
		(void) scanf("%s", file);
		switch (*file) {
		case 'g':
			load = L_GRAPE;
			break;
		case 'c':
			load = L_CHAIN;
			break;
		case 'r':
			load = L_ROUND;
			break;
		case 'd':
			load = L_DOUBLE;
			break;
		default:
			load = L_ROUND;
		}
		if (n) {
			mf->loadR = load;
			mf->readyR = R_LOADED|R_INITIAL;
		} else {
			mf->loadL = load;
			mf->readyL = R_LOADED|R_INITIAL;
		}
	}
	Write(W_CAPTAIN, ms, 1, (int)captain, 0, 0, 0);
	if (!people && !nodriver) {
		char num[10];
		(void) sprintf(num, "%d", game);
		if (!fork()) {
			if (debug)
				execl(DEBUGDRIVER, DRIVERNAME, num, 0);
			else
				execl(DRIVER, DRIVERNAME, num, 0);
			perror(DRIVER);
			exit(1);
		}
	}

	initscreen();

	board();
	(void) sprintf(message, "Captain %s assuming command", captain);
	Write(W_SIGNAL, ms, 1, (int)message, 0, 0, 0);

	newturn();
}

leave(conditions)
int conditions;
{
	FILE *fp;
	int people;
	float net;
	char * capn;
	char message[60];
	register int n;
	struct logs log[10], temp;

	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGALRM, SIG_IGN);
	(void) signal(SIGCHLD, SIG_IGN);

	if (conditions != -1) {
		capn = mf->captain;
		(void) sprintf(message,"Captain %s relinquishing.",capn);
		Write(W_SIGNAL, ms, 1, (int)message, 0, 0, 0);

		if (fp = fopen(LOGFILE, "r+")) {
			net = (float)mf->points / mc->pts;
			people = getw(fp);
			n = fread((char *)log, sizeof(struct logs), 10, fp);
			for (; n < 10; n++)
				log[n].l_name[0]
					= log[n].l_uid
					= log[n].l_shipnum
					= log[n].l_gamenum
					= log[n].l_netpoints = 0;
			rewind(fp);
			if (people < 0)
				(void) putw(1, fp);
			else
				(void) putw(people + 1, fp);
			for (n = 0; n < 10; n++)
				if (net > (float) log[n].l_netpoints / scene[log[n].l_gamenum].ship[log[n].l_shipnum].specs->pts) {
					(void) fwrite((char *)log,
						sizeof (struct logs), n, fp);
					(void) strcpy(temp.l_name, capn);
					temp.l_uid = getuid();
					temp.l_shipnum = player;
					temp.l_gamenum = game;
					temp.l_netpoints = mf->points;
					(void) fwrite((char *)&temp,
						sizeof temp, 1, fp);
					(void) fwrite((char *)&log[n],
						sizeof (struct logs), 9-n, fp);
					break;
				}
			(void) fclose(fp);
		}
		Write(W_CAPTAIN, ms, 1, (int)" ", 0, 0, 0);
		Write(W_PEOPLE, SHIP(0), 0, cc->people - 1, 0, 0, 0);
		if (done_curses) {
			screen();
			Signal("It looks like you've had it!",
				(struct ship *)0);
			switch (conditions) {
			case LEAVE_QUIT:
				break;
			case LEAVE_CAPTURED:
				Signal("Your ship was captured.",
					(struct ship *)0);
				break;
			case LEAVE_HURRICAN:
				Signal("Hurricane!  All ships destroyed.",
					(struct ship *)0);
				break;
			case LEAVE_DRIVER:
				Signal("The driver died.", (struct ship *)0);
				break;
			default:
				Signal("A funny thing happened (%d).",
					(struct ship *)0, conditions);
			}
		} else {
			if (conditions == LEAVE_DRIVER)
				printf("The driver died.\n");
			else
				printf("leave: unknown code %d\n", conditions);
		}
		(void) fclose(syncfile);
	}
	if (done_curses) {
		lastline();
		nocrmode();
		echo();
		endwin();
	}
	exit(0);
}

choke()
{
	leave(LEAVE_QUIT);
}

child()
{
	union wait status;
	int pid;

	(void) signal(SIGCHLD, SIG_IGN);
	do {
		pid = wait3(&status, WNOHANG|WUNTRACED, (struct rusage *)0);
		if (pid < 0 || pid > 0 && !WIFSTOPPED(status))
			leave(LEAVE_DRIVER);
	} while (pid != 0);
	(void) signal(SIGCHLD, child);
}
