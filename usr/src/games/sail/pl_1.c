#ifndef lint
static	char *sccsid = "@(#)pl_1.c	2.6 84/01/27";
#endif

#include "player.h"
#include <sys/types.h>
#include <sys/wait.h>

int choke(), child();

static char hasdriver;

/*ARGSUSED*/
main(argc, argv)
int argc;
char **argv;
{
	char nodrive = 0, randomize = 0, debug = 0;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
	isplayer = 1;

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
	play();
}

initialize(nodriver, randomize, debug)
char randomize, nodriver, debug;
{
	register struct File *fp;
	register struct ship *sp;
	char captain[80];
	char message[60];
	int load;
	register int n;
	char *nameptr;
	int nat[NNATION];

	(void) srand(getpid());

	if (game < 0) {
		(void) puts("Choose a scenario:\n");
		(void) puts("\n\tNUMBER\tSHIPS\tIN PLAY\tTITLE");
		for (n = 0; n < NSCENE; n++) {
			/* ( */
			printf("\t%d):\t%d\t%s\t%s\n", n, scene[n].vessels,
				sync_exists(n) ? "YES" : "no",
				scene[n].name);
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
	ls = SHIP(cc->vessels);

	for (n = 0; n < NNATION; n++)
		nat[n] = 0;
	foreachship(sp) {
		sp->file = (struct File *) calloc(1, sizeof (struct File));
		if (sp->file == NULL) {
			(void) puts("OUT OF MEMORY");
			exit(0);
		}
		sp->file->index = sp - SHIP(0);
		sp->file->stern = nat[sp->nationality]++;
		sp->file->dir = sp->shipdir;
		sp->file->row = sp->shiprow;
		sp->file->col = sp->shipcol;
	}
	windspeed = cc->windspeed;
	winddir = cc->winddir;

	(void) signal(SIGHUP, choke);
	(void) signal(SIGINT, choke);
	(void) signal(SIGQUIT, choke);

	hasdriver = sync_exists(game);
	if (sync_open() < 0) {
		perror("sail: syncfile");
		exit(1);
	}

	if (hasdriver) {
		(void) puts("Synchronizing with the other players...");
		(void) fflush(stdout);
		Sync();
	}
	for (;;) {
		foreachship(sp)
			if (sp->file->captain[0] == 0 && !sp->file->struck
			    && sp->file->captured == 0)
				break;
		if (sp >= ls) {
			(void) puts("All ships taken in that scenario.");
			foreachship(sp)
				free((char *)sp->file);
			sync_close(0);
			people = 0;
			goto reprint;
		}
		if (randomize) {
			player = sp - SHIP(0);
		} else {
			printf("%s\n\n", cc->name);
			foreachship(sp)
				printf("  %2d:  %-10s %-15s  (%-2d pts)   %s\n",
					sp->file->index,
					countryname[sp->nationality],
					sp->shipname,
					sp->specs->pts,
					saywhat(sp, 1));
			printf("\nWhich ship (0-%d)? ", cc->vessels-1);
			(void) fflush(stdout);
			if (scanf("%d", &player) != 1 || player < 0
			    || player >= cc->vessels) {
				while (getchar() != '\n')
					;
				(void) puts("Say what?");
				player = -1;
			} else
				while (getchar() != '\n')
					;
		}
		if (player < 0)
			continue;
		Sync();
		fp = SHIP(player)->file;
		if (fp->captain[0] || fp->struck || fp->captured != 0)
			(void) puts("That ship is taken.");
		else
			break;
	}

	ms = SHIP(player);
	mf = ms->file;
	mc = ms->specs;

	Write(W_BEGIN, ms, 0, 0, 0, 0, 0);
	Sync();

	(void) signal(SIGCHLD, child);
	if (!hasdriver && !nodriver) {
		char num[10];
		(void) sprintf(num, "%d", game);
		switch (fork()) {
		case 0:
			execl(DRIVER1, DRIVERNAME, num, 0);
			execl(DRIVER2, DRIVERNAME, num, 0);
			execl(DRIVER3, DRIVERNAME, num, 0);
			perror(DRIVERNAME);
			exit(1);
			break;
		case -1:
			perror("fork");
			leave(LEAVE_QUIT);
			break;
		default:
			hasdriver++;
		}
	}

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
	Write(W_CAPTAIN, ms, 1, (int)captain, 0, 0, 0);
	for (n = 0; n < 2; n++) {
		char buf[10];

		printf("\nInitial broadside %s (grape, chain, round, double): ",
			n ? "right" : "left");
		(void) fflush(stdout);
		(void) scanf("%s", buf);
		switch (*buf) {
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

	initscreen();
	blockalarm();
	draw_board();
	unblockalarm();
	(void) sprintf(message, "Captain %s assuming command", captain);
	Write(W_SIGNAL, ms, 1, (int)message, 0, 0, 0);
	newturn();
}

/*
 * If we get here before a ship is chosen, then ms == 0 and
 * we don't want to update the score file, or do any Write's either.
 * We can assume the sync file is already created and may need
 * to be removed.
 */
leave(conditions)
int conditions;
{
	FILE *fp;
	int persons;
	float net;
	register int n;
	struct logs log[10], temp;

	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGALRM, SIG_IGN);
	(void) signal(SIGCHLD, SIG_IGN);

	if (done_curses) {
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
			/* don't clear 'hasdriver' here */
			Signal("The driver died.", (struct ship *)0);
			break;
		default:
			Signal("A funny thing happened (%d).",
				(struct ship *)0, conditions);
		}
	} else {
		switch (conditions) {
		case LEAVE_QUIT:
			break;
		case LEAVE_DRIVER:
			printf("The driver died.\n");
			break;
		case LEAVE_FORK:
			hasdriver = 0;
			printf("Can't fork.\n");
			break;
		default:
			printf("A funny thing happened (%d).\n",
				conditions);
		}
	}

	if (ms != 0) {
		if (fp = fopen(LOGFILE, "r+")) {
			net = (float)mf->points / mc->pts;
			persons = getw(fp);
			n = fread((char *)log, sizeof(struct logs), 10, fp);
			for (; n < 10; n++)
				log[n].l_name[0]
					= log[n].l_uid
					= log[n].l_shipnum
					= log[n].l_gamenum
					= log[n].l_netpoints = 0;
			rewind(fp);
			if (persons < 0)
				(void) putw(1, fp);
			else
				(void) putw(persons + 1, fp);
			for (n = 0; n < 10; n++)
				if (net > (float) log[n].l_netpoints / scene[log[n].l_gamenum].ship[log[n].l_shipnum].specs->pts) {
					(void) fwrite((char *)log,
						sizeof (struct logs), n, fp);
					(void) strcpy(temp.l_name, mf->captain);
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
		makesignal(ms, "Captain %s relinquishing.", (struct ship *)0,
			mf->captain);
		Write(W_END, ms, 0, 0, 0, 0, 0);
		Sync();
	}
	sync_close(!hasdriver);
	cleanupscreen();
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
