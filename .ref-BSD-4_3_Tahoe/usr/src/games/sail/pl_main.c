/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)pl_main.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include "player.h"
#include <sys/types.h>
#include <sys/wait.h>

int choke(), child();

/*ARGSUSED*/
pl_main()
{

	if (!SCREENTEST()) {
		printf("Can't sail on this terminal.\n");
		exit(1);
	}
	initialize();
	Signal("Aye aye, Sir", (struct ship *)0);
	play();
	return 0;			/* for lint,  play() never returns */
}

initialize()
{
	register struct File *fp;
	register struct ship *sp;
	char captain[80];
	char message[60];
	int load;
	register int n;
	char *nameptr;
	int nat[NNATION];

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
		if (sp->file == NULL &&
		    (sp->file = (struct File *)calloc(1, sizeof (struct File))) == NULL) {
			(void) puts("OUT OF MEMORY");
			exit(1);
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

	hasdriver = sync_exists(game);
	if (sync_open() < 0) {
		perror("sail: syncfile");
		exit(1);
	}

	if (hasdriver) {
		(void) puts("Synchronizing with the other players...");
		(void) fflush(stdout);
		if (Sync() < 0)
			leave(LEAVE_SYNC);
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
		if (Sync() < 0)
			leave(LEAVE_SYNC);
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
	if (Sync() < 0)
		leave(LEAVE_SYNC);

	(void) signal(SIGCHLD, child);
	if (!hasdriver)
		switch (fork()) {
		case 0:
			longjmp(restart, MODE_DRIVER);
			/*NOTREACHED*/
		case -1:
			perror("fork");
			leave(LEAVE_FORK);
			break;
		default:
			hasdriver++;
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
	draw_board();
	(void) sprintf(message, "Captain %s assuming command", captain);
	Write(W_SIGNAL, ms, 1, (int)message, 0, 0, 0);
	newturn();
}
