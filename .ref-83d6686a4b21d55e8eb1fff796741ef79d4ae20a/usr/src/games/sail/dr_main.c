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
static char sccsid[] = "@(#)dr_main.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include "driver.h"

dr_main()
{
	register int n;
	register struct ship *sp;
	int nat[NNATION];
	int value = 0;

	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTSTP, SIG_IGN);
	if (issetuid)
		(void) setruid(geteuid());
	if (game < 0 || game >= NSCENE) {
		fprintf(stderr, "DRIVER: Bad game number %d\n", game);
		exit(1);
	}
	cc = &scene[game];
	ls = SHIP(cc->vessels);
	if (sync_open() < 0) {
		perror("driver: syncfile");
		exit(1);
	}
	for (n = 0; n < NNATION; n++)
		nat[n] = 0;
	foreachship(sp) {
		if (sp->file == NULL &&
		    (sp->file = (struct File *)calloc(1, sizeof (struct File))) == NULL) {
			(void) fprintf(stderr, "DRIVER: Out of memory.\n");
			exit(1);
		}
		sp->file->index = sp - SHIP(0);
		sp->file->loadL = L_ROUND;
		sp->file->loadR = L_ROUND;
		sp->file->readyR = R_LOADED|R_INITIAL;
		sp->file->readyL = R_LOADED|R_INITIAL;
		sp->file->stern = nat[sp->nationality]++;
		sp->file->dir = sp->shipdir;
		sp->file->row = sp->shiprow;
		sp->file->col = sp->shipcol;
	}
	windspeed = cc->windspeed;
	winddir = cc->winddir;
	people = 0;
	for (;;) {
		sleep(7);
		if (Sync() < 0) {
			value = 1;
			break;
		}
		if (next() < 0)
			break;
		unfoul();
		checkup();
		prizecheck();
		moveall();
		thinkofgrapples();
		boardcomp();
		compcombat();
		resolve();
		reload();
		checksails();
		if (Sync() < 0) {
			value = 1;
			break;
		}
	}
	sync_close(1);
	return value;
}
