/*
 * Copyright (c) 1980 Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)leave.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
/*
 * leave [[+]hhmm]
 *
 * Reminds you when you have to leave.
 * Leave prompts for input and goes away if you hit return.
 * It nags you like a mother hen.
 */
char origlogin[20];
char *getlogin();
char *whenleave;
char *ctime();
char buff[100];

main(argc, argv)
char **argv;
{
	long when, now, diff, hours, minutes;
	char *cp;
	int *nv;
	int gethm();
	int *localtime();

	if ((cp = getlogin()) == NULL) {
		fputs("leave: You are not logged in.\n", stderr);
		exit(1);
	}
	strcpy(origlogin, cp);
	if (argc < 2) {
		printf("When do you have to leave? ");
		fflush(stdout);
		buff[read(0, buff, sizeof buff)] = 0;
		cp = buff;
	} else
		cp = argv[1];
	if (*cp == '\n')
		exit(0);
	if (*cp == '+') {
		cp++;
		if (!gethm(cp, &hours, &minutes))
			usage();
		if (minutes < 0 || minutes > 59)
			usage();
		diff = 60*hours+minutes;
		doalarm(diff);
		exit(0);
	}
	if (!gethm(cp, &hours, &minutes))
		usage();
	if (hours > 12)
		hours -= 12;
	if (hours == 12)
		hours = 0;

	if (hours < 0 || hours > 12 || minutes < 0 || minutes > 59)
		usage();

	time(&now);
	nv = localtime(&now);
	when = 60*hours+minutes;
	if (nv[2] > 12)
		nv[2] -= 12;	/* do am/pm bit */
	now = 60*nv[2] + nv[1];
	diff = when - now;
	while (diff < 0)
		diff += 12*60;
	if (diff > 11*60) {
		fprintf(stderr, "That time has already passed!\n");
		exit(1);
	}
	doalarm(diff);
	exit(0);
}

usage()
{
	fprintf(stderr, "usage: leave [[+]hhmm]\n");
	exit(1);
}

int
gethm(cp, hp, mp)
register char *cp;
int *hp, *mp;
{
	register char c;
	register int tod;

	tod = 0;
	while ((c = *cp++) != '\0') {
		if (!isdigit(c))
			return(0);
		tod = tod * 10 + (c - '0');
	}
	*hp = tod / 100;
	*mp = tod % 100;
	return(1);
}

doalarm(nmins)
long nmins;
{
	char *msg1, *msg2, *msg3, *msg4;
	register int i;
	int slp1, slp2, slp3, slp4;
	int seconds, gseconds;
	long daytime;

	seconds = 60 * nmins;
	if (seconds <= 0)
		seconds = 1;
	gseconds = seconds;

	msg1 = "You have to leave in 5 minutes";
	if (seconds <= 60*5) {
		slp1 = 0;
	} else {
		slp1 = seconds - 60*5;
		seconds = 60*5;
	}

	msg2 = "Just one more minute!";
	if (seconds <= 60) {
		slp2 = 0;
	} else {
		slp2 = seconds - 60;
		seconds = 60;
	}

	msg3 = "Time to leave!";
	slp3 = seconds;

	msg4 = "You're going to be late!";
	slp4 = 60;

	time(&daytime);
	daytime += gseconds;
	whenleave = ctime(&daytime);
	printf("Alarm set for %s", whenleave);
	if (fork())
		exit(0);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);

	if (slp1)
		bother(slp1, msg1);
	if (slp2)
		bother(slp2, msg2);
	bother(slp3, msg3);
	for (i = 0; i < 10; i++)
		bother(slp4, msg4);
	printf("That was the last time I'll tell you. Bye.\n");
	exit(0);
}

bother(slp, msg)
int slp;
char *msg;
{

	delay(slp);
	printf("\7\7\7%s\n", msg);
}

/*
 * delay is like sleep but does it in 100 sec pieces and
 * knows what zero means.
 */
delay(secs)
int secs;
{
	int n;
	register char *l;

	while (secs > 0) {
		n = 100;
		if (secs < n)
			n = secs;
		secs -= n;
		if (n > 0)
			sleep(n);
		l = getlogin();
		if (l == NULL)
			exit(0);
		if (strcmp(origlogin, l) != 0)
			exit(0);
	}
}

#ifdef V6
char *getlogin() {
#include <utmp.h>

	static struct utmp ubuf;
	int ufd;

	ufd = open("/etc/utmp",0);
	seek(ufd, ttyn(0)*sizeof(ubuf), 0);
	read(ufd, &ubuf, sizeof(ubuf));
	ubuf.ut_name[sizeof(ubuf.ut_name)] = 0;
	return(&ubuf.ut_name);
}
#endif
