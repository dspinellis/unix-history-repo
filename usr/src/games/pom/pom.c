/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software posted to USENET.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pom.c	5.2 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Phase of the Moon.  Calculates the current phase of the moon.
 * Based on routines from `Practical Astronomy with Your Calculator',
 * by Duffett-Smith.  Comments give the section from the book that
 * particular piece of code was adapted from.
 *
 * -- Keith E. Brandt  VIII 1984
 *
 */

#include <sys/time.h>
#include <stdio.h>
#include <tzfile.h>
#include <math.h>

#define	PI	  3.141592654
#define	EPOCH	  85
#define	EPSILONg  279.611371	/* solar ecliptic long at EPOCH */
#define	RHOg	  282.680403	/* solar ecliptic long of perigee at EPOCH */
#define	ECCEN	  0.01671542	/* solar orbit eccentricity */
#define	lzero	  18.251907	/* lunar mean long at EPOCH */
#define	Pzero	  192.917585	/* lunar mean long of perigee at EPOCH */
#define	Nzero	  55.204723	/* lunar mean long of node at EPOCH */

main()
{
	extern int errno;
	struct timeval tp;
	struct timezone tzp;
	struct tm *GMT, *gmtime();
	double days, today, tomorrow, dtor(), adj360(), potm();
	int cnt;
	char *strerror();

	if (gettimeofday(&tp,&tzp)) {
		(void)fprintf(stderr, "pom: %s\n", strerror(errno));
		exit(1);
	}
	GMT = gmtime(&tp.tv_sec);
	days = (GMT->tm_yday + 1) + ((GMT->tm_hour +
	    (GMT->tm_min / 60.0) + (GMT->tm_sec / 3600.0)) / 24.0);
	for (cnt = EPOCH; cnt < GMT->tm_year; ++cnt)
		days += isleap(cnt) ? 366 : 365;
	today = potm(days) + .5;
	(void)printf("The Moon is ");
	if ((int)today == 100)
		(void)printf("Full\n");
	else if (!(int)today)
		(void)printf("New\n");
	else {
		tomorrow = potm(days + 1);
		if ((int)today == 50)
			(void)printf("%s\n", tomorrow > today ?
			    "at the First Quarter" : "at the Last Quarter");
		else {
			(void)printf("%s ", tomorrow > today ?
			    "Waxing" : "Waning");
			if (today > 50)
				(void)printf("Gibbous (%1.0f%% of Full)\n",
				    today);
			else if (today < 50)
				(void)printf("Crescent (%1.0f%% of Full)\n",
				    today);
		}
	}
}

/*
 * potm --
 *	return phase of the moon
 */
double
potm(days)
	double days;
{
	double N, Msol, Ec, LambdaSol, l, Mm, Ev, Ac, A3, Mmprime;
	double A4, lprime, V, ldprime, D, Nm;

	N = 360 * days / 365.2422;				/* sec 42 #3 */
	adj360(&N);
	Msol = N + EPSILONg - RHOg;				/* sec 42 #4 */
	adj360(&Msol);
	Ec = 360 / PI * ECCEN * sin(dtor(Msol));		/* sec 42 #5 */
	LambdaSol = N + Ec + EPSILONg;				/* sec 42 #6 */
	adj360(&LambdaSol);
	l = 13.1763966 * days + lzero;				/* sec 61 #4 */
	adj360(&l);
	Mm = l - (0.1114041 * days) - Pzero;			/* sec 61 #5 */
	adj360(&Mm);
	Nm = Nzero - (0.0529539 * days);			/* sec 61 #6 */
	adj360(&Nm);
	Ev = 1.2739 * sin(dtor(2*(l - LambdaSol) - Mm));	/* sec 61 #7 */
	Ac = 0.1858 * sin(dtor(Msol));				/* sec 61 #8 */
	A3 = 0.37 * sin(dtor(Msol));
	Mmprime = Mm + Ev - Ac - A3;				/* sec 61 #9 */
	Ec = 6.2886 * sin(dtor(Mmprime));			/* sec 61 #10 */
	A4 = 0.214 * sin(dtor(2 * Mmprime));			/* sec 61 #11 */
	lprime = l + Ev + Ec - Ac + A4;				/* sec 61 #12 */
	V = 0.6583 * sin(dtor(2 * (lprime - LambdaSol)));	/* sec 61 #13 */
	ldprime = lprime + V;					/* sec 61 #14 */
	D = ldprime - LambdaSol;				/* sec 63 #2 */
	return(50 * (1 - cos(dtor(D))));			/* sec 63 #3 */
}

/*
 * dtor --
 *	convert degrees to radians
 */
double
dtor(deg)
	double deg;
{
	return(deg * PI / 180);
}

/*
 * adj360 --
 *	adjust value so 0 <= deg <= 360
 */
double
adj360(deg)
	double *deg;
{
	for (;;)
		if (*deg < 0)
			*deg += 360;
		else if (*deg > 360)
			*deg -= 360;
		else
			break;
}
