/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ns_stats.c	4.10 (Berkeley) 6/27/90";
#endif /* not lint */

/**************************************************************************/
/*                simple monitoring of named behavior                     */
/*            dumps a bunch of values into a well-know file               */
/*                                                                        */
/**************************************************************************/

#ifdef STATS

#include <sys/param.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <stdio.h>
#include <syslog.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "pathnames.h"

#ifdef STATSFILE
char *statsfile = STATSFILE;
#else
char *statsfile = _PATH_STATS;
#endif /* STATSFILE */

extern	time_t	boottime, resettime;
extern	int	needStatsDump;

/*
 * General statistics gathered
 */
/* The position in this table must agree with the defines in ns.h */
struct stats stats[S_NSTATS] = {
	{ 0, "input packets" },
	{ 0, "output packets" },
	{ 0, "queries" },
	{ 0, "iqueries" },
	{ 0, "duplicate queries" },
	{ 0, "responses" },
	{ 0, "duplicate responses" },
	{ 0, "OK answers" },
	{ 0, "FAIL answers" },
	{ 0, "FORMERR answers" },
	{ 0, "system queries" },
	{ 0, "prime cache calls" },
	{ 0, "check_ns calls" },
	{ 0, "bad responses dropped" },
	{ 0, "martian responses" },
};

/*
 *  Statistics for queries (by type)
 */
unsigned long typestats[T_ANY+1];
char *typenames[T_ANY+1] = {
	/* 5 types per line */
	"Unknown", "A", "NS", "invalid(MD)", "invalid(MF)",
	"CNAME", "SOA", "MB", "MG", "MR",
	"NULL", "WKS", "PTR", "HINFO", "MINFO",
	"MX", "TXT", 0, 0, 0,
	/* 20 per line */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 100 */
	"UINFO", "UID", "GID", "UNSPEC", 0, 0, 0, 0, 0, 0,
	/* 110 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 120 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 200 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 240 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 250 */
	0, 0, "AXFR", "MAILB", "MAILA", "ANY" 
};

ns_stats()
{
	time_t timenow;
	register FILE *f;
	register int i;

	if ((f = fopen(statsfile,"a")) == 0)
	{
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"can't open stat file, \"%s\"\n",statsfile);
#endif
		syslog(LOG_ERR, "cannot open stat file, \"%s\"\n",statsfile);
		return;
	}

	time(&timenow);
	fprintf(f, "###  %s", ctime(&timenow));
	fprintf(f, "%d\ttime since boot (secs)\n", timenow - boottime);
	fprintf(f, "%d\ttime since reset (secs)\n", timenow - resettime);

	/* general statistics */
	for (i = 0; i < S_NSTATS; i++)
		fprintf(f,"%lu\t%s\n", stats[i].cnt, stats[i].description);

	/* query type statistics */
	fprintf(f, "%d\tUnknown query types\n", typestats[0]);
	for(i=1; i < T_ANY+1; i++)
		if (typestats[i])
			if (typenames[i])
				fprintf(f, "%lu\t%s queries\n", typestats[i],
					typenames[i]);
			else
				fprintf(f, "%lu\ttype %d queries\n",
					typestats[i], i);
	(void) fclose(f);
}
#endif STATS
