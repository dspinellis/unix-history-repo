#ifndef lint
static char	*sccsid = "@(#)mkgrdates.c	1.1	(Berkeley) 5/9/86";
#endif

/*
 * Make a list of newsgroups which are "new" in the file NGDATE_FILE.
 * "New" means having an entry in the active file, and having
 * a message numbered "1" in the appropriate news directory.
 * Since this involves a stat of all the newsgroups, we try
 * to be intelligent about things -- if the active file's size
 * since we last ran -- stored in STAT_FILE -- hasn't changed
 * since last time, we assume things are ok, and exit without
 * doing anything.  This could fail in extreme circumstances,
 * but is "too painful to do right".
 *
 * Output in NGDATE_FILE is of the form
 *
 *	date newsgroup
 *
 * where "date" is the date the newsgroup was created, expressed as
 * the number of seconds since 000000 Jan 1, 1970, GMT.  This file
 * winds up sorted in cronological order.
 *
 *	Phil Lapsley
 *	College of Engineering
 *	University of California, Berkeley
 *	(ARPA: phil@Berkeley.ARPA; UUCP: ...!ucbvax!phil)
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include "../common/conf.h"

#define MODE		0644	/* Better be readable by nntpd! */

extern	int	linecmp();
extern	char	*index(), *malloc(), *strcpy(), *strcat();

main(argc, argv)
int	argc;
char	*argv[];
{
	char	*groups[MAX_GROUPS];
	char	line[MAX_STRLEN], gr_name[MAX_STRLEN];
	char	*cp;
	int	i, j;
	long	lastsize, crntsize;
	long	birthtime;
	struct tm *tmptr;
	FILE	*stat_fp, *active_fp, *date_fp;
	long	birthof();

	stat_fp = fopen(STAT_FILE, "r");

	if (stat_fp != NULL) {
		(void) fscanf(stat_fp, "%d", &lastsize);
		(void) fclose(stat_fp);
	}

	active_fp = fopen(ACTIVE_FILE, "r");
	if (active_fp == NULL) {
		fprintf(stderr, "Can't read active file?\n");
		perror(ACTIVE_FILE);
		exit(1);
	}

	/* Check length; if it's the same as last time, quit */

	(void) fseek(active_fp, (long) 0, 2);
	crntsize = ftell(active_fp);
	if (crntsize == lastsize) {
		(void) fclose(active_fp);
		exit(0);
	}

	/* Ok, time to rebuild the date file */

	date_fp = fopen(NGDATE_FILE, "w");

	if (date_fp == NULL) {
		perror(NGDATE_FILE);
		(void) fclose(active_fp);
		exit(1);
	}

	rewind(active_fp);

	i = 0;
	while (fgets(line, sizeof(line), active_fp) != NULL) {
		if ((cp = index(line, ' ')) != NULL)
			*cp = '\0';
		(void) strcpy(gr_name, line);
		birthtime = birthof(line, atoi(cp + 1));
		
		if (birthtime == 0)	/* Skip ancient newsgroups */
			continue;

		(void) sprintf(line, "%ld %s", birthtime, gr_name);
		groups[i] = malloc(strlen(line)+1);
		if (groups[i] != NULL)
			(void) strcpy(groups[i++], line);
		else {
			perror("malloc");
			exit(1);
		}
	}

	(void) fclose(active_fp);

	qsort((char *) groups, i, sizeof(char *), linecmp);

	for (j = 0; j < i; ++j)
		fprintf(date_fp, "%s\n", groups[j]);

	(void) fclose(date_fp);

	(void) chmod(NGDATE_FILE, MODE);

	stat_fp = fopen(STAT_FILE, "w");
	if (stat_fp == NULL) {
		perror(STAT_FILE);
		exit(1);
	}

	fprintf(stat_fp, "%d\n", crntsize);

	(void) fclose(stat_fp);

	(void) chmod(STAT_FILE, MODE);

	exit(0);
}

linecmp(line1, line2)
char	 **line1, **line2;
{
	return(0 - strcmp(*line1, *line2));
}


/* return creation time of newsgroup */

long
birthof(group, highart)
char	*group;
int	highart;
{
	char	*cp, *index();
	char	tst[128];
	struct stat statbuf;
 
	while ((cp = index(group, '.')))
		*cp = '/';

	(void) strcpy(tst, SPOOLDIR);
	(void) strcat(tst, group);
	if (highart)
		(void) strcat(tst, "/1");
	if (stat(tst, &statbuf) < 0)
		return 0L;		/* not there, assume ancient */
	else
		return(statbuf.st_mtime);
}
