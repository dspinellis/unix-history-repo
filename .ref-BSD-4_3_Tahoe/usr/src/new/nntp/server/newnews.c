#ifndef lint
static char	*sccsid = "@(#)newnews.c	1.16	(Berkeley) 7/17/87";
#endif

#include "common.h"
#include "time.h"

#ifdef LOG
int	nn_told = 0;
int	nn_took = 0;
#endif


/*
 * NEWNEWS newsgroups date time ["GMT"] [<distributions>]
 *
 * Return the message-id's of any news articles past
 * a certain date and time, within the specified distributions.
 *
 */

newnews(argc, argv)
	register int	argc;
	char		*argv[];
{
	register char	*cp, *ngp;
	char		*key;
	char		datebuf[32];
	char		line[MAX_STRLEN];
	char		**distlist, **histlist;
	static char	**nglist;
	int		distcount, ngcount, histcount;
	int		all;
	FILE		*fp;
	long		date;
	long		dtol();
	char		*ltod();

	if (argc < 4) {
		printf("%d Usage: NEWNEWS newsgroups yymmdd hhmmss [\"GMT\"] [<distributions>].\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

#ifdef LOG
	sprintf(line, "%s newnews %s %s %s %s %s",
		hostname,
		argv[1],
		argv[2],
		argv[3],
		(argc >= 5 && *argv[4] == 'G') ? "GMT" : "local",
		(argc >= 5 && *argv[argc-1] == '<') ? argv[argc-1] : "none");
	syslog(LOG_INFO, line);
#endif

	all = streql(argv[1], "*");
	if (!all) {
		ngcount = get_nglist(&nglist, argv[1]);
		if (ngcount == 0) {
			printf("%d Bogus newsgroup specifier: %s\r\n",
				ERR_CMDSYN, argv[1]);
			(void) fflush(stdout);
			return;
		}
	}

	/*	    YYMMDD		    HHMMSS	*/
	if (strlen(argv[2]) != 6 || strlen(argv[3]) != 6) {
		printf("%d Date/time must be in form YYMMDD HHMMSS.\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	(void) strcpy(datebuf, argv[2]);
	(void) strcat(datebuf, argv[3]);

	argc -= 4;
	argv += 4;

	/*
	 * Flame on.  The history file is not stored in GMT, but
	 * in local time.  So we have to convert GMT to local time
	 * if we're given GMT, otherwise we need only chop off the
	 * the seconds.  Such braindamage.
	 */

	key = datebuf;		/* Unless they specify GMT */

	if (argc > 0) {
		if (streql(*argv, "GMT")) {	/* Which we handle here */
			date = dtol(datebuf);
			if (date < 0) {
				printf("%d Invalid date specification.\r\n",
					ERR_CMDSYN);
				(void) fflush(stdout);
				return;
			}
			date = gmt_to_local(date);
			key = ltod(date);
			++argv;
			--argc;
		}
	}

	/* So, key now points to the local time, but we need to zap secs */

	key[10] = '\0';

	distcount = 0;
	if (argc > 0) {
		distcount = get_distlist(&distlist, *argv);
		if (distcount < 0) {
			printf("%d Bad distribution list: %s\r\n", ERR_CMDSYN,
				*argv);
			(void) fflush(stdout);
			return;
		}
	}

	fp = fopen(historyfile, "r");
	if (fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "newnews: fopen %s: %m", historyfile);
#endif
		printf("%d Cannot open history file.\r\n", ERR_FAULT);
		(void) fflush(stdout);
		return;
	}

	printf("%d New news by message id follows.\r\n", OK_NEWNEWS);

	if (seekuntil(fp, key, line, sizeof (line)) < 0) {
		printf(".\r\n");
		(void) fflush(stdout);
		(void) fclose(fp);
		return;
	}

/*
 * History file looks like:
 *
 * <1569@emory.UUCP>	01/22/86 09:19	net.micro.att/899 ucb.general/2545 
 *		     ^--tab            ^--tab		 ^--space         ^sp\0
 * Sometimes the newsgroups are missing; we try to be robust and
 * ignore such bogosity.  We tackle this by our usual parse routine,
 * and break the list of articles in the history file into an argv
 * array with one newsgroup per entry.
 */

	do {
		if ((cp = index(line, '\t')) == NULL)
			continue;

		if ((ngp = index(cp+1, '\t')) == NULL)	/* 2nd tab */
			continue;
		++ngp;			/* Points at newsgroup list */
		if (*ngp == '\n')
			continue;
		histcount = get_histlist(&histlist, ngp);
		if (histcount == 0)
			continue;

		/*
		 * For each newsgroup on this line in the history
		 * file, check it against the newsgroup names we're given.
		 * If it matches, then see if we're hacking distributions.
		 * If so, open the file and match the distribution line.
		 */

		if (!all)
			if (!ngmatch(restreql, 0, nglist, ngcount,
			    histlist, histcount))
				continue;

		if (distcount)
			if (!distmatch(distlist, distcount, histlist, histcount))
				continue;

		*cp = '\0';
		putline(line);
#ifdef LOG
		nn_told++;
#endif
	} while (fgets(line, sizeof(line), fp) != NULL);

	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
	(void) fclose(fp);
}


/*
 * seekuntil -- seek through the history file looking for
 * a line with date "key".  Get that line, and return.
 *
 *	Parameters:	"fp" is the active file.
 *			"key" is the date, in form YYMMDDHHMM (no SS)
 *			"line" is storage for the first line we find.
 *
 *	Returns:	-1 on error, 0 otherwise.
 *
 *	Side effects:	Seeks in history file, modifies line.
 */

seekuntil(fp, key, line, linesize)
	FILE		*fp;
	char		*key;
	char		*line;
	int		linesize;
{
	char		datetime[32];
	register int	c;
	register long	top, bot, mid;

	bot = 0;
	(void) fseek(fp, 0L, 2);
	top = ftell(fp);
	for(;;) {
		mid = (top+bot)/2;
		(void) fseek(fp, mid, 0);
		do {
			c = getc(fp);
			mid++;
		} while (c != EOF && c!='\n');
		if (!getword(fp, datetime, line, linesize)) {
			return (-1);
		}
		switch (compare(key, datetime)) {
		case -2:
		case -1:
		case 0:
			if (top <= mid)
				break;
			top = mid;
			continue;
		case 1:
		case 2:
			bot = mid;
			continue;
		}
		break;
	}
	(void) fseek(fp, bot, 0);
	while(ftell(fp) < top) {
		if (!getword(fp, datetime, line, linesize)) {
			return (-1);
		}
		switch(compare(key, datetime)) {
		case -2:
		case -1:
		case 0:
			break;
		case 1:
		case 2:
			continue;
		}
		break;
	}

	return (0);
}


compare(s, t)
	register char *s, *t;
{
	for (; *s == *t; s++, t++)
		if (*s == 0)
			return(0);
	return (*s == 0 ? -1:
		*t == 0 ? 1:
		*s < *t ? -2:
		2);
}


getword(fp, w, line, linesize)
	FILE		*fp;
	register char	*w;
	char		*line;
	int		linesize;
{
	register char	*cp;

	if (fgets(line, linesize, fp) == NULL)
		return (0);
	if (cp = index(line, '\t')) {
/*
 * The following gross hack is present because the history file date
 * format is braindamaged.  They like "mm/dd/yy hh:mm", which is useless
 * for relative comparisons of dates using something like atoi() or
 * strcmp.  So, this changes their format into yymmddhhmm.  Sigh.
 *
 * 12345678901234	("x" for cp[x])
 * mm/dd/yy hh:mm 	(their lousy representation)
 * yymmddhhmm		(our good one)
 * 0123456789		("x" for w[x])
 */
		*cp = '\0';
		(void) strncpy(w, cp+1, 15);
		w[0] = cp[7];		/* Years */
		w[1] = cp[8];
		w[2] = cp[1];		/* Months */
		w[3] = cp[2];
		w[4] = cp[4];		/* Days */
		w[5] = cp[5];
		w[6] = cp[10];		/* Hours */
		w[7] = cp[11];
		w[8] = cp[13];		/* Minutes */
		w[9] = cp[14];
		w[10] = '\0';
	} else
		w[0] = '\0';
	return (1);
}


/*
 * distmatch -- see if a file matches a set of distributions.
 * We have to do this by (yech!) opening the file, finding
 * the Distribution: line, if it has one, and seeing if the
 * things match.
 *
 *	Parameters:	"distlist" is the distribution list
 *			we want.
 *			"distcount" is the count of distributions in it.
 *			"grouplist" is the list of groups (articles)
 *			for this line of the history file.  Note that
 *			this isn't quite a filename.
 *			"groupcount" is the count of groups in it.
 *			
 *	Returns:	1 if the article is in the given distribution.
 *			0 otherwise.
 */

distmatch(distlist, distcount, grouplist, groupcount)
	char		*distlist[];
	int		distcount;
	char		*grouplist[];
	int		groupcount;
{
	register char	c;
	register char	*cp;
	register FILE	*fp;
	register int	i, j;
	char		buf[MAX_STRLEN];

	(void) strcpy(buf, spooldir);
	(void) strcat(buf, "/");
	(void) strcat(buf, grouplist[0]);

	for (cp = buf; *cp; cp++)
		if (*cp == '.')
			*cp = '/';

	fp = fopen(buf, "r");
	if (fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "distmatch: fopen %s: %m", buf);
#endif
		return (0);
	}

	while (fgets(buf, sizeof (buf), fp) != NULL) {
		if ((c = buf[0]) == '\n')		/* End of header */
			break;
		if (c != 'd' && c != 'D')
			continue;
		cp = index(cp + 1, '\n');
		if (cp)
			*cp = '\0';
		cp = index(buf, ':');
		if (cp == NULL)
			continue;
		*cp = '\0';
		if (streql(buf, "distribution")) {
			for (i = 0; i < distcount; ++i) {
				if (streql(cp + 2, distlist[i])) {
					(void) fclose(fp);
					return (1);
				}
			}
			(void) fclose(fp);
			return (0);
		}
	}

	(void) fclose(fp);

	/*
	 * We've finished the header with no distribution field.
	 * So we'll assume that the distribution is the characters
	 * up to the first dot in the newsgroup name.
	 */

	for (i = 0; i < groupcount; i++) {
		cp = index(grouplist[i], '.');
		if (cp)
			*cp = '\0';
		for (j = 0; j < distcount; j++)
			if (streql(grouplist[i], distlist[j]))
				return (1);
	}
		
	return (0);
}


/*
 * get_histlist -- return a nicely set up array of newsgroups
 * (actually, net.foo.bar/article_num) along with a count.
 *
 *	Parameters:		"array" is storage for our array,
 *				set to point at some static data.
 *				"list" is the history file newsgroup list.
 *
 *	Returns:		Number of group specs found.
 *
 *	Side effects:		Changes static data area.
 */

get_histlist(array, list)
	char		***array;
	char		*list;
{
	register int	histcount;
	register char	*cp;
	static	char	**hist_list = (char **) NULL;

	cp = index(list, '\n');
	if (cp)
		*cp-- = '\0';
	histcount = parsit(list, &hist_list);
	*array = hist_list;
	return (histcount);
}


/*
 * get_nglist -- return a nicely set up array of newsgroups
 * along with a count, when given an NNTP-spec newsgroup list
 * in the form ng1,ng2,ng...
 *
 *	Parameters:		"array" is storage for our array,
 *				set to point at some static data.
 *				"list" is the NNTP newsgroup list.
 *
 *	Returns:		Number of group specs found.
 *
 *	Side effects:		Changes static data area.
 */

get_nglist(array, list)
	char		***array;
	char		*list;
{
	register char	*cp;
	register int	ngcount;

	for (cp = list; *cp != '\0'; ++cp)
		if (*cp == ',')
			*cp = ' ';

	ngcount = parsit(list, array);

	return (ngcount);
}
