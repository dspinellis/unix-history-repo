#ifndef lint
static char	*sccsid = "@(#)newgroups.c	1.11	(Berkeley) 10/15/87";
#endif

#include "common.h"
#include "time.h"

/*
 * NEWGROUPS date time ["GMT"] [<distributions>]
 *
 * Display new newsgroups since a given date and time, but only
 * for those in <distributions>.
 */

newgroups(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[MAX_STRLEN];
	register char	*cp, *temp;
	static char	**dist_list = (char **) NULL;
	int		distcount = 0;
	int		i;
	long		date;
	register FILE	*date_fp;

	if (argc < 3) {
		printf("%d Usage: NEWGROUPS yymmdd hhmmss [\"GMT\"] [<distributions>].\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	date_fp = fopen(ngdatefile, "r");
	if (date_fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "newgroups: fopen %s: %m", ngdatefile);
#endif
		printf("%d Cannot open newsgroup date file.\r\n", ERR_FAULT);
		(void) fflush(stdout);
		return;
	}

	/*	    YYMMDD		    HHMMSS	*/
	if (strlen(argv[1]) != 6 || strlen(argv[2]) != 6) {
		printf("%d Date/time must be in form YYMMDD HHMMSS.\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(date_fp);
		return;
	}

	(void) strcpy(line, argv[1]);			/* yymmdd */
	(void) strcat(line, argv[2]);			/* hhmmss */

	date = dtol(line);
	if (date < 0) {
		printf("%d Invalid date specification.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(date_fp);
		return;
	}

	argc -= 3;
	argv += 3;

	if (argc > 0 && streql(*argv, "GMT")) {	/* We store stuff in GMT */
			++argv;			/* anyway, so this is */
			--argc;			/* a "noop" */
	} else 					/* But that means not GMT */
		date = local_to_gmt(date);	/* is a definite "op" */

	if (argc > 0) {
		distcount = get_distlist(&dist_list, *argv);
		if (distcount < 0) {
			printf("%d Bad distribution list %s:\r\n", *argv);
			(void) fflush(stdout);
			(void) fclose(date_fp);
			return;
		}
	}

	printf("%d New newsgroups since %s follow.\r\n", OK_NEWGROUPS, line);

	while (fgets(line, sizeof(line), date_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, ' ')) != NULL)
			*cp = '\0';
		if (atoi(line) < date)
			break;

		if (distcount == 0) {
			putline(cp + 1);
		} else {
			temp = cp + 1;
			cp = index(temp, '.');
			if (cp == NULL)
				continue;
			*cp = '\0';
			for (i = 0; i < distcount; ++i)
				if (strcmp(temp, dist_list[i]) == 0) {
					*cp = '.';
					putline(temp);
					break;
				}
		}
	}
	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
	(void) fclose(date_fp);
}
