#ifndef lint
static char	*sccsid = "@(#)group.c	1.10	(Berkeley) 7/17/87";
#endif

#include "common.h"

/*
 * GROUP newsgroup
 *
 * Change the current group to the specified newsgroup.
 * We also change our current directory to that newsgroup if
 * a spool directory for it exists.
 * If the newsgroup specified is invalid, the old newsgroup
 * remains selected.
 */

group(argc, argv)
	int	argc;
	char	*argv[];
{
	char	temp_dir[256];
	int	high_msg, low_msg;
	char	*cp;
	char	*reqlist[2];

	if (argc != 2) {
		printf("%d Usage: GROUP newsgroup.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	if (index(argv[1], '/') != (char *) NULL) {
		printf("%d Invalid group name.\r\n", ERR_NOGROUP);
		(void) fflush(stdout);
		return;
	}

	if (find_group(argv[1], num_groups, &low_msg, &high_msg) < 0) {
		printf("%d Invalid group name.\r\n", ERR_NOGROUP);
		(void) fflush(stdout);
		return;
	}

	reqlist[0] = argv[1];
	reqlist[1] = NULL;

	if (ngpermcount) {
		if (ngmatch(strneql, ALLBUT,
		    ngpermlist, ngpermcount, reqlist, 1) == 0) {
			printf("%d You're not allowed to read %s, sorry.\r\n",
				ERR_ACCESS, argv[1]);
			(void) fflush(stdout);
			return;
		}
	}

	close_crnt();
	(void) chdir(spooldir);

#ifdef LOG
	syslog(LOG_INFO, "%s group %s", hostname, argv[1]);
#endif

	while ((cp = index(argv[1], '.')) != (char *) NULL)
		*cp = '/';

	(void) strcpy(temp_dir, spooldir);
	(void) strcat(temp_dir, "/");
	(void) strcat(temp_dir, argv[1]);

	/*
	 * (void) because a group can be in the active file
	 * but not have a spool directory.  Just leave us
	 * chdired to base spool directory if this fails.
	 */
	(void) chdir(temp_dir);

#ifdef LOG
	++grps_acsd;
#endif

	num_arts = scan_dir(low_msg, high_msg);
	art_ptr = 0;

	ingroup = 1;

	while ((cp = index(argv[1], '/')) != (char *) NULL)
		*cp = '.';

	printf("%d %d %d %d %s\r\n",
		OK_GROUP,
		num_arts,
		(num_arts > 0 ? art_array[0] : 0),
		(num_arts > 0 ? art_array[num_arts-1] : 0),
		argv[1]);
	(void) fflush(stdout);
}
