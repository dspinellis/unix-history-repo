#ifndef lint
static char	*sccsid = "@(#)active.c	1.11	(Berkeley) 7/17/87";
#endif

#include "common.h"

/*
 * Routines to deal with the active file
 */

int	act_cmp();

/*
 * read_groups -- read the active file into memory, sort it,
 *	and return the number of newsgroups read in.
 *	If FASTFORK is true, this can be called by interrupt,
 *	and may have to free up the old storage.  We decide
 *	this by the fact that "num_groups" will be non-zero if
 *	we're here on an interrupt.
 *
 *	Parameters:	None.
 *
 *	Returns:	Number of newsgroups read into
 *			memory.
 *			Zero on error.
 *
 *	Side effects:	Reads newsgroups into "group_array"
 *			and sorts them.
 */

read_groups()
{
	register int	i;
	register int	act_fd;
	register char	*actbuf, *cp, *end;
	char		*malloc();
	struct stat	statbuf;

	/*
	 * If we're here on an interrupt, free up all the
	 * previous groups.
	 */

	if (num_groups != 0) {
		(void) free(group_array[0]);	/* Assume [0] -> actbuf */
		(void) free(group_array);
	}

	act_fd = open(activefile, O_RDONLY);
	if (act_fd < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "read_groups: open %s: %m", activefile);
#endif
		return (0);
	}

	if (fstat(act_fd, &statbuf) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "read_groups: fstat: %m");
#endif
		(void) close(act_fd);
		return (0);
	}

	actbuf = malloc(statbuf.st_size);
	if (actbuf == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "read_groups: malloc %d bytes: %m",
			statbuf.st_size);
#endif
		(void) close(act_fd);
		return (0);
	}

	if (read(act_fd, actbuf, (int)statbuf.st_size) != statbuf.st_size) {
#ifdef SYSLOG
		syslog(LOG_ERR, "read_groups: read %d bytes: %m",
			statbuf.st_size);
#endif
		(void) close(act_fd);
		return (0);
	}

	(void) close(act_fd);

	for (i = 0, cp = actbuf, end = actbuf + statbuf.st_size; cp < end; cp++)
		if (*cp == '\n')
			i++;

	group_array = (char **) malloc(i * (sizeof (char *)));
	if (group_array == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "read_groups: malloc %d bytes: %m",
			i * sizeof (char **));
#endif
		(void) close(act_fd);
		return (0);
	}

	cp = actbuf;
	i = 0;
	while (cp < end) {
		group_array[i++] = cp;
		cp = index(cp, '\n');
		if (cp == NULL)
			break;
		*cp = '\0';
		cp++;
	}

	qsort((char *) group_array, i, sizeof (char *), act_cmp);

	return (i);
}


act_cmp(ptr1, ptr2)
	char	 **ptr1, **ptr2;
{
	return(strcmp(*ptr1, *ptr2));
}


/*
 * find_group -- find a given newsgroup and return
 *	the low and high message numbers in the group
 *	(space provided by user).
 *
 *	Parameters:	"group" is the name of the group
 *			we're searching for.
 *			"num_groups" is the total number
 *			of groups in the group array.
 *			"low_msg" and "high_msg" are
 *			pointers to where we're supposed
 *			to put the low and high message numbers.
 *
 *	Returns:	0 if all goes well,
 *			-1 if we can't find the group.
 *
 *	Side effects:	None.
 */

find_group(group, num_groups, low_msg, high_msg)
	char		*group;
	int		num_groups;
	int		*low_msg, *high_msg;
{
	char		kludgebuf[MAX_STRLEN];
	int		cond;
	register int	low, high, mid;
	int		length;

	low = 0;
	high = num_groups-1;
	(void) strcpy(kludgebuf, group);
	(void) strcat(kludgebuf, " ");
	length = strlen(kludgebuf);

	while (low <= high) {
		mid = (low + high) / 2;
		if ((cond = strncmp(kludgebuf, group_array[mid], length)) < 0)
			high = mid - 1;
		else if (cond > 0)
			low = mid + 1;
		else {
			(void) sscanf(group_array[mid], "%s %d %d",
				kludgebuf, high_msg, low_msg);
			return(0);
		}
	}
	return(-1);
}
