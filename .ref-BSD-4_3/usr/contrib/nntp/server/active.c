#ifndef lint
static char	*sccsid = "@(#)active.c	1.2	(Berkeley) 3/20/86";
#endif

#include "common.h"

/*
 * Routines to deal with the active file
 */

extern	int	act_cmp();

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
	char		line[MAX_STRLEN];
	register int	i;
	register FILE	*act_fp;
	char		*malloc();

	i = 0;

	/* If we're here on an interrupt, free up all the */
	/* previous groups */

	if (num_groups != 0)
		for (i = 0; i < num_groups; ++i)
			free(group_array[i]);

	act_fp = fopen(ACTIVE_FILE, "r");
	if (act_fp == NULL)
		return(0);

	while (i < MAX_GROUPS && fgets(line, sizeof(line), act_fp) != NULL) {
		if ((group_array[i] = malloc(strlen(line)+1)) == NULL) {
			(void) fclose(act_fp);
			return(0);
		}
		(void) strcpy(group_array[i++], line);
	}

	if (i == MAX_GROUPS) {
		syslog(LOG_ERR, "read_active: active file >= %d groups", i);
		syslog(LOG_ERR, "warning: recompile with MAX_GROUPS larger");
	}

	(void) fclose(act_fp);

	qsort((char *) group_array, i, sizeof(char *), act_cmp);

	return(i);
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
char	*group;
int	num_groups;
int	*low_msg, *high_msg;
{
	char	kludgebuf[MAX_STRLEN];
	int	cond;
	int	low, high;
	int	length;
	register int mid;

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
