/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include "yesno.h"

static char **STRINGS;			/* pointer array containing strings */
static int NSTRINGS;			/* number of strings in STRINGS */

/*
 * bininit() initializes and sorts the STRINGS pointer array.
 */
void
bininit(argc, argv)
	int argc;
	char **argv;
{
	int qsort();			/* quicker sort */
	int strpcmp();			/* pointed-to string comparison */

	NSTRINGS = argc;
	STRINGS = argv;
	qsort((char *) STRINGS, NSTRINGS, sizeof(char *), strpcmp);
}



/*
 * binsearch() returns integer YES if string is found in sorted pointer
 * array STRINGS, otherwise NO. Uses binary search.
 */
binsearch(string)
	char *string;			/* string to be matched */
{
	int comp;			/* compare string values */
	int high;			/* upper limit of array */
	int low;			/* lower limit of array */
	int mid;			/* middle of search range */
	int strcmp();			/* string comparison */

	low = 0;
	high = NSTRINGS - 1;
	while (low <= high)
		{
		mid = (low+high) / 2;
		if ((comp = strcmp(string, STRINGS[mid])) < 0)
			high = mid - 1;
		else if (comp > 0)
			low = mid + 1;
		else
			return(YES);
		}
	return(NO);
}



/*
 * strpcmp() compares strings in a pointer array. Returns whatever strcmp()
 * returns.
 */
static int
strpcmp(s1, s2)
	char **s1;
	char **s2;
{
	int strcmp();			/* string comparison */

	return(strcmp(*s1, *s2));
}
