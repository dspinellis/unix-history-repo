#ifndef lint
static char	*sccsid = "@(#)scandir.c	1.3	(Berkeley) 6/26/87";
#endif

#include "common.h"

/*
 * scan_dir -- scan the current directory for news articles,
 *	loading the article numbers into art_array.  Return
 *	number of articles loaded.
 *
 *	Paramaters:	"low_msg", "high_msg" are the low
 *			and high messages numbers in this
 *			group; we ignore numbers outside this
 *			range.
 *
 *	Returns:	Number of articles loaded into
 *			array.
 *
 *	Side effects:	Changes "art_array".
 */

extern	int	intcmp();

scan_dir(low_msg, high_msg)
int	low_msg, high_msg;
{
	register struct direct	*dirent;
	register DIR		*dirp;
	int			artnum;

	num_arts = 0;

	dirp = opendir(".");

	if (dirp == NULL)
		return (0);

	while ((dirent = readdir(dirp)) != NULL) {
		artnum = atoi(dirent->d_name);
		if (artnum != 0 && artnum >= low_msg && artnum <= high_msg)
			art_array[num_arts++] = artnum;
	}

	closedir(dirp);

	qsort((char *) art_array, num_arts, sizeof(int), intcmp);

	return (num_arts);
}


/*
 * intcmp -- compare to integers.
 *
 *	Parameters:	"x", "y" point to the integers to be compared.
 *
 *	Returns:	-1 if "x" is less than "y",
 *			0 if "x" equals "y", and
 *			1 if "x" is greater than "y".
 *
 *	Side effects:	None.
 */

intcmp(x, y)
register int	*x, *y;
{
	return (*x - *y);
}
