# include	"monitor.h"
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)newdirec.c	7.1	2/5/81)



/*
**  CHANGE WORKING DIRECTORY
*/

newdirec()
{
	register char	*direc;
	extern char	*getfilenm();

	direc = getfilenm();
	if (chdir(direc))
		printf("Cannot access directory \"%s\"\n", direc);
}
