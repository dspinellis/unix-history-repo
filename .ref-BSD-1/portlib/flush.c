# include	"iodec.h"

/**
 **	flush all i/o buffers
 **/

flush()
{
	register int	i;

	for (i = 0; i < MAXFILES; i++)
		if (__filehdr[i].wrflag)
			cflush(i);
}
