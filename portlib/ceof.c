# include	"iodec.h"

/**
 **	report end of file conditions
 **/

ceof(fn)
int	fn;
{
	register struct fileps	*fp;

	if (fn < 0 || fn >= MAXFILES)
		__error("ceof: bad file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->eoferr > 0)
		return (1);
	return (0);
}
