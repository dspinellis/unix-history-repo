# include	"iodec.h"

/**
 **	flush out a single file
 **/

cflush(fn)
int	fn;
{
	register struct fileps	*fp;

	if (fn < 0 || fn >= MAXFILES)
		__error("cflush: bad file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->nchars > 0 && fp->wrflag >= 2)
	{
		if (fp->wrflag == 3)
		{
			seek(fn, 0, 2);
			fp->wrflag = 2;
		}
		write(fn, fp->buff, fp->nchars);
		fp->bptr = fp->buff;
		fp->nchars = fp->eoferr = 0;
	}
	if (fp->wrflag == 1)
		seek(fn, -(fp->nchars), 1);
	return;
}
