# include	"iodec.h"

/**
 **	close a file
 **/

cclose(fn)
int	fn;
{
	register struct fileps	*fp;

	if (fn < 0 || fn >= MAXFILES)
		__error("cclose: bad file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->nchars > 0 && fp->wrflag > 1)
		flush(fn);
	if (fp->wrflag != 0 && fp->buff >= &__statbuf[MAXFILES])
		free(fp->pbuff);
	fp->nchars = fp->eoferr = fp->buff = fp->bptr = fp->wrflag = fp->bsize = 0;
	close(fn);
}
