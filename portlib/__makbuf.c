# include	"iodec.h"

/**
 **	allocate buffer space for a file
 **/

__makbuf(fn, type)
int	fn;
int	type;
{
	register struct fileps	*fp;
	register int		size;
	register char		*space;
	int			peeksize;

	peeksize = 0;
	if (type <= 0 || type > 2)
		peeksize = __param.peeksize;
	fp = &__filehdr[fn];
	fp->eoferr = fp->nchars = 0;
	fp->wrflag = type + 1;
	size = 1;
	if (fn > 2)
		size = __param.bufsize;
	for (fp->buff = 0; ; size =/ 4)
	{
		if ((space = alloc(size + peeksize)) != -1)
		{
			fp->bsize = size;
			fp->buff = fp->bptr = space + peeksize;
			fp->pbuff = space;
			break;
		}
		if (size < 16)
			break;
	}
	if (fp->buff == 0)
		{
		fp->pbuff = &__statbuf[fn];
		fp->buff = fp->bptr = fp->pbuff + 1;
		fp->bsize = 1;
		}
}
