# include	"iodec.h"

/**
 **	get a single character
 **/

int	__backup	0;

cgetc(xfn)
int	xfn;
{
	register struct fileps	*fp;
	register int		i;
	extern int		f_log;
	extern int		cin;
	register int		fn;

	fn = xfn;
	if (fn < 0 || fn >= MAXFILES)
		__error("cgetc: illegal file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->wrflag == 2)
		__error("cgetc: file %d not open to read", fn);
	if (fp->wrflag == 0)
		__makbuf(fn, 0);
	if (fp->eoferr)
		return ('\0');
	if (fp->nchars == 0)
	{
		i = read(fn, fp->bptr = fp->buff, fp->bsize);
		if (i < 0)
			__error("cgetc: read error on %d", fn);
		if (i == 0)
		{
			fp->eoferr = 2;
			return ('\0');
		}
		fp->nchars = i;
	}
	fp->nchars--;
	if (f_log > 0 && fn == cin)
		if (__backup)
			__backup--;
		else
			cputc(*fp->bptr, f_log);
	return(*fp->bptr++);
}
