# include	"iodec.h"

/**
 **	put a single character
 **/

int	f_log	0;

cputc(c, fn)
char	c;
int	fn;
{
	register struct fileps	*fp;
	extern int		cout;

	if (fn < 0 || fn >= MAXFILES)
		__error("cputc: illegal file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->wrflag == 1)
		__error("cputc: file %d not open for write", fn);
	if (fp->wrflag == 0)
		__makbuf(fn, 1);
	if (f_log > 0 && fn == cout)
		cputc(c, f_log);
	*fp->bptr++ = c;
	if (++fp->nchars < fp->bsize)
		return (c);
	if (fp->wrflag == 3)
	{
		seek(fn, 0, 2);
		fp->wrflag = 2;
	}
	if (write(fn, fp->bptr = fp->buff, fp->nchars) < fp->nchars)
		__error("cputc: write error on %d", fn);
	fp->nchars = 0;
	return (c);
}
