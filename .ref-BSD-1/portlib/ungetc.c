# include	"iodec.h"

/**
 **	return a single character to the io buffer
 **/

ungetc(c, fn)
char	c;
int	fn;
{
	register struct fileps	*fp;
	extern int		__backup;
	extern int		f_log, cin;

	if (fn < 0 || fn >= MAXFILES)
		__error("ungetc: illegal file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->wrflag == 2)
		__error("ungetc: file %d not open for reading", fn);
	if (fp->wrflag == 0)
		__makbuf(fn, 0);
	if (fp->bptr == fp->pbuff)
		__error("ungetc: file %d: peek buffer full", fn);
	if (f_log > 0 && fn == cin)
		__backup++;
	*--fp->bptr = c;
	fp->nchars++;
	return (c);
}
