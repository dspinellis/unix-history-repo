# include "ciodec.c"
cflush (fn)
int fn;
{
struct fileps *fp;
if (nargs() != 1)
	IEHzap("cflush");
if (fn < 0 || fn >15) return;
fp = &IEH3fpts[fn];
if (fp->nchars > 0 && fp->wrflag >= 2)
	{
	write (fn, fp->buff,fp->nchars);
	fp->bptr = fp->buff;
	fp -> nchars = fp->eoferr = 0;
	}
if (fp->wrflag == 1)
	seek(fn, -(fp->nchars),1);
return;
}
