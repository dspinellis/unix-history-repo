# include "ciodec.c"
char cputc(ch, fn)
{
struct fileps *fp;
if (nargs() != 2)
  IEHzap("cputc  ");
if (fn<0 || fn>20) return(ch);
fp = &IEH3fpts[fn];
if (fp->wrflag == 1)
	IEH3err("cputc: %d not open",fn);
if (fp->wrflag == 0)
	IEH3mbuf(fn,1);
*(fp->bptr++) = ch;
if (++(fp->nchars) < fp->bsize)
	return(ch);
if (fp->wrflag == 3) /* append, seek to end */
	{
	seek(fn, 0, 2);
	fp->wrflag = 2; /* now just write from here on */
	}
if ( write(fn, fp->bptr=fp->buff, fp->nchars) < 0)
	{
	if (fp->eoferr == 0)
		IEH3err("cputc: writing %d",fn);
	fp->eoferr = 3;
	}
fp->nchars = 0;
return (ch);
}
cout 1;
