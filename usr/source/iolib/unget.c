# include "ciodec.c"
ungetc (c, fn)
{
struct fileps *fp;
if (nargs() != 2)
   IEHzap("ungetc");
/* push back onto input */
fp = &IEH3fpts[fn];
if (fp->wrflag == 0)
	IEH3mbuf(fn,0);
if (fp->bptr <= fp->buff - 100)
	IEH3err("ungetc/unprintf: buffer full file %d",fn);
*--fp->bptr = c;
fp->nchars++;
}
