# include "ciodec.c"
char cgetc(fn)
{
struct fileps *fp;
if (nargs() != 1)
	IEHzap("cgetc  ");
if (fn <0 || fn >15)
	IEH3err("cgetc: %d illegal file number",fn);
fp = &IEH3fpts[fn];
if (fp == 0 || fp->wrflag > 1)
	IEH3err("cgetc: %d not open to read",fn);
if (fp->wrflag == 0)
	/* file not opened with fopen: try making a buffer */
	IEH3mbuf (fn, 0);
if (fp->eoferr > 1)
	return ('\0');
if (fp->nchars == 0)
	 switch (fp->nchars = read(fn, fp->bptr=fp->buff, fp->bsize))
		{
		case -1: /* error */
			if (fp->eoferr == 0)
				IEH3err("cgetc: error on %d",fn);
			fp->eoferr = 3;
			return ('\0');
		case 0: fp->eoferr = 2;
			return ('\0');
		}
fp->nchars--;
return (*(fp->bptr++));
}
cin 0;
