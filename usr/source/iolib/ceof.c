# include "ciodec.c"
ceof (fn)
int fn;
{
struct fileps *fp;
fp = &IEH3fpts[fn];
if (fp->wrflag > 0 && fp->eoferr == 2)
	return(1);
else return(0);
}
cerr (fn)
int fn;
{
struct fileps *fp;
fp = &IEH3fpts[fn];
if (fp->eoferr == 0) fp->eoferr = 1;
if (fp->wrflag > 0 && fp->eoferr == 3)
	return(1);
else return(0);
}
