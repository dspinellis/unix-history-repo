# include "ciodec.c"
cclose (fn)
int fn;
{
struct fileps *fp;
extern char *IEH3olbf[10];
if (fn < 0 || fn > 20) return;
fp = &IEH3fpts[fn];
if (fp->nchars > 0 || fp->wrflag == 3)
	switch (fp->wrflag)
		{
		case 3: seek(fn,0,2);
		case 2: write (fn, fp->buff, fp->nchars);
			break;
		case 1: seek(fn, -(fp->nchars), 1);
		}
fp -> nchars = fp->eoferr = 0;
fp -> wrflag = 0;
if ( IEH3olbf[fn] != 0 && fp->bsize >4)
	free(IEH3olbf[fn]);
IEH3olbf[fn] = 0;
close (fn);
}
char *IEH3olbf[10] {0,0,0,0,0,0,0,0,0,0};
