# include "ciodec.c"
IEH3bsz 512;
IEH3mbuf (fn, type)
int fn, type;
{
struct fileps *fp;
extern char *IEH3olbf[], *alloc();
extern int IEHfbak[10];
int bx[19], size, bloc;
fp = &IEH3fpts[fn];
fp->eoferr = fp->nchars = 0;
fp->wrflag = type+1;
/* decide whether to buffer or not */
if (ttyn(fn) != 'x')
	size = 1;
else
if (fstat(fn,bx) > 0 && bx[0] == 40 && type == 0)
	size = 1;
else
	size = IEH3bsz;
for (fp->buff = 0; size >10 && fp->buff == 0; size =/ 4)
		if ((bloc = alloc(size+100)) != -1)
			{
			IEH3olbf[fn] = bloc;
			fp->buff = fp->bptr =bloc + 100;
			fp->bsize = size;
			break;
			}
if (fp->buff == 0)
	{
	fp->buff = fp->bptr = &IEHfbak[fn];
	fp->bsize = size>1 ? 2 : 1;
	}
}
struct fileps IEH3fpts [10];
int IEHfbak[10];
