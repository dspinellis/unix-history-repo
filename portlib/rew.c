# include	"iodec.h"

/**
 **	rewind a file
 **/

rew(fn)
int	fn;
{
	register struct fileps	*fp;

	if (fn < 0 || fn >= MAXFILES)
		__error("rew: bad file number %d", fn);
	fp = &__filehdr[fn];
	if (fp->wrflag == 0)
		return;
	switch (fp->wrflag)
	{

	  case 3:
		fp->wrflag = 2;
		return;

	  case 4:		/* modify */
	  case 2:		/* write */
		cflush(fn);

	  case 1:		/* read */
		fp->nchars = fp->eoferr = 0;
		seek(fn, 0, 0);
		return;

	}
}
