/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"

#ifdef	LOAD_AV
# if	defined(BSD4_2) && !defined(BSD2_10)
#   if	defined(PURDUE_EE) && (defined(vax) || defined(gould))

void
closekmem()
{
}

int
get_la()
{
	return loadav(0);
}

#   else	/* !(defined(PURDUE_EE) && (defined(vax) || defined(gould))) */

#ifdef	sun
#   include <sys/param.h>
#endif
#include <nlist.h>

/*
 * struct nlist has different sizes on various machines.  So we
 * deliberately initialize only the first element.
 */
static struct	nlist nl[] = {
	{ "_avenrun" },
#define	X_AVENRUN	0
	{ "" },
};

static int	kmem = 0;

void
closekmem()
{
	if (kmem > 0)
		close(kmem);
}

int
get_la()
{
#ifdef	sun
	long	avenrun[3];
#else
	double	avenrun[3];
#endif
	extern long	lseek proto((int, long, int));

	if (kmem == -1) {
		return 400;	/* So shell commands will say "Chugging" */
	} else if (kmem == 0) {
		if ((kmem = open("/dev/kmem", 0)) == -1) {
			f_mess("Can't open kmem for load average.");
			return 400;
		}
		nlist("/vmunix", nl);
	}
	lseek(kmem, (long) nl[X_AVENRUN].n_value, 0);
	read(kmem, (UnivPtr) avenrun, sizeof(avenrun));
#ifdef	sun
	return (int) (avenrun[0] * 100L / FSCALE);
#else
	return (int) (avenrun[0] * 100. + .5);
#endif
}

#    endif  /* !(defined(PURDUE_EE) && (defined(vax) || defined(gould))) */
#  else	/* !(defined(BSD4_2) && !defined(BSD2_10)) */

void
closekmem()
{
}

int
get_la()
{
	short	avg[3];

	gldav(avg);
	return (int) (avg[0]*100L / 256);
}

#  endif	/* !(defined(BSD4_2) && !defined(BSD2_10)) */
#endif	/* LOAD_AV */
