/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "date.h"
#include "from.h"
#include "null.h"
#include "yesno.h"

#define INCRFROM	1000		/* amount to increase From ptr array */
#define MAXFROM		1000		/* initial size of From pointer array */

static FROM **Fromp;			/* pointer to "From " pointer array */
static int Ifrom;			/* current "From " ptr array index */
static int Maxfrom = MAXFROM;		/* maximum number of "From " lines */
static int Nfrom;			/* number of "From " lines */

/*
 * fromcmp() compares the dates of two "From " lines and returns an integer
 * less than, equal to, or greater than zero, according to the chronological
 * order of the dates.
 */
static int
fromcmp(f1, f2)
	register FROM **f1;		/* FROM struct pointer */
	register FROM **f2;		/* FROM struct pointer */
{
	register int d;			/* relative time */

	if ((d = (*f1)->bdt.t_year - (*f2)->bdt.t_year) < 0 || d > 0)
		return(d);
	else if ((d = (*f1)->bdt.t_mon - (*f2)->bdt.t_mon) < 0 || d > 0)
		return(d);
	else if ((d = (*f1)->bdt.t_day - (*f2)->bdt.t_day) < 0 || d > 0)
		return(d);
	else if ((d = (*f1)->bdt.t_hour - (*f2)->bdt.t_hour) < 0 || d > 0)
		return(d);
	else if ((d = (*f1)->bdt.t_min - (*f2)->bdt.t_min) < 0 || d > 0)
		return(d);
	return((*f1)->bdt.t_sec - (*f2)->bdt.t_sec);
}



/*
 * isfrom() returns a pointer to a "From " line struct if a "From " line,
 * otherwise NULL.
 */
FROM *
isfrom(linebuf)
	char *linebuf;			/* line to be examined */
{
	static FROM from;		/* "From " line struct */
	static char frombuf[BUFSIZ];	/* parsed "From " line buffer */
	char *strcpy();			/* string copy */
	int  isdate();			/* is string a ctime(3) date? */
	int strncmp();			/* compare strings for n chars */
	void parsefrom();		/* parse "From " line */

	if (strncmp(linebuf, "From ", 5) != 0)
		return(0);
	/*
	 * save a copy of linebuf in frombuf because parsefrom()
	 * breaks up the buffer into several strings and assigns
	 * from struct pointers to those strings
	 */
	strcpy(frombuf, linebuf);
	parsefrom(frombuf, &from);
	if (!isdate(from.date))
		return(NULL);
	return(&from);
}



/*
 * initfrom() creates a "From " pointer array and initializes it with
 * a dummy from struct to take care of spurious lines of information
 * before the first "From " line in the file. Returns a pointer to the
 * from struct, or NULL if out of memory.
 */
FROM *
initfrom()
{
	char *calloc();			/* zeroed memory allocation */
	char *malloc();			/* memory allocator */

	if ((Fromp=(FROM **) malloc((unsigned)Maxfrom*sizeof(FROM *))) == NULL)
		return((FROM *) NULL);
	Ifrom = 0;
	if ((Fromp[Ifrom] = (FROM *) calloc(1, sizeof(FROM))) == NULL)
		return((FROM *) NULL);
	Nfrom = 1;
	return(Fromp[Ifrom]);
}



/*
 * nextword() collects a liberal (blank, tab delimited) word and returns a
 * pointer to the next word or string.
 */
char *
nextword(word, bp)
	register char *bp;		/* buffer pointer */
	char **word;			/* word or string */
{
	for (; *bp != '\0' && isspace(*bp); bp++)
		continue;
	*word = bp;
	for (; *bp != '\0' && !isspace(*bp); bp++)
		continue;
	*bp++ = '\0';
	for (; *bp != '\0' && isspace(*bp); bp++)
		continue;
	return(bp);
}



/*
 * outfrom() copies an input stream to an output stream in chronological
 * order of "From " messages. Returns YES if successful, otherwise NO.
 */
outfrom(ifp, ofp)
	register FILE *ifp;		/* input stream */
	register FILE *ofp;		/* output stream */
{
	register int c;			/* current character */
	register int i;			/* "From " pointer array index */
	register int n;			/* character counter */

	for (i = 0; i < Nfrom; i++)
		{
		fseek(ifp, Fromp[i]->m_seek, 0);
		for (n = Fromp[i]->m_len; n > 0; n--)
			{
			c = getc(ifp);
			if (putc(c, ofp) == EOF)
				return(NO);
			}
		}
	return(YES);
}



/*
 * parsefrom splits a "From " line into its components and sets from
 * struct pointers into frombuf.
 */
void
parsefrom(frombuf, from)
	char *frombuf;			/* "From " line buffer */
	FROM *from;			/* "From " line struct */
{
	register char *dp;		/* date pointer */
	register char *fb;		/* frombuf pointer */
	char *nextword();		/* get word & go to next word */
	char *skipword();		/* skip to next word */
	char *strcpy();			/* string copy */
	int strncmp();			/* compare strings for n chars */

	fb = frombuf;
	fb = nextword(&from->from, skipword(fb));
	if (strncmp("tty", fb, 3) == 0)
		fb = nextword(&from->tty, skipword(fb));
	from->date = fb;
	for (dp = fb; *dp != '\n' && *dp != '\0'; dp++)
		continue;
	*dp = '\0';
}



/*
 * savefrom() saves a "From " struct somewhere and appends it to the From
 * pointer array. The array is extended if necessary. Returns a pointer
 * to the somewhere, or NULL if out of memory.
 */
FROM *
savefrom(f)
	FROM *f;			/* "From " line struct */
{
	char *malloc();			/* memory allocator */
	char *realloc();		/* reallocate memory block */

	if (Nfrom > Maxfrom)
		{
		Maxfrom += INCRFROM;
		if ((Fromp = (FROM **) realloc((char *)Fromp,
		     (unsigned)Maxfrom*sizeof(FROM *))) == NULL)
			return((FROM *) NULL);
		}
	if ((Fromp[++Ifrom] = (FROM *) malloc(sizeof(FROM))) == NULL)
		return((FROM *) NULL);
	Nfrom++;

	Fromp[Ifrom]->bdt.t_sec = f->bdt.t_sec;
	Fromp[Ifrom]->bdt.t_min = f->bdt.t_min;
	Fromp[Ifrom]->bdt.t_hour = f->bdt.t_hour;
	Fromp[Ifrom]->bdt.t_day = f->bdt.t_day;
	Fromp[Ifrom]->bdt.t_mon = f->bdt.t_mon;
	Fromp[Ifrom]->bdt.t_year = f->bdt.t_year;
	Fromp[Ifrom]->m_seek = f->m_seek;
	Fromp[Ifrom]->m_len = 0;
	return(Fromp[Ifrom]);
}



/*
 * sortfrom() sorts "From " lines chronologically.
 */
void
sortfrom()
{
	int fromcmp();			/* compare "From " lines by date */

	qsort((char *) Fromp, Nfrom, sizeof(FROM *), fromcmp);
}
