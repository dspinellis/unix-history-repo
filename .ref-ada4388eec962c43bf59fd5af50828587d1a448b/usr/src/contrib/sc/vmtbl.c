#ifdef PSC
# include <stdio.h>
# include "sc.h"
# ifndef FALSE
#  define	FALSE	0
#  define	TRUE	1
# endif /* !FALSE */
# undef	error
# define error(msg)	fprintf(stderr, msg);
#else /* PSC */
# include <curses.h>
# include "sc.h"
#endif /* PSC */

extern	char	*malloc();
extern	char	*realloc();

#if defined(BSD42) || defined(BSD43)
#define	memcpy(dest, source, len)	bcopy(source, dest, (unsigned int)len);
#define	memset(dest, zero, len)		bzero((dest), (unsigned int)(len));
#endif

/*
 * check to see if *rowp && *colp are currently allocated, if not expand the
 * current size if we can.
 */
#ifndef PSC
void
checkbounds(rowp, colp)
int	*rowp;
int	*colp;
{
	if (*rowp < 0)
		*rowp = 0;
	else if (*rowp >= maxrows)
	{	if (*colp >= maxcols)
		{	if (!growtbl(GROWBOTH, *rowp, *colp))
			{	*rowp = maxrows -1;
				*colp = maxcols -1;
			}
			return;
		}
		else
		{	if (!growtbl(GROWROW, *rowp, 0))
				*rowp = maxrows-1;
			return;
		}
	}
	if (*colp < 0) 
		*colp = 0;
	else if (*colp >= maxcols)
	{	if (!growtbl(GROWCOL, 0, *colp));
			*colp = maxcols-1;
	}
}
#endif /* !PSC */
	

#define GROWALLOC(newptr, oldptr, nelem, type, msg) \
	if (oldptr == (type *)NULL) \
		newptr = (type *)malloc((unsigned)(nelem*sizeof(type))); \
	else \
		newptr = (type *)realloc((char *)oldptr, \
					 (unsigned)(nelem*sizeof(type))); \
	if (newptr == (type *)NULL) \
	{   error(msg); \
	    return(FALSE); \
	} \
	oldptr = newptr /* wait incase we can't alloc */

static	char	nolonger[] = "The table can't be any longer";
static	char	nowider[] = "The table can't be any wider";

/*
 * grow the main && auxiliary tables (reset maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
int
growtbl(rowcol, toprow, topcol)
int	rowcol;
int	toprow, topcol;
{
	struct ent ***tbl2;
	int	*fwidth2;
	int	*precision2;
	char	*col_hidden2;
	char	*row_hidden2;
	int	newrows, newcols;
	int	i;

#ifndef PSC
	newrows = maxrows;
#endif /* !PSC */

	newcols = maxcols;
	if (rowcol == GROWNEW)
	{
#ifndef PSC
		maxrows = toprow = 0;
		/* when we first start up, fill the screen w/ cells */
		{	int startval;
			startval = LINES - RESROW;
			newrows = startval > MINROWS ? startval : MINROWS;
			startval = ((COLS) - RESCOL) / DEFWIDTH;
			newcols = startval > MINCOLS ? startval : MINCOLS;
		}
#else
		newcols = MINCOLS;
#endif /* !PSC */
		maxcols = topcol = 0;
	}
#ifndef PSC
	/* set how much to grow */
	if ((rowcol == GROWROW) || (rowcol == GROWBOTH))
	{	if (toprow > maxrows)
			newrows = GROWAMT + toprow;
		else
			newrows += GROWAMT;
	}
#endif /* !PSC */
	if ((rowcol == GROWCOL) || (rowcol == GROWBOTH))
	{	if ((rowcol == GROWCOL) && ((maxcols == ABSMAXCOLS) ||
					(topcol >= ABSMAXCOLS)))
		{	error(nowider);
			return(FALSE);
		}

		if (topcol > maxcols)
			newcols = GROWAMT + topcol;
		else
			newcols += GROWAMT;

		if (newcols > ABSMAXCOLS)
			newcols = ABSMAXCOLS;
	}

#ifndef PSC
	if ((rowcol == GROWROW) || (rowcol == GROWBOTH) || (rowcol == GROWNEW))
	{
		GROWALLOC(row_hidden2, row_hidden, newrows, char, nolonger);
		memset(row_hidden+maxrows, 0, (newrows-maxrows)*sizeof(char));

		/* alloc tbl row pointers */
		GROWALLOC(tbl2, tbl, newrows, struct ent **, nolonger);
		memset(tbl+maxrows, 0, (newrows-maxrows)*(sizeof(struct ent **)));
	}
#endif /* !PSC */

	if ((rowcol == GROWCOL) || (rowcol == GROWBOTH) || (rowcol == GROWNEW))
	{
		GROWALLOC(fwidth2, fwidth, newcols, int, nowider);
		GROWALLOC(precision2, precision, newcols, int, nowider);
#ifdef PSC
		memset(fwidth+maxcols, 0, (newcols-maxcols)*sizeof(int));
		memset(precision+maxcols, 0, (newcols-maxcols)*sizeof(int));
	}
#else
		GROWALLOC(col_hidden2, col_hidden, newcols, char, nowider);
		memset(col_hidden+maxcols, 0, (newcols-maxcols)*sizeof(char));
		for (i = maxcols; i < newcols; i++) {
			fwidth[i] = DEFWIDTH;
			precision[i] = DEFPREC;
		}

		/* [re]alloc the space for each row */
		for (i = 0; i < maxrows; i++)
		{
		    if ((tbl[i] = (struct ent **)realloc((char *)tbl[i],
			(unsigned)(newcols * sizeof(struct ent **)))) == (struct ent **)0)
			{	error(nowider);
				return(FALSE);
			}
		    memset((char *)ATBL(tbl,i, maxcols), 0,
			   (newcols-maxcols)*sizeof(struct ent **));
		}
	}
	else
		i = maxrows;

	/* fill in the bottom of the table */
	for (; i < newrows; i++)
	{	if ((tbl[i] = (struct ent **)malloc((unsigned)(newcols *
				sizeof(struct ent **)))) == (struct ent **)0)
		{	error(nowider);
			return(FALSE);
		}
		memset((char *)tbl[i], 0, newcols*sizeof(struct ent **));
	}

	FullUpdate++;
	maxrows = newrows;
#endif /* PSC */

	maxcols = newcols;
	return(TRUE);
}
