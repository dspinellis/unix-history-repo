# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)utility.c	7.1	2/5/81)

dumptid(tid)
register TID	*tid;
{
	long	pageid;

	pluck_page(tid, &pageid);
	printf("tid: %ld/%d\n", pageid, (tid->line_id & I1MASK));
	return (0);
}

/*
**	struct for extracting page number from a tid
**	and storing in a long
**
**	We want the line number (lpgx) to be in the low-order part of
**	a long.  Since PDP's and VAXes have the order of the half-
**	words reversed, this structure must be different.
*/

struct lpage
{
# ifdef PDP11
	char	lpg0, lpgx;
	char	lpg2, lpg1;
# else
	char	lpg2, lpg1, lpg0, lpgx;
# endif
};
/*  PLUCK_PAGE
**
**	pluck_page extracts the three byte page_id from a TID
**	and puts it into a long variable with proper allignment.
*/

pluck_page(t, var)
register TID	*t;
long		*var;
{
	register struct lpage	*v;

	v = (struct lpage *) var;
	v->lpg0 = t->pg0;
	v->lpg1 = t->pg1;
	v->lpg2 = t->pg2;
	v->lpgx = 0;
	return (0);
}

/*	stuff_page is the reverse of pluck_page	*/
stuff_page(t, var)
register TID	*t;
long		*var;
{
	register struct lpage	*v;

	v = (struct lpage *) var;
	t->pg0 = v->lpg0;
	t->pg1 = v->lpg1;
	t->pg2 = v->lpg2;
	return (0);
}
