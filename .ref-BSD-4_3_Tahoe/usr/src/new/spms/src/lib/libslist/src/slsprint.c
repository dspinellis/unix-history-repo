/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsprint() prints a list slslist on named output stream. The list is
 * printed in ncol keys per line with column width colwidth. If tab is
 * an integer YES, keys will be padded by tabs, otherwise blanks. If
 * keys are padded by tabs, colwidth should be a multiple of TABSIZE.
 */
#include <stdio.h>
#include "null.h"
#include "slslist.h"
#include "yesno.h"

#define TABSIZE 8

static int tabflag;

void
slsprint(ncol, colwidth, tab, stream, slslist)
	int ncol;			/* number of columns */
	int colwidth;			/* maximum column width */
	int tab;			/* tab or blank padding */
	FILE *stream;			/* output stream */
	SLSLIST *slslist;		/* pointer to list head block */
{
	int col;			/* column index	*/
	int kc;				/* key count */
	int kn;				/* key number */
	int lastkn = 1;			/* last key number */
	int nlcols;			/* number of longer columns */
	int nrows;			/* maximum number of rows */
	int row;			/* row index */
	SLSBLK *curblk;			/* current list block */
	void putkey();			/* put key on stream */

	tabflag = tab;

	nrows = slslist->nk/ncol + 1;
	nlcols = slslist->nk % ncol;
	curblk = slslist->head;
	for (kc=1, row=1; row <= nrows; row++)
		for (col = 1; col <= ncol && kc <= slslist->nk; col++, kc++)
			{
			if (col <= nlcols)
				kn = (nrows*(col-1)) + row;
			else
				kn = ((nrows-1)*(col-1)) + nlcols + row;

			if (lastkn > kn)
				{
				curblk = slslist->head;
				lastkn = 1;
				}
			for (; lastkn < kn; lastkn++)
				curblk = curblk->next;

			if (col == ncol || kc == slslist->nk)
				{
				fputs(curblk->key, stream);
				putc('\n', stream);
				}
			else
				putkey(curblk->key, colwidth, stream);
			}
}



/*
 * Putkey prints a key, left-justified in a field of colwidth characters.
 * The key is padded on the right by tabs or blanks.
 */
static void
putkey(key, colwidth, stream)
	char *key;			/* key string */
	int colwidth;			/* maximum column width */
	FILE *stream;			/* output stream */
{
	while (*key != '\0' && colwidth-- > 0)
		putc(*key++, stream);

	if (colwidth > 0)
		if (tabflag == YES)
			for (; colwidth > 0; colwidth -= TABSIZE)
				putc('\t', stream);
		else while (colwidth-- > 0)
			putc(' ', stream);
}
