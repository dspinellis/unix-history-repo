#include "llist.h"

extern void	free();
extern caddr_t	malloc();

#define	align(x)	((x) + ((x) % sizeof(long)))

/*
** recursively free a linked list
*/
void
l_free(lp)
register struct llist *lp;
{
	if (lp->l_next == NULL)
		return;
	l_free(lp->l_next);
	free(lp->l_item);
}

/*
** allocate a new element in a linked list, along with enough space
** at the end of the item for the next list element header.
*/
struct llist *
l_alloc(lp, s, len)
register struct llist	*lp;
caddr_t	*s;
register unsigned len;
{
	if (s == NULL || lp == NULL)
		return(NULL);

	lp->l_len = len;
	len = align(len);

	if ((lp->l_item = malloc(len + sizeof(struct llist))) == NULL)
		return(NULL);

	bcopy(s, lp->l_item, len);
	lp->l_next = (struct llist *)(&lp->l_item[len]);

	/*
	** set up next list entry
	*/
	lp = lp->l_next;
	lp->l_next = NULL;
	lp->l_item = NULL;
	lp->l_len = 0;
	return(lp);
}
