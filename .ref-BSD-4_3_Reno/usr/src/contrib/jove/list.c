#include "jove.h"
#include "list.h"

extern char	*emalloc();

private List *
list_new()
{
	List	*new;

	new = (List *) emalloc(sizeof (List));
	new->car = NIL;
	return new;
}

/* push an object to the beginning of list */

Element *
list_push(list, element)
register List	**list;
Element	*element;
{
	List	*new;

	new = list_new();
	new->cdr = *list;
	new->car = element;
	*list = new;
	return element;
}

Element *
list_pop(list)
List	**list;
{
	List	*cell;
	Element	*element;

	if (*list == NIL)
		return NIL;
	cell = *list;
	element = cell->car;
	free((char *) cell);
	*list = (*list)->cdr;

	return element;
}

#ifdef	NEVER
Element *
list_remove(list_head, element)
List	**list_head;
Element	*element;
{
	register List	*cp = *list_head,
			*prev = NIL;

	while (cp != NIL) {
		if (cp->car == element)
			break;
		prev = cp;
		cp = cp->cdr;
	}
	if (cp == NIL)
		return NIL;
	if (prev == NIL)
		*list_head = (*list_head)->cdr;
	else
		prev->cdr = cp->cdr;

	return element;
}

Element *
list_append(list, element)
List	**list;
Element	*element;
{
	List	*new, *lp;

	lp = *list;
	if (lp == NIL)
		return list_push(list, element);

	while (lp->cdr != NIL)
		lp = lp->cdr;
	new = list_new();
	lp->cdr = new;
	new->car = element;

	return element;
}

Element *
list_find(list, element)
List	*list;
Element	*element;
{
	while (list != NIL)
		if (list->car == element)
			return element;
	return NIL;
}
#endif	/* NEVER */
