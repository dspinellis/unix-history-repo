#
/*
 * C library -- alloc/free
 */

#define	logical	char *

struct fb {
	logical	size;
	char	*next;
};

int	freelist[] {
	0,
	-1,
};
logical	slop	2;

alloc(asize)
logical asize;
{
	register logical size;
	register logical np;
	register logical cp;

	if ((size = asize) == 0)
		return(0);
	size =+ 3;
	size =& ~01;
	for (;;) {
		for (cp=freelist; (np= cp->next) != -1; cp=np) {
			if (np->size>=size) {
				if (size+slop >= np->size) {
					cp->next = np->next;
					return(&np->next);
				}
				cp = cp->next = np+size;
				cp->size = np->size - size;
				cp->next = np->next;
				np->size = size;
				return(&np->next);
			}
		}
		asize = size<1024? 1024: size;
		if ((cp = sbrk(asize)) == -1) {
			return (-1);
		}
		cp->size = asize;
		free(&cp->next);
	}
}

free(aptr)
char *aptr;
{
	register logical ptr;
	register logical cp;
	register logical np;

	ptr = aptr-2;
	cp = freelist;
	while ((np = cp->next) < ptr)
		cp = np;
	if (ptr+ptr->size == np) {
		ptr->size =+ np->size;
		ptr->next = np->next;
		np = ptr;
	} else
		ptr->next = np;
	if (cp+cp->size == ptr) {
		cp->size =+ ptr->size;
		cp->next = ptr->next;
	} else
		cp->next = ptr;
}
