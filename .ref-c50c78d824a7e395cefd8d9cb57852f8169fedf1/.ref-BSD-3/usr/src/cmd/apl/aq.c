#
/*
 * C library -- alloc/free
 */

#define	logical	int

struct fb {
	logical	size;
	char	*next;
};

int	freelist[] = {
	0,
	-1,
};
logical	slop	= 2;

alloc(asize)
logical asize;
{ return ( calloc ( 1 , asize ) ); }  
/*{
	register logical size;
	register logical np;
	register logical cp;

	if ((size = asize) == 0)
		return(0);
	size += 3;
	size &= ~01;
	for (;;) {
		cp = freelist;
		while ((np = cp->next) != -1) {
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
			cp = np;
		}
		return ( calloc ( 1 , asize ) ); 

		/* asize = size<1024? 1024: size;
		if ((cp = sbrk(asize)) == -1) {
			error("workspace exceeded");

		}
		cp->size = asize;
		free(&cp->next); 
	}
}  */


afree(aptr)
char *aptr;
{if ( aptr == 0 ) return;
cfree ( aptr ); }  
/* {
	register logical ptr;
	register logical cp;
	register logical np;

	if (aptr == 0) return;
	ptr = aptr-4;
	cp = freelist;
	while ((np = cp->next) < ptr)
		cp = np;
	if (ptr+ptr->size == np) {
		ptr->size += np->size;
		ptr->next = np->next;
		np = ptr;
	} else
		ptr->next = np;
	if (cp+cp->size == ptr) {
		cp->size += ptr->size;
		cp->next = ptr->next;
	} else
		cp->next = ptr;
} */
