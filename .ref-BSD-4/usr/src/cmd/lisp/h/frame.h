/* sccs id  @(#)frame.h	34.1  10/3/80  */

struct frame {
	lispval	(*handler)();
	long	mask;
	lispval	*ap;
struct 	frame	*fp;
	lispval	(*pc)();
	lispval	*r6;
	lispval *r7;
};
