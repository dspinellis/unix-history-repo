struct frame {
	lispval	(*handler)();
	long	mask;
	lispval	*ap;
struct 	frame	*fp;
	lispval	(*pc)();
	lispval	*r6;
	lispval *r7;
};
