/* sccs id  %W%  %G%  */

struct frame {
struct 	frame	*fp;
	lispval	(*pc)();
	lispval ap[1];
};
