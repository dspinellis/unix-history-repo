/*					-[Sat Jan 29 14:02:34 1983 by jkf]-
 * 	vaxframe.h			$Locker:  $
 * vax calling frame definition
 *
 * $Header: /na/franz/franz/h/vaxframe.h,v 1.1 83/01/29 14:08:11 jkf Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */

struct frame {
	lispval	(*handler)();
	long	mask;
	lispval	*ap;
struct 	frame	*fp;
	lispval	(*pc)();
	lispval	*r6;
	lispval *r7;
};
