/*
 * $Header: 68kframe.h,v 1.3 84/02/29 12:43:22 sklower Exp $
 * $Locker:  $
 * machine stack frame
 */
struct machframe {
struct 	machframe	*fp;
	lispval	(*pc)();
	lispval ap[1];
};
