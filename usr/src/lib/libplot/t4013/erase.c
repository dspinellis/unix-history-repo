#ifndef lint
static char sccsid[] = "@(#)erase.c	1.1 (Berkeley) %G%";
#endif

extern int ohiy;
extern int ohix;
extern int oloy;
erase(){
	int i;
		putch(033);
		putch(014);
		ohiy= -1;
		ohix = -1;
		oloy = -1;
		return;
}
