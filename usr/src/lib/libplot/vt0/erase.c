#ifndef lint
static char sccsid[] = "@(#)erase.c	4.1 (Berkeley) %G%";
#endif

extern vti;
erase(){
	int i;
	i=0401;
	write(vti,&i,2);
}
