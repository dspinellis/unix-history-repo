#ifndef lint
static char sccsid[] = "@(#)close.c	4.1 (Berkeley) %G%";
#endif

extern vti;
closevt(){
	close(vti);
}
closepl(){
	close(vti);
}
