#ifndef lint
static char sccsid[] = "@(#)close.c	4.1 (Berkeley) 6/27/83";
#endif

extern vti;
closevt(){
	close(vti);
}
closepl(){
	close(vti);
}
