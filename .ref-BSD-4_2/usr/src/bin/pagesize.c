#ifndef lint
static char sccsid[] = "@(#)pagesize.c	4.2 (Berkeley) 8/11/83";
#endif

main()
{

	printf("%d\n", getpagesize());
}
