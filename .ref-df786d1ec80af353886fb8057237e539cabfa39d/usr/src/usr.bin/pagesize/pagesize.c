#ifndef lint
static char sccsid[] = "@(#)pagesize.c	4.2 (Berkeley) %G%";
#endif

main()
{

	printf("%d\n", getpagesize());
}
