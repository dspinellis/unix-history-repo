#ifndef lint
static char sccsid[] = "@(#)exit.c	5.1 (Berkeley) %G%";
#endif not lint

exit(code)
	int code;
{

	_cleanup();
	_exit(code);
}
