/*	exit.c	1.1	83/06/23	*/

exit(code)
	int code;
{

	_cleanup();
	_exit(code);
}
