/*
 *	"@(#)exit_.c	1.2"
 */


exit_(n)
long *n;
{
	int	exitcode;

#if	vax
	if (nargs() == 0)
		exitcode = 0;
	else
#endif	vax
		exitcode = *n;	/* take any segmentation violation here */
	f_exit();
	_cleanup();
	exit(exitcode);
}
