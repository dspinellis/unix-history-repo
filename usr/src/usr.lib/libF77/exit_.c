
exit_(n)
long *n;
{
	f_exit();
	_cleanup();
	exit((int)*n);
}
