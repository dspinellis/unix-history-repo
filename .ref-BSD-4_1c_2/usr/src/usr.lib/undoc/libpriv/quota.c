/*	%W%	(Melbourne)	%E%	*/

quota(a, b, c, d)
{
	return(syscall(64+61, a, b, c, d));
}
