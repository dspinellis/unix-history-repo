/**
 **	returns true if running in foreground, zero if in background
 **/

intss()
{
	int		a[3];

	if (gtty(0, a) == 0)
		return (1);
	return (0);
}
