#

#include <signal.h>

/*
 * The following is for systems with botched signal() system entries
 * which don't return the proper value for the previous signal.
 */

int	oldsigs[17];

int
(*Signal())(sig, spot)
	int spot;
{
	int ret;

	/* printf("Signal(%d, %.1o)\n", sig, spot); */
	if (sig < 1 || sig > 16)
		return(-1);
	ret = oldsigs[sig];
	oldsigs[sig] = (int) spot;
	signal(sig, spot);
	return(ret);
}

Siginit()
{
	register int i;

	for (i = 1; i < 17; i++) {
		oldsigs[i] = (int) signal(i, SIG_IGN);
		signal(i, (int (*)()) oldsigs[i]);
	}
}
