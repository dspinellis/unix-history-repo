# include	<ingres.h>
# include	<sccs.h>

SCCSID(@(#)mcall.c	7.1	2/5/81)



/*
**  MCALL -- call a macro
**
**	This takes care of springing a macro and processing it for
**	any side effects.  Replacement text is saved away in a static
**	buffer and returned.
**
**	Parameters:
**		mac -- the macro to spring.
**
**	Returns:
**		replacement text.
**
**	Side Effects:
**		Any side effects of the macro.
**
**	Trace Flags:
**		51
*/

char *
mcall(mac)
char	*mac;
{
	register char	c;
	register char	*m;
	register char	*p;
	static char	buf[100];
	extern char	macsget();

	m = mac;

#	ifdef xMTR2
	tTfp(51, -1, "mcall('%s')\n", m);
#	endif

	/* set up to process the macro */
	macinit(macsget, &mac, FALSE);

	/* process it -- throw away result */
	for (p = buf; (c = macgetch()) > 0; )
	{
#		ifdef xMTR2
		if (tTf(51, 1))
			putchar(c);
#		endif
		if (p < &buf[sizeof buf])
			*p++ = c;
	}

	*p = 0;

	return (buf);
}
