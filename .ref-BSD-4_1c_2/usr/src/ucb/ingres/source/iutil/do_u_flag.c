# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)do_u_flag.c	7.1	2/5/81)

/*
**  DO_U_FLAG -- Get User Code from User Code or Name
**
**	This routine is the "-u" flag processing.
**
**	Parameters:
**		u_flag -- a pointer to the -u flag.
**
**	Returns:
**		zero -- user not found (message also printed).
**		else -- pointer to the user code for the override user.
**
**	Side Effects:
**		The actual text of the -u flag may be trashed with the
**			new user code.
*/

char *
do_u_flag(u_flag)
char	*u_flag;
{
	register char	*p;
	char		buf[MAXLINE + 1];

	p = u_flag;

	if (getnuser(&p[2], buf) != 0)
	{
		if (p[2] == 0 || p[3] == 0 || p[4] != 0 ||
		    getuser(&p[2], buf) != 0)
		{
			getuser(0);
			printf("User %s does not exist\n", &p[2]);
			return (0);
		}
		getuser(0);
		return (&p[2]);
	}
	smove(getufield(buf, 1), p);
	return (p);
}
