# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)getnuser.c	7.1	2/5/81)

/*
**  GETNUSER -- get line from user file based on name
**
**	Given a user name as a string, this routine returns the
**	corresponding line from .../files/users into a buffer.
**
**	Parameters:
**		name -- the name of the user
**		buf -- a buf to dump the line in (declare as
**			char buf[MAXLINE + 1]
**
**	Returns:
**		zero -- success
**		one -- failure (user not present)
**
**	Side effects:
**		none
**
**	Files:
**		.../files/users (readable)
*/

getnuser(name, buf)
char	*name;
char	buf[];
{
	FILE		*userf;
	register char	c;
	register char	*bp;
	
	userf = fopen(ztack(Pathname, "/files/users"), "r");
	if (userf == NULL)
		syserr("getuser: open err");
	
	for (;;)
	{
		bp = buf;
		while ((c = getc(userf)) != '\n')
		{
			if (c == EOF)
			{
				fclose(userf);
				return (1);
			}
			*bp++ = c;
		}
		*bp++ = '\0';
		bp = buf;
		while ((c = *bp++) != ':')
		{
			if (c == '\0')
			{
				fclose(userf);
				return (1);
			}
		}
		*--bp = 0;
		if (sequal(buf, name))
		{
			fclose(userf);
			*bp = ':';
			return (0);
		}
	}
}
