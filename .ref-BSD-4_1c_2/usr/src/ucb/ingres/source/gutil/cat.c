# include	<sccs.h>

SCCSID(@(#)cat.c	7.1	2/5/81)

/*
**  CAT -- "cat" a file
**
**	This function is essentially identical to the UNIX cat(I).
**
**	Parameters:
**		file -- the name of the file to be cat'ed
**
**	Returns:
**		zero -- success
**		else -- failure (could not open file)
**
**	Side Effects:
**		"file" is open and read once through; a copy is made
**			to the standard output.
*/

cat(file)
char	*file;
{
	char		buf[512];
	register int	i;
	register int	fd;

	fd = open(file, 0);
	if (fd < 0)
		return (1);

	while ((i = read(fd, buf, 512)) > 0)
	{
		write(1, buf, i);
	}

	return (0);
}
