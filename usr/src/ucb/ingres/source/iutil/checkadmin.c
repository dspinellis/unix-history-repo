# include	<ingres.h>
# include	<version.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)checkadmin.c	7.1	2/5/81)

/*
**  CHECKADMIN -- check admin file version, etc.
**
**	The checks for database version code and whatnot are
**	factored out into this routine.  When this routine returns,
**	the admin file should be legible to this program.
**	If the admin file is not legible, it will syserr.
**
**	Parameters:
**		fd -- open file descriptor for admin file.  Only
**			read access is required.
**
**	Returns:
**		nothing if ok.
**		not at all (or via syserr) if not ok.
**
**	Side Effects:
**		The Admin.adhdr struct will be filled in.
*/

checkadmin(fd)
register int	fd;
{
	register int	i;
	register int	k;

	i = ((char *) &Admin.adhdr.adversion) - ((char *) &Admin.adhdr);
	if (read(fd, (char *) &Admin.adhdr, i) != i)
		syserr("readadmin: admin read err 1");
	if (!bitset(A_NEWFMT, Admin.adhdr.adflags))
		syserr("readadmin: cannot use old databases");

	/* read in remainder of admin header */
	i = sizeof Admin.adhdr;
	if (Admin.adhdr.adlength < i)
		i = Admin.adhdr.adlength;
	i -= ((char *) &Admin.adhdr.adversion) - ((char *) &Admin.adhdr);
	if (i <= 0)
		syserr("readadmin: adlen=%d, hdrsz=%d, ct=%d", Admin.adhdr.adlength, sizeof Admin.adhdr, i);
	if ((k = read(fd, (char *) &Admin.adhdr.adversion, i)) != i)
		syserr("readadmin: admin read err 2, i=%d k=%d", i, k);

	/* check versions here */
	if (Admin.adhdr.adversion != DBVERCODE)
		syserr("cannot handle code %d databases (current code is %d)",
			Admin.adhdr.adversion, DBVERCODE);
	if (Admin.adhdr.adreldsz != sizeof Admin.adreld)
		syserr("checkadmin: descriptor size mismatch, dec=%d, actual=%d",
			Admin.adhdr.adreldsz, sizeof Admin.adreld);

	/* get to beginning of descriptors */
	if (lseek(fd, (long) Admin.adhdr.adlength, 0) < 0)
		syserr("checkadmin: seek");

	/* read the descriptors */
	if (read(fd, (char *) &Admin.adreld, Admin.adhdr.adreldsz) != Admin.adhdr.adreldsz)
		syserr("checkadmin: reld read sz=%d", Admin.adhdr.adreldsz);
	if (read(fd, (char *) &Admin.adattd, Admin.adhdr.adattdsz) != Admin.adhdr.adattdsz)
		syserr("checkadmin: attd read sz=%d", Admin.adhdr.adattdsz);
}
