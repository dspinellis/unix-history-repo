# include	<ingres.h>
# include	<access.h>
# include	<aux.h>
# include	<lock.h>
# include	<sccs.h>

SCCSID(@(#)readadmin.c	7.1	2/5/81)

/*
**  READADMIN -- read admin file into 'Admin' cache
**
**	The admin file in the current directory is opened and read
**	into the 'Admin' cache.  The admin file contains the following
**	information:
**
**	A header block, containing the owner of the database (that is,
**	the DBA), and a set of status bits for the database as a whole.
**	These bits are defined in aux.h.  This header also includes a
**	field that defines the length of the header part & a version
**	stamp.
**
**	Descriptors for the relation and attribute relations.  These
**	descriptors should be completely correct except for the
**	relfp and relopn fields.  These are required so that the
**	process of opening a relation is not recursive.
**
**	After the admin file is read in, the relation and attribute
**	files are opened, and the relfp and relopn fields in both
**	descriptors are correctly initialized.  Both catalogs are
**	opened read/write.
**
**	WARNING:
**		This routine is redefined by creatdb.  If this
**		routine is changed, check that program also!!
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		The 'Admin' struct is filled in from the 'admin' file
**			in the current directory.
**		The 'relation....xx' and 'attribute...xx' files are
**			opened.
**
**	Files:
**		./admin
**			The bootstrap description of the database,
**			described above.
**
**	Trace Flags:
**		none
*/

readadmin()
{
	register int	i;
	char		relname[MAXNAME + 4];
	extern long	lseek();

	/* read the stuff from the admin file */
	i = open("admin", 0);
	if (i < 0)
		syserr("readadmin: open admin %d", i);
	checkadmin(i);
	close(i);

	/* open the physical files for 'relation' and 'attribute' */
	ingresname(Admin.adreld.reldum.relid, Admin.adreld.reldum.relowner, relname);
	if ((Admin.adreld.relfp = open(relname, 2)) < 0)
		syserr("readadmin: open rel %d", Admin.adreld.relfp);
	ingresname(Admin.adattd.reldum.relid, Admin.adattd.reldum.relowner, relname);
	if ((Admin.adattd.relfp = open(relname, 2)) < 0)
		syserr("readadmin: open att %d", Admin.adattd.relfp);
	Admin.adreld.relopn = (Admin.adreld.relfp + 1) * -5;
	/* we just want to read here create, modify and destroy fix it up */
	Admin.adattd.relopn = (Admin.adattd.relfp + 1) * 5;

	return;
}
