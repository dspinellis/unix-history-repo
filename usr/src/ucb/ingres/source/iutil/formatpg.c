# include	<ingres.h>
# include	<access.h>
# include	<sccs.h>

SCCSID(@(#)formatpg.c	7.1	2/5/81)

formatpg(d, n)
DESC	*d;
long	n;
{
	struct accbuf	buf;
	register char	*p;
	extern long	lseek();

	if (Acc_head == 0)
		acc_init();
	if (lseek(d->relfp, 0l, 0) == -1)
		return (-2);
	buf.rel_tupid = d->reltid.ltid;
	buf.filedesc = d->relfp;
	for (p = (char *) &buf; p <= (char *) buf.linetab; p++)
		*p = NULL;
	buf.nxtlino = 0;
	buf.linetab[0] = (int) buf.firstup - (int) &buf;
	buf.ovflopg = 0;
	for (buf.mainpg = 1; buf.mainpg < n; (buf.mainpg)++)
	{
		if (write(buf.filedesc, (char *) &buf, PGSIZE) != PGSIZE)
			return (-3);
	}
	buf.mainpg = 0;
	if (write(buf.filedesc, (char *) &buf, PGSIZE) != PGSIZE)
		return (-4);
	Accuwrite += n;
	return (0);
}
