# include	"ctlmod.h"
# include	<sccs.h>

SCCSID(@(#)cm_close.c	7.1	2/5/81)

/*
**  CM_CLOSE -- close the parser write pipe
**
**	This routine is a Kludge for use by the tty monitor only.
**	It is here because of the difficulty of getting at the
**	Cm struct from another directory.
*/

cm_close()
{
	close(Cm.cm_proc[1].pr_file);
}
