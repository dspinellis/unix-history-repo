#
/*
**  QRYMOD.H -- Query Modification header file.
**
**	Contains the manifest constants and global variables
**	used by the query modification process.
**
**	Version:
**		@(#)qrymod.h	7.1	2/5/81
*/

extern DESC	Treedes;	/* descriptor for tree catalog */


struct
{
	short	qm_newresvar;	/* new result variable number */
}  Qm;


/*********************************************************************
**								    **
**  The following stuff is used by the protection algorithm only.   **
**								    **
*********************************************************************/
/* maximum query mode for proopset (<--> sizeof Proopmap - 1) */
# define	MAXPROQM	4
