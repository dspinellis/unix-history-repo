/*
**  USERDBM.H -- user-level definitions for the DBM library
**
**	Version:
**		@(#)userdbm.h	4.1		7/25/83
*/

typedef struct
{
	char	*dptr;
	int	dsize;
} DATUM;

extern DATUM fetch();
