# include	<sccs.h>

SCCSID(@(#)version.c	7.10	10/27/81)

/*
**  VERSION.C -- define the current version
**
**	This just factors out the current version into one file
**	to make releases easier.  It is printed by the terminal
**	monitor, but should probably be loaded with everything
**	to guarantee a stamp in every file.
**
**	This file defines the system version identification.
*/

char	SysIdent[] =	"INGRES version 7.10 (10/27/81)";
