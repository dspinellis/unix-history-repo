# include <sysexits.h>
# include "useful.h"

SCCSID(@(#)sysexits.c	3.3		%G%);

/*
**  SYSEXITS.C -- error messages corresponding to sysexits.h
*/

char	*SysExMsg[] =
{
	/* 64 */	"Bad usage",
	/* 65 */	"Data format error",
	/* 66 */	"Cannot open input",
	/* 67 */	"User unknown",
	/* 68 */	"Host unknown",
	/* 69 */	"Service unavailable",
	/* 70 */	"Internal error",
	/* 71 */	"Operating system error",
	/* 72 */	"System file missing",
	/* 73 */	"Can't create output",
	/* 74 */	"I/O error",
	/* 75 */	"Temporary failure",
};

int	N_SysEx = sizeof SysExMsg / sizeof SysExMsg[0];
