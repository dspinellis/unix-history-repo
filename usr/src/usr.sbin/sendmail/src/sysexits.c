# include <sysexits.h>
# include "useful.h"

SCCSID(@(#)sysexits.c	3.4		%G%);

/*
**  SYSEXITS.C -- error messages corresponding to sysexits.h
*/

char	*SysExMsg[] =
{
	/* 64 USAGE */		"Bad usage",
	/* 65 DATAERR */	"Data format error",
	/* 66 NOINPUT */	"Cannot open input",
	/* 67 NOUSER */		"User unknown",
	/* 68 NOHOST */		"Host unknown",
	/* 69 UNAVAILABLE */	"Service unavailable",
	/* 70 SOFTWARE */	"Internal error",
	/* 71 OSERR */		"Operating system error",
	/* 72 OSFILE */		"System file missing",
	/* 73 CANTCREAT */	"Can't create output",
	/* 74 IOERR */		"I/O error",
	/* 75 TEMPFAIL */	"Temporary failure",
	/* 76 PROTOCOL */	"Remote protocol error",
};

int	N_SysEx = sizeof SysExMsg / sizeof SysExMsg[0];
