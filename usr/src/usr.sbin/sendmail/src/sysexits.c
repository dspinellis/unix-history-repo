# include <sysexits.h>


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
};

int	N_SysEx = sizeof SysExMsg / sizeof SysExMsg[0];
