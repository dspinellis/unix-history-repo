/*
 * Definitions of external routines and variables for tn3270
 */

/*
 * Pieces exported from the telnet susbsection.
 */

extern int
#if defined(unix)
	HaveInput,
#endif /* defined(unix) */
	tout,
	tin;

extern char	*transcom;

extern int
	netflush(),
	quit(),
	TtyChars(),
	DataToTerminal();

extern void
	outputPurge(),
	EmptyTerminal(),
	StringToTerminal(),
	_putchar(),
	ExitPerror(),
	setcommandmode();

/*
 * Pieces exported from other random locations.
 */

extern char
	*strsave();
