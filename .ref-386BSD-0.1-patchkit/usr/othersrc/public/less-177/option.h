#define	END_OPTION_STRING	('$')

/*
 * Types of options.
 */
#define	BOOL		01	/* Boolean option: 0 or 1 */
#define	TRIPLE		02	/* Triple-valued option: 0, 1 or 2 */
#define	NUMBER		04	/* Numeric option */
#define	STRING		010	/* String-valued option */
#define	NOVAR		020	/* No associated variable */
#define	REPAINT		040	/* Repaint screen after toggling option */
#define	NO_TOGGLE	0100	/* Option cannot be toggled with "-" cmd */

#define	OTYPE		(BOOL|TRIPLE|NUMBER|STRING|NOVAR)

/*
 * Argument to a handling function tells what type of activity:
 */
#define	INIT	0	/* Initialization (from command line) */
#define	QUERY	1	/* Query (from _ or - command) */
#define	TOGGLE	2	/* Change value (from - command) */

/* Flag to toggle_option to specify how to "toggle" */
#define	OPT_NO_TOGGLE	0
#define	OPT_TOGGLE	1
#define	OPT_UNSET	2
#define	OPT_SET		3

struct option
{
	char oletter;		/* The controlling letter (a-z) */
	char otype;		/* Type of the option */
	int odefault;		/* Default value */
	int *ovar;		/* Pointer to the associated variable */
	void (*ofunc)();	/* Pointer to special handling function */
	char *odesc[3];		/* Description of each value */
};

