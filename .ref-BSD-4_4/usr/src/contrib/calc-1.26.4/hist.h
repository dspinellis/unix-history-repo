/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Definitions for command history module.
 */

/*
 * Default binding file and history size.
 */
#ifndef	HIST_BINDING_FILE
#define	HIST_BINDING_FILE	"/usr/lib/hist.bind"
#endif

#ifndef	HIST_SIZE
#define	HIST_SIZE		(1024*10)
#endif


/* 
 * path search defines
 */
#define	HOMECHAR	'~'	/* char which indicates home directory */
#define DOTCHAR		'.'	/* char which indicates current directory */
#define	PATHCHAR	'/'	/* char which separates path components */
#define	LISTCHAR	':'	/* char which separates paths in a list */
#define	PATHSIZE	1024	/* maximum length of path name */


/*
 * Possible returns from hist_init.  Note that an error from hist_init does
 * not prevent calling the other routines, but fancy command line editing
 * is then disabled.
 */
#define	HIST_SUCCESS	0	/* successfully inited */
#define	HIST_INITED	1	/* initialization is already done */
#define	HIST_NOFILE	2	/* bindings file could not be read */
#define	HIST_NOTTY	3	/* terminal modes could not be set */


extern	int	hist_init();
extern	void	hist_term();
extern	int	hist_getline();
extern	void	hist_saveline();

/* END CODE */
