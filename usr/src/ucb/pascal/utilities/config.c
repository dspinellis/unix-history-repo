    /*
     *	make sure you are editing
     *		CONFIG.c
     *	editing config.c won't work
     */

#ifndef lint
static	char *sccsid = "@(#)CONFIG.c	2.7 (Berkeley) 84/02/08";
#endif

    /*
     *	the version of translator
     */
char	*version = "3.1 (2/8/84)";

    /*
     *	the location of the error strings
     *	and the length of the path to it
     *	(in case of execution as a.something)
     */
char	*err_file = "/usr/lib/pi3.1strings";
int	err_pathlen = sizeof("/usr/lib/")-1;

    /*
     *	the location of the short explanation
     *	and the length of the path to it
     *	the null at the end is so pix can change it to pi'x' from pi.
     */
char	*how_file = "/usr/lib/how_pi\0";
int	how_pathlen = sizeof("/usr/lib/")-1;
    
    /*
     *	things about the interpreter.
     *	these are not used by the compiler.
     */
#ifndef PC
char	*px_header = "/usr/lib/px_header";	/* px_header's name */
#endif

#ifndef PXP
char	*pi_comp = "/usr/ucb/pi";		/* the compiler's name */
char	*px_intrp = "/usr/ucb/px";		/* the interpreter's name */
char	*px_debug = "/usr/ucb/pdx";		/* the debugger's name */
#endif
