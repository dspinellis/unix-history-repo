    /*
     *	make sure you are editing
     *		CONFIG.c
     *	editing config.c won't work
     */

static	char *sccsid = "@(#)CONFIG.c	2.1 (Berkeley) 82/03/31";

    /*
     *	the version of translator
     */
char	*version = "2.1 (VERSION %G%)";

    /*
     *	the location of the error strings
     *	and the length of the path to it
     *	(in case of execution of pc0 as a.out)
     */
char	*err_file = "LIBDIR/ERRORSTRINGS";
int	err_pathlen = sizeof("LIBDIR/");

    /*
     *	the location of the short explanation
     *	and the length of the path to it
     *	the null at the end is so pix can change it to pi'x' from pi.
     */
char	*how_file = "LIBDIR/HOWFILE\0";
int	how_pathlen = sizeof("LIBDIR/");
char	*px_header = "LIBDIR/px_header";	/* px_header's name */
char	*pi_comp = "INTERPDIR/pi";		/* the compiler's name */
char	*px_intrp = "INTERPDIR/px";		/* the interpreter's name */
char	*px_debug = "INTERPDIR/pdx";		/* the debugger's name */
