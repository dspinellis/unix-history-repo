/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)config.h	5.4 (Berkeley) %G%
 */

    /*
     *	external declarations of things from 
     *		CONFIG.c
     *
     */

    /*
     *	the version of translator
     */
extern char	*version;

    /*
     *	the location of the error strings
     *	and the length of the path to it
     *	(in case of execution of pc0 as a.out)
     */
extern char	*err_file;
extern int	err_pathlen;

    /*
     *	the location of the short explanation
     *	and the length of the path to it
     *	the null at the end is so pix can change it to pi'x' from pi.
     */
extern char	*how_file;
extern int	how_pathlen;
extern char	*px_header;
extern char	*pi_comp;
extern char	*px_intrp;
extern char	*px_debug;
