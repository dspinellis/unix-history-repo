/* $Header: vssite.h,v 10.2 86/12/01 15:25:54 jg Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology	*/

/* vssite.h
**
** This is where any machine or display specific information should
** be stored so that different sites can have control over where things live.
**
*/

#define	DEFAULT_FONT_PATH	"/usr/new/lib/X/font:~/font"

/* this path currently should have only one component and must end with a
 * slash
 */
#define DEFAULT_APOLLO_FONT_PATH  "/usr/new/lib/X/apfont/"

#define DEFAULT_FONT_SUFFIX	".onx"

