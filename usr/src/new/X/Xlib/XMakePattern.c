/* $Header: XMakePattern.c,v 1.1 86/04/22 15:15:16 jg Rel $ */

/* 
 * XMakePattern.c - Substitute for macro in the library
 * 
 * Author:	Scott Nettles
 * 		Digital Equipment Corporation
 * 		Western Research Laboratory
 * Date:	Sat Aug 24 1985
 */

/* $Log:	XMakePattern.c,v $
 * Revision 1.1  86/04/22  15:15:16  jg
 * Changes to hide protocol better for BIGSHORTS
 * 
 * Revision 1.1  85/12/05  13:34:57  nettles
 * Initial revision
 *  */

static char rcs_ident[] = "$Header: XMakePattern.c,v 1.1 86/04/22 15:15:16 jg Rel $";

typedef long Pattern;

Pattern XMakePattern(pattern, patlen, patmul)
int	pattern, patlen, patmul;
{
    return	((Pattern)(((patmul) << 20) | (((patlen) - 1) << 16) | (pattern) ));
}
