/* $RCSfile: util.h,v $$Revision: 4.0.1.2 $$Date: 91/11/05 19:18:40 $
 *
 *    Copyright (c) 1991, Larry Wall
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 * $Log:	util.h,v $
 * Revision 4.0.1.2  91/11/05  19:18:40  lwall
 * patch11: safe malloc code now integrated into Perl's malloc when possible
 * 
 * Revision 4.0.1.1  91/06/07  12:11:00  lwall
 * patch4: new copyright notice
 * 
 * Revision 4.0  91/03/20  01:56:48  lwall
 * 4.0 baseline.
 * 
 */

EXT int *screamfirst INIT(Null(int*));
EXT int *screamnext INIT(Null(int*));

#ifndef safemalloc
char	*safemalloc();
char	*saferealloc();
#endif
char	*cpytill();
char	*instr();
char	*fbminstr();
char	*screaminstr();
void	fbmcompile();
char	*savestr();
/* void	setenv(); */
int	envix();
void	growstr();
char	*ninstr();
char	*rninstr();
char	*nsavestr();
FILE	*mypopen();
int	mypclose();
#ifndef HAS_MEMCPY
#ifndef HAS_BCOPY
char	*bcopy();
#endif
#ifndef HAS_BZERO
char	*bzero();
#endif
#endif
unsigned long scanoct();
unsigned long scanhex();
