
/********************************************
zmalloc.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	zmalloc.h,v $
 * Revision 5.1  91/12/05  07:59:41  brennan
 * 1.1 pre-release
 * 
*/

/* zmalloc.h */

#ifndef  ZMALLOC_H
#define  ZMALLOC_H

#if ! HAVE_STDLIB_H
char *malloc() , *realloc() ;
void free() ;
#endif


PTR  PROTO( bmalloc, (unsigned) ) ;
void PROTO( bfree, (PTR, unsigned) ) ;
PTR  PROTO( zrealloc , (PTR,unsigned,unsigned) ) ;


#define ZBLOCKSZ    8    
#define ZSHIFT      3


#define zmalloc(size)  bmalloc((((unsigned)size)+ZBLOCKSZ-1)>>ZSHIFT)
#define zfree(p,size)  bfree(p,(((unsigned)size)+ZBLOCKSZ-1)>>ZSHIFT)

#define ZMALLOC(type)  ((type*)zmalloc(sizeof(type)))
#define ZFREE(p)	zfree(p,sizeof(*(p)))


#endif  /* ZMALLOC_H */
