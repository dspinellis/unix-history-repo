
/********************************************
zmalloc.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	zmalloc.c,v $
 * Revision 5.1  91/12/05  07:56:35  brennan
 * 1.1 pre-release
 * 
*/

/*  zmalloc.c  */
#include  "mawk.h"
#include  "zmalloc.h"

void PROTO( mawk_exit, (int) ) ;

extern  struct yacc_mem  *yacc_memp ;

/*
  zmalloc() gets mem from malloc() in CHUNKS of 2048 bytes
  and cuts these blocks into smaller pieces that are multiples
  of eight bytes.  When a piece is returned via zfree(), it goes
  on a linked linear list indexed by its size.  The lists are
  an array, pool[].

  E.g., if you ask for 22 bytes with p = zmalloc(22), you actually get
  a piece of size 24.  When you free it with zfree(p,22) , it is added
  to the list at pool[2].
*/

#define POOLSZ      16

#define  CHUNK          256    
        /* number of blocks to get from malloc */

static PTR  PROTO( emalloc, (unsigned) ) ;
void PROTO( errmsg, (int , char *, ...) ) ;

static PTR emalloc(size)
  unsigned size ;
{ PTR p ;
  static char out[] = "out of memory" ;

  if( !(p = (PTR) malloc(SIZE_T(size))) )
	if ( mawk_state == EXECUTION ) rt_error(out) ;
	else /* I don't think this will ever happen */
	{ compile_error(out) ; mawk_exit(1) ; }
  return p ;
}


typedef  union  zblock {
char dummy[ZBLOCKSZ] ;
union zblock *link ;
}  ZBLOCK  ;

/* ZBLOCKS of sizes 1, 2, ... 16
   which is bytes of sizes 8, 16, ... , 128
   are stored on the linked linear lists in
   pool[0], pool[1], ... , pool[15]
*/

static  ZBLOCK  *pool[POOLSZ] ;

PTR   bmalloc( blocks )
  register unsigned blocks ;
{ 
  register ZBLOCK *p ;
  static  unsigned amt_avail ;
  static  ZBLOCK  *avail ;

  if ( blocks > POOLSZ )  return emalloc(blocks<<ZSHIFT) ;

  if ( p = pool[blocks-1] )
  { pool[blocks-1] = p->link ; return (PTR) p ; }

  if ( blocks > amt_avail )
  { if ( amt_avail ) /* free avail */
    { avail->link = pool[--amt_avail] ; pool[amt_avail] = avail ; }

    /* use parser tables first */
    if ( yacc_memp->zblocks >= blocks )
    { avail = (ZBLOCK *) yacc_memp->mem ;
      amt_avail = yacc_memp++ -> zblocks ;
      /* make sure its -- aligned */
      if ( (int) avail & 7 )
      { avail = (ZBLOCK*)((char *)avail + 8 - ((int)avail&7)) ;
	amt_avail-- ;
      }
    }
    else
    if ( !(avail = (ZBLOCK *) malloc(SIZE_T(CHUNK*ZBLOCKSZ))) )
    { /* if we get here, almost out of memory */
        amt_avail = 0 ;   
	return  emalloc(blocks << ZSHIFT) ;
    }
    else  amt_avail = CHUNK ;
  }
  
  /* get p from the avail pile */
  p = avail ; avail += blocks ; amt_avail -= blocks ; 
  return (PTR) p ;
}

void  bfree( p, blocks)
  register PTR p ;  
  register unsigned blocks ;
{ 

  if ( blocks > POOLSZ )  free(p) ;
  else
  {
    ((ZBLOCK *)p)->link = pool[--blocks] ;
    pool[blocks] = (ZBLOCK *) p ;
  }
}

PTR  zrealloc( p, old_size, new_size )
  register PTR  p ;
  unsigned old_size, new_size ;
{ register PTR q ;

  (void) memcpy(q = zmalloc(new_size), p, 
                SIZE_T(old_size < new_size ? old_size : new_size)) ;
 
  zfree(p, old_size) ;
  return q ;
}


