
/********************************************
fin.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	fin.c,v $
 * Revision 5.2  92/02/21  13:30:08  brennan
 * fixed bug that free'd FILENAME twice if 
 * command line was var=value only
 * 
 * Revision 5.1  91/12/05  07:56:02  brennan
 * 1.1 pre-release
 * 
*/

/* fin.c */

#include "mawk.h"
#include "fin.h"
#include "memory.h"
#include "bi_vars.h"
#include "field.h"
#include "symtype.h"
#include "scan.h"

#if  HAVE_FCNTL_H
#include <fcntl.h>
#endif

/* This file handles input buffering */

#ifndef MSDOS_MSC
extern int errno ;
#endif
int PROTO(isatty, (int) ) ;

static FIN *PROTO( next_main, (int) ) ;
static char *PROTO( enlarge_fin_buffer, (FIN *) ) ;
static void PROTO(set_main_to_stdin, (void) ) ;
int  PROTO( is_cmdline_assign, (char *) ) ; /* also used by init */

FIN  *FINdopen( fd, main_flag )
  int fd , main_flag ;
{ register FIN *fin = (FIN *) zmalloc( sizeof(FIN) ) ;

  fin->fd = fd ;
  fin->flags = main_flag ? (MAIN_FLAG|START_FLAG) : START_FLAG ;
  fin->buffp = fin->buff = (char *) zmalloc(BUFFSZ+1) ;
  fin->nbuffs = 1 ;
  fin->buff[0] = 0 ;

  if ( isatty(fd) && rs_shadow.type == SEP_CHAR 
       && rs_shadow.c == '\n' )
  {
    /* interactive, i.e., line buffer this file */
    if ( fd == 0 ) fin->fp = stdin ;
    else
    if ( !(fin->fp = fdopen(fd, "r")) )
    { errmsg(errno, "fdopen failed") ; exit(1) ; }
  }
  else  fin->fp = (FILE *) 0 ;

  return fin ;
}

FIN  *FINopen( filename, main_flag )
  char *filename ;
  int main_flag ;
{ int fd ;
  int oflag = O_RDONLY ;

#if  MSDOS && NO_BINMODE==0
  int bm = binmode() & 1 ;
  if ( bm ) oflag |= O_BINARY ;
#endif

  if ( filename[0] == '-' && filename[1] == 0 )
  {
#if  MSDOS  &&  NO_BINMODE==0
     if ( bm )  setmode(0, O_BINARY) ;
#endif
      return  FINdopen(0, main_flag) ;
  }

#ifdef THINK_C
  if ( (fd = open( filename , oflag )) == -1 )
#else
  if ( (fd = open( filename , oflag, 0 )) == -1 )
#endif
  { errmsg( errno, "cannot open %s" , filename ) ;
    return (FIN *) 0 ; }

  return  FINdopen( fd, main_flag ) ;
}

/* frees the buffer and fd, but leaves FIN structure until
   the user calls close() */

void FINsemi_close(fin)
  register FIN *fin ;
{ static char dead  = 0 ;

  if ( fin->buff != &dead ) 
  {
    zfree(fin->buff, fin->nbuffs*BUFFSZ + 1) ;

    if ( fin->fd )
	if ( fin->fp )  (void) fclose(fin->fp) ;
	else   (void) close(fin->fd) ;

    fin->buff = fin->buffp = &dead  ; /* marks it semi_closed */
  }
  /* else was already semi_closed */
}

/* user called close() on input file */
void  FINclose( fin )
  FIN *fin ;
{
  FINsemi_close(fin) ;
  zfree( fin , sizeof(FIN) ) ;
}

/* return one input record as determined by RS,
   from input file (FIN)  fin
*/

char *FINgets( fin, len_p )
  FIN *fin ;
  unsigned *len_p ;
{ register char *p, *q ;
  unsigned match_len ;
  unsigned r ;

restart :

  if ( ! (p = fin->buffp)[0] )  /* need a refill */
  { 
    if ( fin->flags & EOF_FLAG )
        if ( fin->flags & MAIN_FLAG )
	{ fin = next_main(0) ;  goto restart ; }
        else
        { *len_p = 0 ; return (char *) 0 ; }
        
    if ( fin->fp ) /* line buffering */
      if ( ! fgets(fin->buff, BUFFSZ+1, fin->fp) )
      {
        fin->flags |= EOF_FLAG ;
        fin->buff[0] = 0 ;
        fin->buffp = fin->buff ;
        goto restart ;  /* might be main_fin */
      }
      else  /* return this line */
      {
        if ( !(p = strchr(fin->buff, '\n')) )
             p = fin->buff + BUFFSZ + 1 ; /* unlikely to occur */

        *p = 0 ; *len_p = p - fin->buff ;
        fin->buffp = p ;
#ifdef THINK_C
	/*
	 *  I still don't understand why this is needed, unless fgets()
	 *  also does this conversion internally for no good reason.  :-(
	 */
        for ( p = fin->buff ; *p ; ++p )
	    {
	    if (*p == '\n')       *p = '\r';
	    else if (*p == '\r')  *p = '\n';
	    }
#endif
        return  fin->buff ;
      }
    else  /* block buffering */
    {
      if ( (r = fillbuff(fin->fd, fin->buff, fin->nbuffs*BUFFSZ)) == 0 )
      {
        fin->flags |= EOF_FLAG ;
        fin->buffp = fin->buff ;
        goto restart ; /* might be main */
      }
      else
      if ( r < fin->nbuffs*BUFFSZ )  fin->flags |= EOF_FLAG ;

      p = fin->buffp = fin->buff ;

      if ( fin->flags & START_FLAG )
      {
	fin->flags &= ~START_FLAG ;
	if ( rs_shadow.type == SEP_MLR  )
	{ /* trim blank lines from front of file */
	  while ( *p == '\n' ) p++ ;
	  fin->buffp = p ;
	  if ( *p == 0 )  goto  restart ;
	}
      }
    }
  }

retry: 

  switch( rs_shadow.type )
  {
    case SEP_CHAR :
            q = strchr(p, rs_shadow.c) ;
            match_len = 1 ;
            break ;

    case SEP_STR  :
            q = str_str(p, ((STRING *) rs_shadow.ptr)->str,
                match_len = ((STRING *) rs_shadow.ptr)->len ) ;
            break ;

    case SEP_MLR :
    case SEP_RE :
            q = re_pos_match(p, rs_shadow.ptr, &match_len) ;
            /* if the match is at the end, there might still be
	       more to match in the file */
            if ( q && q[match_len] == 0 && ! (fin->flags & EOF_FLAG))
		 q = (char *) 0 ;
            break ;
            
    default :
            bozo("type of rs_shadow") ;
  }

  if ( q )
  {  /* the easy and normal case */
     *q = 0 ;  *len_p = q - p ;
     fin->buffp = q + match_len  ;
     return p ;
  }

  if ( fin->flags & EOF_FLAG )
  { /* last line without a record terminator */
    *len_p = r = strlen(p) ; fin->buffp = p+r ;

    if ( rs_shadow.type == SEP_MLR && fin->buffp[-1] == '\n' 
	 && r != 0  )
    { (*len_p)-- ; * -- fin->buffp = 0 ; }
    return p ;
  }

  if ( p == fin->buff )  
  { /* current record is too big for the input buffer, grow buffer */
    p = enlarge_fin_buffer(fin) ;
  }
  else
  {
    /* move a partial line to front of buffer and try again */
    unsigned rr ;

    p = (char *) memcpy( fin->buff, p, SIZE_T(r = strlen(p)) ) ;
    q = p+r ;  rr = fin->nbuffs*BUFFSZ - r ;

    if ( (r = fillbuff(fin->fd, q, rr)) < rr ) fin->flags |= EOF_FLAG ;
  }
  goto retry ;
}

static char *enlarge_fin_buffer(fin)
  FIN *fin ;
{
  unsigned r ;
  unsigned oldsize = fin->nbuffs*BUFFSZ+1 ;

  fin->buffp = 
  fin->buff = (char *) zrealloc(fin->buff, oldsize, oldsize+BUFFSZ);
  fin->nbuffs++ ;

  r = fillbuff(fin->fd, fin->buff + (oldsize-1) , BUFFSZ ) ;
  if ( r < BUFFSZ ) fin->flags |= EOF_FLAG ;

  return fin->buff ;
}

/*--------
  target is big enough to hold size + 1 chars 
  on exit the back of the target is zero terminated
 *--------------*/
unsigned  fillbuff(fd, target, size)
  int fd ;
  register char *target ;
  unsigned size ;
{ register int r ;
  unsigned entry_size = size ;
#ifdef THINK_C
  register char *p = target;
#endif

  while ( size )
    switch( r = read(fd, target, size) )
    { case -1 :
        errmsg(errno, "read error") ;
        exit(1) ;

      case 0 :
        goto out ;

      default :
        target += r ; size -= r ;
        break ;
    }

out :
  *target = 0 ;
#ifdef THINK_C
  /*
   *  I still don't understand why this is needed, unless read() also does
   *  this conversion internally for no good reason.  :-(
   */
  for ( ; *p ; ++p )
    {
      if (*p == '\r')
	*p = '\n';
      else if (*p == '\n')
	*p = '\r';
    }
#endif
  return  entry_size - size ;
}

/* main_fin is a handle to the main input stream
   == 0  never been opened   */

FIN *main_fin ;
ARRAY   Argv ;   /* to the user this is ARGV  */
static double argi = 1.0 ;  /* index of next ARGV[argi] to try to open */


static void  set_main_to_stdin()
{
    cell_destroy( FILENAME ) ;
    FILENAME->type = C_STRING ;
    FILENAME->ptr = (PTR) new_STRING( "-") ;
    main_fin = FINdopen(0, 1) ;
}
   

void  open_main()  
{ CELL argc ;

#if  MSDOS && NO_BINMODE==0   /* set input modes */
  int k = binmode() ;

  if ( k & 1 )  setmode(0, O_BINARY) ;
  if ( k & 2 ) { setmode(1,O_BINARY) ; setmode(2,O_BINARY) ; }
#endif

  (void) cellcpy(&argc, ARGC) ;
  if ( argc.type != C_DOUBLE ) cast1_to_d(&argc) ;

  if ( argc.dval == 1.0 )  set_main_to_stdin() ;
  else  (void)  next_main(1) ;
}

static  FIN  *next_main(open_flag)
  int open_flag ; /* called by open_main() if on */
{ 
  register CELL *cp ;
  CELL   argc ;  /* copy of ARGC */
  CELL   c_argi ; /* cell copy of argi */
  CELL   argval ;    /* copy of ARGV[c_argi] */


  argval.type = C_NOINIT ;
  c_argi.type = C_DOUBLE ;

  if ( main_fin )  FINclose(main_fin) ;
  cell_destroy( FILENAME ) ;
  FILENAME->type = C_NOINIT ; 
     /* so don't free again if we go to set_main_to_stdin() */
  cell_destroy( FNR ) ;
  FNR->type = C_DOUBLE ;
  FNR->dval = 0.0 ;

  if ( cellcpy(&argc, ARGC)->type != C_DOUBLE )
          cast1_to_d(&argc) ;
  
  while ( argi < argc.dval )
  {
    c_argi.dval = argi ;
    argi += 1.0 ;

    if ( !(cp = array_find( Argv, &c_argi, NO_CREATE)) )
        continue ; /* its deleted */

    /* make a copy so we can cast w/o side effect */
    cell_destroy(&argval) ;
    cp = cellcpy(&argval, cp) ;
    if ( cp->type < C_STRING )  cast1_to_s(cp) ;
    if ( string(cp)->len == 0 )  continue ;

    /* it might be a command line assignment */
    if ( is_cmdline_assign(string(cp)->str) )  continue ;

    /* try to open it -- we used to continue on failure, 
       but posix says we should quit */
    if ( ! (main_fin = FINopen( string(cp)->str, 1 )) ) mawk_exit(1) ;

    /* success */
    (void) cellcpy(FILENAME , cp ) ;
    free_STRING( string(cp) ) ;
    return  main_fin ;
  }
  /* failure */
  cell_destroy(&argval) ;

  if ( open_flag )  /* all arguments were null or assignment */
  { set_main_to_stdin() ;  return  main_fin ; }
    
  /* real failure */
  FILENAME->type = C_STRING ;
  FILENAME->ptr = (PTR) new_STRING( "" ) ;

  { /* this is how we mark EOF on main_fin  */
    static char dead_buff = 0 ;
    static FIN  dead_main = {0, (FILE*)0, &dead_buff, &dead_buff,
       1, EOF_FLAG} ;

    return  main_fin = &dead_main ;
    /* since MAIN_FLAG is not set, FINgets won't call next_main() */
  }
}
    

int is_cmdline_assign(s)
  char *s ;
{ 
  register char *p ;
  int c ;
  SYMTAB *stp ;
  CELL *cp ;
  unsigned len ;
  CELL cell ; /* used if command line assign to pseudo field */
  CELL *fp = (CELL *) 0 ; /* ditto */

  if ( scan_code[*(unsigned char *)s] != SC_IDCHAR ) return 0 ;

  p = s+1 ;
  while ( (c = scan_code[*(unsigned char*)p]) == SC_IDCHAR 
	  || c == SC_DIGIT ) p++ ;

  if ( *p != '=' )  return 0 ;

  *p = 0 ;
  stp = find(s) ;

  switch( stp->type )
  {
    case  ST_NONE :
        stp->type = ST_VAR ;
        stp->stval.cp = cp = new_CELL() ;
        break ;

    case  ST_VAR :
    case  ST_NR : /* !! no one will do this */
        cp = stp->stval.cp ;
	cell_destroy(cp) ;
        break ;

    case  ST_FIELD :
	/* must be pseudo field */
	fp = stp->stval.cp ;
	cp = &cell ;
	break ;

    default :
        rt_error(
          "cannot command line assign to %s\n\ttype clash or keyword" 
          , s ) ;
  }
  
  /* we need to keep ARGV[i] intact */
  *p++ = '=' ;
  len = strlen(p)+1 ;
  /* posix says escape sequences are on from command line */
  p = rm_escape( strcpy((char*)zmalloc(len), p) ) ;
  cp->ptr = (PTR) new_STRING(p) ;
  zfree(p,len) ;
  check_strnum(cp) ;  /* sets cp->type */
  if ( fp ) /* move it from cell to pfield[] */
  { field_assign(fp, cp) ; free_STRING(string(cp)) ; }
  return 1 ;
}
