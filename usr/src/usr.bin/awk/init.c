
/********************************************
init.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	init.c,v $
 * Revision 5.2  92/01/09  08:46:14  brennan
 * small change for MSC
 * 
 * Revision 5.1  91/12/05  07:56:07  brennan
 * 1.1 pre-release
 * 
*/


/* init.c */
#include "mawk.h"
#include "code.h"
#include "memory.h"
#include "symtype.h"
#include "init.h"
#include "bi_vars.h"
#include "field.h"

#ifdef THINK_C
#include <console.h>
#endif


static void PROTO( process_cmdline , (int, char **) ) ;
static void PROTO( set_ARGV, (int, char **, int)) ;
static void PROTO( bad_option, (char *)) ;

extern  void PROTO( print_version, (void) ) ;
extern  int  PROTO( is_cmdline_assign, (char*)) ;

#if 0
#if  MSDOS  &&  ! HAVE_REARGV
#include <fcntl.h>
static  void  PROTO(emit_prompt, (void) ) ;
#endif
#endif


void initialize(argc, argv)
  int argc ; char **argv ;
{
  bi_vars_init() ; /* load the builtin variables */
  bi_funct_init() ; /* load the builtin functions */
  kw_init() ; /* load the keywords */
  field_init() ; 

#ifdef THINK_C
  fputc('\n',stderr);   /* (causes Think C console window to appear) */
  SetWTitle( FrontWindow(), "\pMacMAWK" );
  argc = ccommand(&argv);
#endif 

#if   MSDOS  &&  NO_BINMODE==0
  { char *p = getenv("MAWKBINMODE") ;

    if ( p )  set_binmode( atoi(p) ) ;
  }
#endif


  process_cmdline(argc, argv)  ;   

  code_init() ;
  fpe_init() ;
  set_stderr() ;
}

void  compile_cleanup()
/* program has parsed OK, free some memory
   we don't need anymore */
{
   scan_cleanup() ;
   code_cleanup() ;
}

int  dump_code ;  /* if on dump internal code */
int  posix_space_flag ;

#ifdef   DEBUG
int  dump_RE   ;  /* if on dump compiled REs  */
#endif


static void bad_option(s)
  char *s ;
{ errmsg(0, "not an option: %s", s) ; mawk_exit(1) ; }


static void  process_cmdline(argc, argv)
  int argc ;
  char **argv ;
{
  int i , nextarg ;
  char *optarg ;
  PFILE dummy ;  /* starts linked list of filenames */
  PFILE *tail = &dummy ;

  for( i = 1 ; i < argc && argv[i][0] == '-'  ; i = nextarg )
  {
    if ( argv[i][1] == 0 )  /* -  alone */
    {
      if ( ! pfile_name ) 
      { errmsg(0, "no program") ; exit(1) ; }
      break ; /* the for loop */
    }
    /* safe to look at argv[i][2] */

    if ( argv[i][2] == 0 )
    {
      if ( i == argc-1 && argv[i][1] != '-' )
      { 
        if ( strchr("WFvf", argv[i][1]) )
        { errmsg(0, "option %s lacks argument", argv[i]) ;
          mawk_exit(1) ;
        }
        bad_option(argv[i]) ;
      }

      optarg = argv[i+1] ;
      nextarg = i+2 ;
    }
    else /* argument glued to option */
    {
      optarg = &argv[i][2] ;
      nextarg = i+1 ;
    }

    switch( argv[i][1] )
    {
       case 'W' : 

	    if ( optarg[0] >= 'a' && optarg[0] <= 'z')
		 optarg[0] += 'A' - 'a' ;
            if ( optarg[0] == 'V' ) print_version() ;
	    else
            if ( optarg[0] == 'D' )
	    { dump_code = 1 ;
#if  SM_DOS
	      errmsg(0, "-W dump option unavailable in small model") ;
	      dump_code=0 ;
#endif
	    }
	    else
	    if ( optarg[0] == 'S' )
	    {
	      char *p = strchr(optarg,'=') ;
	      int x = p ? atoi(p+1) : 0 ;

	      if ( x > SPRINTF_SZ )
	      {
		sprintf_buff = (char *) zmalloc(x) ;
		sprintf_limit = sprintf_buff + x ;
	      }
	    }
#if  MSDOS  &&  NO_BINMODE==0
	    else
	    if ( optarg[0] == 'B' )
	    {
	      char *p = strchr(optarg,'=') ;
	      int x = p ? atoi(p+1) : 0 ;

	      set_binmode(x) ;
	    }
#endif
	    else
	    if ( optarg[0] == 'P' )
	    {
	      posix_space_flag = 1 ;
	    }
	    else
	      errmsg(0,"vacuous option: -W %s", optarg) ;
	      

            break ;

       case 'v' :  
            if ( ! is_cmdline_assign(optarg) ) 
            { errmsg(0 , "improper assignment: -v %s" , optarg) ;
              exit(1) ;
            }
            break ;

       case 'F' :

            rm_escape(optarg) ; /* recognize escape sequences */
            cell_destroy(FS) ;
            FS->type = C_STRING ;
            FS->ptr = (PTR) new_STRING(optarg) ;
            cast_for_split( cellcpy(&fs_shadow, FS) ) ;
            break ;

       case '-' :
            if ( argv[i][2] != 0 )  bad_option(argv[i]) ;
            i++ ;
            goto  no_more_opts ;

       case 'f' :
	    /* first file goes in pfile_name ; any more go
	       on a list */
            if ( ! pfile_name )  pfile_name = optarg ;
            else 
            { tail = tail->link = ZMALLOC(PFILE) ;
              tail->fname = optarg ;
            }
            break ;

       default :  bad_option(argv[i]) ;

    }

    i = nextarg ;
  }

no_more_opts:
  
  tail->link = (PFILE*) 0 ;
  pfile_list = dummy.link ;

  if ( pfile_name )
  {
    set_ARGV(argc, argv, i) ;
    scan_init( (char *) 0 ) ;
  }
  else /* program on command line */
  {
#if 0
#if MSDOS && ! HAVE_REARGV  /* prompt for program */
    set_ARGV(argc, argv, i) ;
    emit_prompt() ;
    pfile_name = "CON" ;
    scan_init( (char *) 0 ) ;
#else /* the real world */
#endif
#endif /* 0 */
    if ( i == argc )
    { errmsg(0, "no program") ; mawk_exit(1) ; }
    set_ARGV(argc, argv, i+1) ;

#if  MSDOS && ! HAVE_REARGV  /* reversed quotes */
    { char *p ;

      for( p = argv[i] ; *p ; p++ )
	 if ( *p == '\'' )  *p = '\"' ;
    }
#endif
    scan_init( argv[i]) ;
/* #endif  */
  }
}


static void set_ARGV(argc, argv, i)
  int argc ; char **argv ;
  int i ; /* argv[i] = ARGV[1] */
{
  SYMTAB *st_p ;
  CELL argi ;
  register CELL *cp ;

  st_p = insert( "ARGV" ) ;
  st_p->type = ST_ARRAY ;
  Argv = st_p->stval.array = new_ARRAY() ;
  argi.type = C_DOUBLE ;
  argi.dval = 0.0 ;
  cp = array_find( st_p->stval.array, &argi, CREATE) ;
  cp->type = C_STRING ;
  cp->ptr = (PTR) new_STRING( progname ) ;

  /* ARGV[0] is set, do the rest 
     The type of ARGV[1] ... should be C_MBSTRN
     because the user might enter numbers from the command line */

    for( argi.dval = 1.0 ; i < argc ; i++, argi.dval += 1.0 )
    { 
      cp = array_find( st_p->stval.array, &argi, CREATE) ;
      cp->type = C_MBSTRN ;
      cp->ptr = (PTR) new_STRING( argv[i] ) ;
    }
    ARGC->type = C_DOUBLE ;
    ARGC->dval = argi.dval ;
}

#if 0
#if  MSDOS  &&  ! HAVE_REARGV

static void  emit_prompt()
{  static char prompt[] = "mawk> " ;
   int fd = open("CON", O_WRONLY, 0) ;

   (void) write(fd, prompt, strlen(prompt)) ;
   (void) close(fd) ;
}
#endif
#endif


/*----- ENVIRON ----------*/

void load_environ( ENV )
  ARRAY ENV ;
{
  CELL c ;
#ifndef  MSDOS_MSC  /* MSC declares it near */
  extern char **environ ;
#endif
  register char **p = environ ; /* walks environ */
  char *s ; /* looks for the '=' */
  CELL *cp ; /* pts at ENV[&c] */

  c.type = C_STRING ;

  while ( *p )
  {
    if ( s = strchr(*p, '=') ) /* shouldn't fail */
    {
      *s = 0 ;
      c.ptr = (PTR) new_STRING(*p) ;
      *s++ = '=' ;

      cp = array_find(ENV, &c, CREATE) ;
      cp->type = C_MBSTRN ;
      cp->ptr = (PTR) new_STRING(s) ;

      free_STRING(string(&c)) ;
    }
    p++ ;
  }
}
