
/********************************************
fcall.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/*$Log:	fcall.c,v $
 * Revision 5.1  91/12/05  07:55:54  brennan
 * 1.1 pre-release
 * 
*/

#include "mawk.h"
#include "symtype.h"
#include "code.h"

/* This file has functions involved with type checking of
   function calls
*/

static  FCALL_REC *PROTO(first_pass, (FCALL_REC *) ) ;
static  CA_REC    *PROTO(call_arg_check, (FBLOCK *, CA_REC *,
        INST *, unsigned) ) ;
static  int PROTO(arg_cnt_ok, (FBLOCK *,CA_REC *, unsigned) ) ;


static int check_progress ;
    /* flag that indicates call_arg_check() was able to type
       check some call arguments */

/* type checks a list of call arguments,
   returns a list of arguments whose type is still unknown
*/
static CA_REC *call_arg_check( callee, entry_list , start,  line_no)
  FBLOCK *callee ;
  CA_REC *entry_list  ;  
  INST  *start ; /* to locate patch */
  unsigned line_no ; /* for error messages */
{ register CA_REC *q ;
  CA_REC *exit_list  = (CA_REC *) 0 ;

  check_progress = 0 ;

  /* loop :
       take q off entry_list
       test it
           if OK  zfree(q)  else put on exit_list
  */
     
  while ( q = entry_list )
  {
    entry_list = q->link ;

    if ( q->type == ST_NONE )
    { /* try to infer the type */
      /* it might now be in symbol table */
      if ( q->sym_p->type == ST_VAR )
      { /* set type and patch */
        q->type = CA_EXPR ;
        start[q->call_offset+1].ptr  = (PTR) q->sym_p->stval.cp ;
      }
      else
      if ( q->sym_p->type == ST_ARRAY )
      { q->type = CA_ARRAY ;
        start[q->call_offset].op = A_PUSHA ;
        start[q->call_offset+1].ptr = (PTR) q->sym_p->stval.array ;
      } 
      else /* try to infer from callee */
      {
        switch( callee->typev[q->arg_num] )
        {
          case  ST_LOCAL_VAR :
                q->type = CA_EXPR ;
                q->sym_p->type = ST_VAR ;
                q->sym_p->stval.cp = new_CELL() ;
                q->sym_p->stval.cp->type = C_NOINIT ;
                start[q->call_offset+1].ptr  = 
                         (PTR) q->sym_p->stval.cp ;
                break ;

          case  ST_LOCAL_ARRAY :
                q->type = CA_ARRAY ;
                q->sym_p->type = ST_ARRAY ;
                q->sym_p->stval.array = new_ARRAY() ;
                start[q->call_offset].op = A_PUSHA ;
                start[q->call_offset+1].ptr = 
                      (PTR) q->sym_p->stval.array ;
                break ;
        }
      }
    }
    else
    if ( q->type == ST_LOCAL_NONE )
    { /* try to infer the type */
      if ( * q->type_p == ST_LOCAL_VAR )
      { /* set type , don't need to patch */
        q->type = CA_EXPR ;
      }
      else
      if ( * q->type_p == ST_LOCAL_ARRAY )
      { q->type = CA_ARRAY ;
        start[q->call_offset].op = LA_PUSHA ;
        /* offset+1 op is OK */
      } 
      else /* try to infer from callee */
      {
        switch( callee->typev[q->arg_num] )
        {
          case  ST_LOCAL_VAR :
                q->type = CA_EXPR ;
                * q->type_p = ST_LOCAL_VAR ;
                /* do not need to patch */
                break ;

          case  ST_LOCAL_ARRAY :
                q->type = CA_ARRAY ;
                * q->type_p = ST_LOCAL_ARRAY ;
                start[q->call_offset].op = LA_PUSHA ;
                break ;
        }
      }
    }

    /* if we still do not know the type put on the new list
       else type check */

    if ( q->type == ST_NONE || q->type == ST_LOCAL_NONE )
    {
      q->link = exit_list ;
      exit_list = q ;
    }
    else  /* type known */
    {
      if ( callee->typev[q->arg_num] == ST_LOCAL_NONE )
           callee->typev[q->arg_num] = q->type ;

      else
      if ( q->type != callee->typev[q->arg_num] )
      {
        errmsg(0, "line %u: type error in arg(%d) in call to %s",
          line_no, q->arg_num+1, callee->name) ;
        if ( ++compile_error_count == MAX_COMPILE_ERRORS )
                    mawk_exit(1) ;
      }

      zfree(q, sizeof(CA_REC)) ;
      check_progress = 1 ;
    }
  } /* while */

  return  exit_list ;
}


static  int  arg_cnt_ok( fbp, q, line_no )
  FBLOCK  *fbp ;
  CA_REC  *q ;
  unsigned line_no ;
{
  if ( q->arg_num  >= fbp->nargs )
  {
    errmsg(0, "line %u: too many arguments in call to %s" ,
       line_no, fbp->name ) ;
    if ( ++compile_error_count == MAX_COMPILE_ERRORS ) 
              mawk_exit(1) ;

    return  0 ;
  }
  else  return 1 ;
}


FCALL_REC  *resolve_list ;
        /* function calls whose arg types need checking 
           are stored on this list */


/* on first pass thru the resolve list
   we check :
      if forward referenced functions were really defined
      if right number of arguments
   and compute call_start which is now known
*/

static  FCALL_REC *first_pass( p )
  register FCALL_REC *p ;
{ FCALL_REC dummy ;
  register FCALL_REC *q = &dummy ; /* trails p */

  q->link = p ;
  while ( p )
  {
    if ( ! p->callee->code )
    { /* callee never defined */
      errmsg(0, "line %u: function %s never defined" ,
          p->line_no, p->callee->name) ;
      if ( ++compile_error_count == MAX_COMPILE_ERRORS ) 
              mawk_exit(1) ;
      /* delete p from list */
      q->link = p->link ;
      /* don't worry about freeing memory, we'll exit soon */
    }
    else  /* note p->arg_list starts with last argument */
    if ( ! p->arg_list  /* nothing to do */  ||
         ! p->arg_cnt_checked && 
         ! arg_cnt_ok(p->callee, p->arg_list, p->line_no) )
    {  q->link = p->link ; /* delete p */ 
       /* the ! arg_list case is not an error so free memory */
       zfree(p, sizeof(FCALL_REC)) ;
    }
    else
    { /* keep p and set call_start */
      q = p ;
      switch ( p->call_scope )
      {
        case SCOPE_MAIN  :
                p->call_start = main_start ;
                break ;

        case SCOPE_BEGIN :
                p->call_start = begin_code.start ;
                break ;

        case SCOPE_END :
                p->call_start = end_code.start ;
                break ;

        case SCOPE_FUNCT :
                p->call_start = p->call->code ;
                break ;
      }
    }
    p = q->link ;
  }
  return  dummy.link ;
}

/* continuously walk the resolve_list making type deductions
   until this list goes empty or no more progress can be made
   (An example where no more progress can be made is at end of file
*/

void  resolve_fcalls()
{ register FCALL_REC *p, *old_list , *new_list ;
  int progress ; /* a flag */

  old_list = first_pass(resolve_list) ;
  new_list = (FCALL_REC *) 0 ;
  progress = 0 ;

  while ( 1 )
  {
    if ( !(p = old_list) )
    { /* flop the lists */
      if ( !(p = old_list = new_list)  /* nothing left */
          || ! progress    /* can't do any more */ )  return ;

      /* reset after flop */
      new_list = (FCALL_REC *) 0 ;  progress = 0 ;
    }

    old_list = p->link ;

    if ( p->arg_list = call_arg_check(p->callee, p->arg_list ,
              p->call_start, p->line_no)  )
    {
       /* still have work to do , put on new_list   */
       progress  |= check_progress ;
       p->link = new_list ;  new_list = p ;
    }
    else  /* done with p */
    {  progress = 1 ;  zfree(p, sizeof(FCALL_REC)) ; }
  }
}

/* the parser has just reduced a function call ;
   the info needed to type check is passed in.  If type checking
   can not be done yet (most common reason -- function referenced
   but not defined), a node is added to the resolve list.
*/
void check_fcall( callee, call_scope, call, arg_list, line_no )
  FBLOCK *callee ;
  int call_scope ;
  FBLOCK  *call ;
  CA_REC  *arg_list ;
  unsigned line_no ;
{
  FCALL_REC *p ;
  INST *call_start ;

  if ( ! callee->code ) 
  { /* forward reference to a function to be defined later */
    p = (FCALL_REC *) zmalloc(sizeof(FCALL_REC)) ;
    p->callee = callee ;
    p->call_scope = call_scope ;
    p->call = call ;
    p->arg_list = arg_list ;
    p->arg_cnt_checked = 0 ;
    p->line_no = line_no ;
    /* add to resolve list */
    p->link = resolve_list ; resolve_list = p ;
  }
  else
  if ( arg_list && arg_cnt_ok( callee, arg_list, line_no ) )
  {
      switch ( call_scope )
      {
        case SCOPE_MAIN  :
                call_start = main_start ;
                break ;

        case SCOPE_BEGIN :
                call_start = begin_code.start ;
                break ;

        case SCOPE_END :
                call_start = end_code.start ;
                break ;

        case SCOPE_FUNCT :
                call_start = call->code ;
                break ;
      }

      /* usually arg_list disappears here and all is well
         otherwise add to resolve list */

      if ( arg_list = call_arg_check(callee, arg_list,
                call_start, line_no) )
      {
            p = (FCALL_REC *) zmalloc(sizeof(FCALL_REC)) ;
            p->callee = callee ;
            p->call_scope = call_scope ;
            p->call = call ;
            p->arg_list = arg_list ;
            p->arg_cnt_checked = 1 ;
            p->line_no = line_no ;
            /* add to resolve list */
            p->link = resolve_list ; resolve_list = p ;
      }
  }
}



/*  example where typing cannot progress

{ f(z) }

function f(x) { print NR }

# this is legal, does something useful, but absurdly written
# We have to design so this works
*/

