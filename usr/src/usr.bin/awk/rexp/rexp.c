
/********************************************
rexp.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	rexp.c,v $
 * Revision 3.4  91/08/13  09:09:59  brennan
 * VERSION .9994
 * 
 * Revision 3.3  91/08/04  15:45:03  brennan
 * no longer attempt to recover mem on failed REcompile
 * Its not worth the effort
 * 
 * Revision 3.2  91/08/03  07:24:06  brennan
 * check for empty machine stack (missing operand) wasn't quite right
 * 
 * Revision 3.1  91/06/07  10:33:16  brennan
 * VERSION 0.995
 * 
 * Revision 1.7  91/06/05  08:58:47  brennan
 * change RE_free to free
 * 
 * Revision 1.6  91/06/03  07:07:17  brennan
 * moved parser stacks inside REcompile
 * removed unnecessary copying
 * 
*/

/*  op precedence  parser for regular expressions  */

#include  "rexp.h"


/*  DATA   */
int   REerrno ;
char *REerrlist[] = { (char *) 0 ,
/* 1  */    "missing '('",
/* 2  */    "missing ')'",
/* 3  */    "bad class -- [], [^] or [" ,
/* 4  */    "missing operand" ,
/* 5  */    "resource exhaustion -- regular expression too large"
} ;
/* E5 is very unlikely to occur */

/* This table drives the operator precedence parser */
static  short  table[8][8]  =  {

/*        0   |   CAT   *   +   ?   (   )   */
/* 0 */   0,  L,  L,    L,  L,  L,  L,  E1,
/* | */   G,  G,  L,    L,  L,  L,  L,  G,
/* CAT*/  G,  G,  G,    L,  L,  L,  L,  G,
/* * */   G,  G,  G,    G,  G,  G, E7,  G,
/* + */   G,  G,  G,    G,  G,  G, E7,  G,
/* ? */   G,  G,  G,    G,  G,  G, E7,  G,
/* ( */   E2, L,  L,    L,  L,  L,  L,  EQ,
/* ) */   G , G,  G,    G,  G,  G,  E7,  G     }   ;


#define  STACKSZ   64


static jmp_buf  err_buf  ;  /*  used to trap on error */

void  RE_error_trap(x)  /* return is dummy to make macro OK */
  int x ;
{
  REerrno = x ;
  longjmp(err_buf, 1 ) ;
}


VOID *REcompile(re)
  char *re ;
{ 
  MACHINE  m_stack[STACKSZ] ;
  struct op {
    int token ;
    int prec ;
  }  op_stack[STACKSZ] ;
  register MACHINE *m_ptr  ;
  register struct op *op_ptr ;
  register int  t ;

  /* do this first because it also checks if we have a
     run time stack */
  RE_lex_init(re) ;

  if ( *re == 0 )
  { STATE *p = (STATE *) RE_malloc( sizeof(STATE) ) ;
    p->type = M_ACCEPT ;
    return  (VOID *) p ;
  }

  if ( setjmp(err_buf) )  return (VOID *) 0 ;
  /* we used to try to recover memory left on machine stack ;
     but now m_ptr is in a register so it won't be right unless
     we force it out of a register which isn't worth the trouble */

  /* initialize the stacks  */
  m_ptr = m_stack - 1 ;
  op_ptr = op_stack ;
  op_ptr->token = 0 ;

  t = RE_lex(m_stack) ;

  while( 1 )
   { switch( t )
       { 
         case T_STR  :
         case T_ANY  :
         case T_U    :
         case T_START :
         case T_END :
         case T_CLASS :   m_ptr++ ; break ;

         case  0 :   /*  end of reg expr   */
           if ( op_ptr -> token == 0 )  /*  done   */
               if ( m_ptr == m_stack )  return (VOID *)m_ptr->start ;
               else
               /*  machines still on the stack  */
               RE_panic("values still on machine stack") ;
           
         /*  otherwise  fall  thru to default
             which is operator case  */

         default:

           if ( (op_ptr -> prec = table[op_ptr -> token][t]) == G )
           { 
             do
             {  /* op_pop   */

                if ( op_ptr->token <= T_CAT ) /*binary op*/ m_ptr-- ;
		/* if not enough values on machine stack
		   then we have a missing operand */
                if ( m_ptr < m_stack )  RE_error_trap(-E4) ;

                switch( op_ptr->token )
                {  case  T_CAT :  RE_cat(m_ptr, m_ptr+1) ;  break ;
                   case  T_OR  :  RE_or( m_ptr, m_ptr+1) ;  break ;
                   case T_STAR  :  RE_close( m_ptr) ;  break ;
                   case T_PLUS  :  RE_poscl( m_ptr ) ; break ;
                   case T_Q     :  RE_01( m_ptr ) ;    break ;
                   default       :  break ; /*nothing on ( or ) */
                }

                op_ptr-- ;
            }
            while ( op_ptr->prec != L ) ;

            continue ;  /* back thru switch at top */
          }

          if ( op_ptr -> prec < 0 )
              if ( op_ptr->prec == E7 ) 
                  RE_panic("parser returns E7") ;
              else  RE_error_trap(-op_ptr->prec) ;

          if ( ++op_ptr == op_stack + STACKSZ ) /* stack overflow */
                 RE_error_trap(-E5) ;
          op_ptr -> token = t ;
       } /* end of switch */

    if ( m_ptr == m_stack+(STACKSZ-1) ) /*overflow*/ 
                       RE_error_trap(-E5) ;
    t = RE_lex(m_ptr+1) ;
  }
}


/* getting here means a logic flaw or unforeseen case */
void RE_panic( s )
  char *s ;
{ fprintf( stderr, "REcompile() - panic:  %s\n", s) ;
  exit(100) ; }

