
/********************************************
execute.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	execute.c,v $
 * Revision 5.1  91/12/05  07:55:50  brennan
 * 1.1 pre-release
 * 
*/


#include "mawk.h"
#include "code.h"
#include "memory.h"
#include "symtype.h"
#include "field.h"
#include "bi_funct.h"
#include "bi_vars.h"
#include "regexp.h"
#include "repl.h"
#include "fin.h"
#include <math.h>

/* static functions */
static int PROTO( compare, (CELL *) ) ;
static void PROTO( eval_overflow, (void) ) ;


#if   NOINFO_SIGFPE
static char dz_msg[] = "division by zero" ;
#endif

#ifdef   DEBUG
#define  inc_sp()   if( ++sp == eval_stack+EVAL_STACK_SIZE )\
                         eval_overflow()
#else

/* If things are working, the eval stack should not overflow */

#define inc_sp()    sp++
#endif

#define  SAFETY    16
#define  DANGER    (EVAL_STACK_SIZE-SAFETY)

/*  The stack machine that executes the code */

CELL  eval_stack[EVAL_STACK_SIZE] ;
/* these can move for deep recursion */
static CELL  *stack_base = eval_stack ;
static CELL  *stack_danger = eval_stack + DANGER ;      

#ifdef  DEBUG
static void eval_overflow()
{ overflow("eval stack" , EVAL_STACK_SIZE) ; mawk_exit(1) ; }
#endif

static INST *restart_label ; /* control flow labels */
INST *next_label ;
static CELL tc ; /*useful temp */

void  execute(cdp, sp, fp)
  register INST *cdp ;  /* code ptr, start execution here */
  register CELL *sp ;   /* eval_stack pointer */
  CELL *fp ;            /* frame ptr into eval_stack for
                           user defined functions */
{ 
  /* some useful temporaries */
  CELL *cp ;
  int t ;

  /* for moving the stack (deep recursion) */
  CELL *old_stack_base ;
  CELL *old_sp ;

#ifdef  DEBUG
  CELL *entry_sp = sp ;
#endif


  if ( fp )  /* we are a function call, check for deep recursion */
  {
    if (sp > stack_danger)
    { /* change stacks */
      old_stack_base = stack_base ;
      old_sp = sp ;
      stack_base = (CELL *) zmalloc(sizeof(CELL)*EVAL_STACK_SIZE) ;
      stack_danger = stack_base + DANGER ;
      sp = stack_base ;
	  /* waste 1 slot for ANSI, actually LM_DOS breaks in
	     RET if we don't */
#ifdef  DEBUG 
      entry_sp = sp ;
#endif
    }
    else old_stack_base = (CELL*) 0 ;
  }

  while ( 1 )
    switch( cdp++ -> op )
    {   

/* HALT only used by the disassemble now ; this remains
   so compilers don't offset the jump table */
        case  _HALT :

        case  _STOP :  /* only for range patterns */
#ifdef  DEBUG
                if ( sp != entry_sp+1 ) bozo("stop0") ;
#endif
                return ;

        case  _PUSHC :  
            inc_sp() ;
            (void) cellcpy(sp, cdp++ -> ptr) ;
            break ;

        case _PUSHD  :
            inc_sp() ;
            sp->type = C_DOUBLE ;
            sp->dval = *(double*) cdp++->ptr ;
            break ;

        case  _PUSHS :
            inc_sp() ;
            sp->type = C_STRING ;
            sp->ptr = cdp++->ptr ;
            string(sp)->ref_cnt++ ;
            break ;

        case  F_PUSHA :
            if ( (CELL*)cdp->ptr != field && nf < 0 ) split_field0() ;
            /* fall thru */

        case  _PUSHA :
        case  A_PUSHA :
            inc_sp() ;
            sp -> ptr = cdp++ -> ptr ;
            break ;

        case _PUSHI :  /* put contents of next address on stack*/
            inc_sp() ;
            (void) cellcpy(sp, cdp++ -> ptr) ;
            break ;
            
        case L_PUSHI :  
            /* put the contents of a local var on stack,
               cdp->op holds the offset from the frame pointer */
            inc_sp() ;
            (void) cellcpy(sp, fp + cdp++->op) ;
            break ;

        case L_PUSHA : /* put a local address on eval stack */
            inc_sp() ;
            sp->ptr = (PTR)(fp + cdp++->op) ;
            break ;


        case F_PUSHI :

        /* push contents of $i 
           cdp[0] holds & $i , cdp[1] holds i */

            inc_sp() ;
            if ( nf < 0 )  split_field0() ;
            cp = (CELL *) cdp->ptr ;
            t =  (cdp+1)->op ;
            cdp += 2 ;

            if ( t <= nf ) (void) cellcpy(sp, cp) ;
            else  /* an unset field */
            { sp->type = C_STRING ;
              sp->ptr = (PTR) & null_str ;
              null_str.ref_cnt++ ;
            }
            break ;

        case NF_PUSHI :

            inc_sp() ;
            if ( nf < 0 ) split_field0() ;
            (void) cellcpy(sp, NF) ;
            break ;

        case  FE_PUSHA :
            if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
            if ( (t = (int) sp->dval) < 0 )
                rt_error( "negative field index $%d", t) ;
            if ( t && nf < 0 )  split_field0() ;
            sp->ptr = (PTR) field_ptr(t) ;
            break ;

        case  FE_PUSHI :
            if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;

            if ( (t = (int) sp->dval) < 0 )
                  rt_error( "negative field index $%d", t) ;

            if ( nf < 0)  split_field0() ;
            if ( t <= nf ) (void) cellcpy(sp, field_ptr(t)) ;
            else
            { sp->type = C_STRING ;
              sp->ptr = (PTR) & null_str ;
              null_str.ref_cnt++ ;
            }
            break ; 


        case  AE_PUSHA :
        /* top of stack has an expr, cdp->ptr points at an
           array, replace the expr with the cell address inside
           the array */

            cp = array_find((ARRAY)cdp++->ptr, sp, CREATE) ;
            cell_destroy(sp) ;
            sp->ptr = (PTR) cp ;
            break ;

        case  AE_PUSHI :
        /* top of stack has an expr, cdp->ptr points at an
           array, replace the expr with the contents of the
           cell inside the array */

            cp = array_find((ARRAY) cdp++->ptr, sp, CREATE) ;
            cell_destroy(sp) ;
            (void) cellcpy(sp, cp) ;
            break ;

        case  LAE_PUSHI :
        /*  sp[0] is an expression
            cdp->op is offset from frame pointer of a CELL which
               has an ARRAY in the ptr field, replace expr
            with  array[expr]
        */
            cp = array_find( (ARRAY)fp[cdp++->op].ptr, sp, CREATE) ;
            cell_destroy(sp) ;
            (void) cellcpy(sp, cp) ;
            break ;
            
        case  LAE_PUSHA :
        /*  sp[0] is an expression
            cdp->op is offset from frame pointer of a CELL which
               has an ARRAY in the ptr field, replace expr
            with  & array[expr]
        */
            cp = array_find( (ARRAY)fp[cdp++->op].ptr, sp, CREATE) ;
            cell_destroy(sp) ;
            sp->ptr = (PTR) cp ;
            break ;
            
        case  LA_PUSHA  :
        /*  cdp->op is offset from frame pointer of a CELL which
               has an ARRAY in the ptr field. Push this ARRAY
               on the eval stack
        */
            inc_sp() ;
            sp->ptr = fp[cdp++->op].ptr ;
            break ;

        case  SET_ALOOP :
            { ALOOP_STATE *ap = (ALOOP_STATE *)
	                        (cdp + cdp->op + 2)->ptr ;

              ap->var = (CELL *) sp[-1].ptr ;
              ap->A = (ARRAY) sp->ptr ;
              sp -= 2 ;

	      ap->index = -1 ;
              if ( inc_aloop_state(ap) )  cdp++ ;
	      else  cdp += cdp->op + 3 ;
	    }
	    break ;

        case  ALOOP :

	    if ( inc_aloop_state( (ALOOP_STATE*) cdp[1].ptr ) )
		    cdp += cdp->op ;
	    else    cdp += 2 ;
	    break ;

        case  _POP : 
            cell_destroy(sp) ;
            sp-- ;
            break ;

        case _DUP  :
            (void) cellcpy(sp+1, sp) ;
            sp++ ; break ;

        case  _ASSIGN :
            /* top of stack has an expr, next down is an
               address, put the expression in *address and
               replace the address with the expression */

            /* don't propagate type C_MBSTRN */
            if ( sp->type == C_MBSTRN ) check_strnum(sp) ;
            sp-- ;
            cell_destroy( ((CELL *)sp->ptr) ) ;
            (void) cellcpy( sp, cellcpy(sp->ptr, sp+1) ) ;
            cell_destroy(sp+1) ;
            break ;

        case  F_ASSIGN : /* assign to a field  */
            if (sp->type == C_MBSTRN) check_strnum(sp) ;
            sp-- ;
            field_assign((CELL*)sp->ptr, sp+1) ;
            cell_destroy(sp+1) ;
            (void) cellcpy(sp, (CELL *) sp->ptr) ;
            break ;

        case  _ADD_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;

#if SW_FP_CHECK   /* specific to V7 and XNX23A */
            clrerr();
#endif
            cp->dval += sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        case  _SUB_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
#if SW_FP_CHECK
            clrerr();
#endif
            cp->dval -= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        case  _MUL_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
#if SW_FP_CHECK
            clrerr();
#endif
            cp->dval *= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        case  _DIV_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp->dval) ;
#endif

#if SW_FP_CHECK
            clrerr();
#endif
            cp->dval /= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        case  _MOD_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp->dval) ;
#endif

            cp->dval = fmod(cp->dval,sp-- -> dval) ;
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        case  _POW_ASG:
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            cp->dval = pow(cp->dval,sp-- -> dval) ;
            sp->type = C_DOUBLE ;
            sp->dval = cp->dval ;
            break ;

        /* will anyone ever use these ? */

        case F_ADD_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;
#if SW_FP_CHECK
            clrerr();
#endif
            tc.dval += sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case F_SUB_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;
#if SW_FP_CHECK
            clrerr();
#endif
            tc.dval -= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case F_MUL_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;
#if SW_FP_CHECK
            clrerr();
#endif
            tc.dval *= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case F_DIV_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp->dval) ;
#endif

#if SW_FP_CHECK
            clrerr();
#endif
            tc.dval /= sp-- -> dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case F_MOD_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp->dval) ;
#endif

            tc.dval = fmod(tc.dval, sp-- -> dval) ;
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case F_POW_ASG :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            cp = (CELL *) (sp-1)->ptr ;
            cast1_to_d( cellcpy(&tc, cp) ) ;
            tc.dval = pow(tc.dval, sp-- -> dval) ;
            sp->type = C_DOUBLE ;
            sp->dval = tc.dval ;
            field_assign(cp, &tc) ;
            break ;

        case _ADD :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;
#if SW_FP_CHECK
            clrerr();
#endif
            sp[0].dval += sp[1].dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            break ;

        case _SUB :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;
#if SW_FP_CHECK
            clrerr();
#endif
            sp[0].dval -= sp[1].dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            break ;

        case _MUL :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;
#if SW_FP_CHECK
            clrerr();
#endif
            sp[0].dval *= sp[1].dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            break ;

        case _DIV :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp[1].dval) ;
#endif

#if SW_FP_CHECK
            clrerr();
#endif
            sp[0].dval /= sp[1].dval ;
#if SW_FP_CHECK
            fpcheck();
#endif
            break ;

        case _MOD :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;

#if  NOINFO_SIGFPE
        CHECK_DIVZERO(sp[1].dval) ;
#endif

            sp[0].dval = fmod(sp[0].dval,sp[1].dval) ;
            break ;

        case _POW :
            sp-- ;
            if ( TEST2(sp) != TWO_DOUBLES )
                    cast2_to_d(sp) ;
            sp[0].dval = pow(sp[0].dval,sp[1].dval) ;
            break ;

        case _NOT :
        reswitch_1:
            switch( sp->type )
            { case C_NOINIT :
                    sp->dval = 1.0 ; break ;
              case C_DOUBLE :
                    sp->dval = D2BOOL(sp->dval) ? 0.0 : 1.0 ;
                    break ;
              case C_STRING :
                    sp->dval = string(sp)->len ? 0.0 : 1.0 ;
                    free_STRING(string(sp)) ;
                    break ;
              case C_STRNUM : /* test as a number */
                    sp->dval = D2BOOL(sp->dval) ? 0.0 : 1.0 ;
                    free_STRING(string(sp)) ;
                    break ;
              case C_MBSTRN :
                    check_strnum(sp) ;
                    goto reswitch_1 ;
              default :
                    bozo("bad type on eval stack") ;
            }
            sp->type = C_DOUBLE ;
            break  ;

        case _TEST :
        reswitch_2:
            switch( sp->type )
            { case C_NOINIT :
                    sp->dval = 0.0 ; break ;
              case C_DOUBLE :
                    sp->dval = D2BOOL(sp->dval) ? 1.0 : 0.0 ;
                    break ;
              case C_STRING :
                    sp->dval  = string(sp)->len ? 1.0 : 0.0 ;
                    free_STRING(string(sp)) ;
                    break ;
              case C_STRNUM : /* test as a number */
                    sp->dval = D2BOOL(sp->dval) ? 1.0 : 0.0 ;
                    free_STRING(string(sp)) ;
                    break ;
              case C_MBSTRN :
                    check_strnum(sp) ;
                    goto reswitch_2 ;
              default :
                    bozo("bad type on eval stack") ;
            }
            sp->type = C_DOUBLE ;
            break ;

        case _UMINUS :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            sp->dval = - sp->dval ;
            break ;

        case _UPLUS :  
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            break ;

        case _CAT :
            { unsigned len1, len2 ;
              char *str1, *str2 ;
              STRING *b ;
              
              sp-- ;
              if ( TEST2(sp) != TWO_STRINGS )
                    cast2_to_s(sp) ;
              str1 = string(sp)->str ;
              len1 = string(sp)->len ;
              str2 = string(sp+1)->str ;
              len2 = string(sp+1)->len ;

              b = new_STRING((char *)0, len1+len2) ;
              (void) memcpy(b->str, str1, SIZE_T(len1)) ;
              (void) memcpy(b->str + len1, str2, SIZE_T(len2)) ;
              free_STRING(string(sp)) ;
              free_STRING( string(sp+1) ) ;

              sp->ptr = (PTR) b ;
              break ;
            }

        case _PUSHINT :
            inc_sp() ;
            sp->type = cdp++ -> op ;
            break ;

        case _BUILTIN :
        case _PRINT :
            sp = (* (PF_CP) cdp++ -> ptr) (sp) ;
            break ;

        case _POST_INC :
            (void) cellcpy(sp, cp = (CELL *)sp->ptr) ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            cp->dval += 1.0 ;
            break ;

        case _POST_DEC :
            (void) cellcpy(sp, cp = (CELL *)sp->ptr) ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            cp->dval -= 1.0 ;
            break ;

        case _PRE_INC :
            cp = (CELL *) sp->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            sp->dval = cp->dval += 1.0 ;
            sp->type = C_DOUBLE ;
            break ;

        case _PRE_DEC :
            cp = (CELL *) sp->ptr ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            sp->dval = cp->dval -= 1.0 ;
            sp->type = C_DOUBLE ;
            break ;


        case F_POST_INC  :
            cp = (CELL *) sp->ptr ;
            (void) cellcpy(sp, cellcpy(&tc, cp) ) ;
            cast1_to_d(&tc) ;
            tc.dval += 1.0 ;
            field_assign(cp, &tc) ;
            break ;

        case F_POST_DEC  :
            cp = (CELL *) sp->ptr ;
            (void) cellcpy(sp, cellcpy(&tc, cp) ) ;
            cast1_to_d(&tc) ;
            tc.dval -= 1.0 ;
            field_assign(cp, &tc) ;
            break ;

        case F_PRE_INC :
            cp = (CELL *) sp->ptr ;
            cast1_to_d(cellcpy(&tc, cp)) ;
            sp->dval = tc.dval += 1.0 ;
            sp->type = C_DOUBLE ;
            field_assign(cp, sp) ;
            break ;

        case F_PRE_DEC :
            cp = (CELL *) sp->ptr ;
            cast1_to_d(cellcpy(&tc, cp)) ;
            sp->dval = tc.dval -= 1.0 ;
            sp->type = C_DOUBLE ;
            field_assign(cp, sp) ;
            break ;

        case _JMP  :
            cdp += cdp->op  ;
            break ;

        case _JNZ  :
            /* jmp if top of stack is non-zero and pop stack */
            if ( test( sp ) )
                cdp += cdp->op  ;
            else  cdp++ ;
            cell_destroy(sp) ;
            sp-- ;
            break ;

        case _JZ  :
            /* jmp if top of stack is zero and pop stack */
            if ( ! test( sp ) )
                cdp += cdp->op  ;
            else  cdp++ ;
            cell_destroy(sp) ;
            sp-- ;
            break ;

    /*  the relation operations */
    /*  compare() makes sure string ref counts are OK */
        case  _EQ :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t == 0 ? 1.0 : 0.0 ;
            break ;

        case  _NEQ :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t ? 1.0 : 0.0 ;
            break ;

        case  _LT :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t < 0 ? 1.0 : 0.0 ;
            break ;

        case  _LTE :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t <= 0 ? 1.0 : 0.0 ;
            break ;

        case  _GT :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t > 0 ? 1.0 : 0.0 ;
            break ;

        case  _GTE :
            t = compare(--sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = t >= 0 ? 1.0 : 0.0 ;
            break ;

        case _MATCH0 : 
            /* does $0 match, the RE at cdp */

            inc_sp() ;
            if ( field->type >= C_STRING )
            { sp->type = C_DOUBLE ;
              sp->dval = REtest(string(field)->str, cdp++->ptr)
                         ? 1.0 : 0.0 ;

              break /* the case */ ;
            }
            else
            {
              cellcpy(sp, field) ;
              /* and FALL THRU */
            }

        case _MATCH1 :
            /* does expr at sp[0] match RE at cdp */
            if ( sp->type < C_STRING ) cast1_to_s(sp) ;
            t = REtest(string(sp)->str, cdp++->ptr) ;
            free_STRING(string(sp)) ;
            sp->type = C_DOUBLE ;
            sp->dval = t ? 1.0 : 0.0 ;
            break ;


        case  _MATCH2 :
            /* does sp[-1] match sp[0] as re */
             cast_to_RE(sp) ;

            if ( (--sp)->type < C_STRING )  cast1_to_s(sp) ;
            t = REtest(string(sp)->str, (sp+1)->ptr) ; 

            free_STRING(string(sp)) ;
            sp->type = C_DOUBLE ;
            sp->dval = t ? 1.0 : 0.0 ;
            break ;

        case  A_TEST :
        /* entry :  sp[0].ptr-> an array
                    sp[-1]  is an expression

           we compute   expression in array  */
            sp-- ;
            cp = array_find( (sp+1)->ptr, sp, NO_CREATE) ;
            cell_destroy(sp) ;
            sp->type = C_DOUBLE ;
            sp->dval = (cp!=(CELL*)0)  ? 1.0 : 0.0 ;
            break ;

        case  A_DEL :
        /* sp[0].ptr ->  array
           sp[-1] is an expr
           delete  array[expr]  */

            array_delete(sp->ptr, sp-1) ;
            cell_destroy(sp-1) ;
            sp -= 2 ;
            break ;
        
        /* form a multiple array index */
        case A_CAT :
            sp = array_cat(sp, cdp++->op) ;
            break ;

        case  _EXIT  :
            if ( sp->type != C_DOUBLE ) cast1_to_d(sp) ;
            exit_code = (int) sp-- -> dval ;
            /* fall thru */

        case  _EXIT0 :
            if ( !(cdp = end_code.start) ) mawk_exit(exit_code) ;

            end_code.start = (INST *) 0 ; /* makes sure next exit exits */
            if ( begin_code.start )
		 zfree(begin_code.start, begin_code.size) ;
            if ( main_start )  zfree(main_start, main_size);
            sp = eval_stack - 1 ; /* might be in user function */
            break ;

        case  _JMAIN : /* go from BEGIN code to MAIN code */
            zfree(begin_code.start, begin_code.size) ;
            begin_code.start = (INST *) 0 ;
            cdp = main_start ;
            break ;

        case  _OMAIN :
            if ( !main_fin )  open_main() ;
            restart_label = cdp ;
            cdp = next_label ;
            break ;

        case  _NEXT :
            cdp = next_label ;
            break ;

        case  OL_GL :
            {
              char *p ;
              unsigned len ;

              if ( !(p = FINgets(main_fin, &len)) )
              {
                if ( !end_code.start )  mawk_exit(0) ;

                cdp = end_code.start ;
                zfree(main_start, main_size) ;
                main_start = end_code.start = (INST*) 0 ;
              }
              else
              { set_field0(p, len) ; cdp = restart_label ; }
            }
            break ;

        case  OL_GL_NR :
            {
              char *p ;
              unsigned len ;

              if ( !(p = FINgets(main_fin, &len)) )
              {
                if ( !end_code.start )  mawk_exit(0) ;

                cdp = end_code.start ;
                zfree(main_start, main_size) ;
                main_start = end_code.start = (INST*) 0 ;
              }
              else
              {
                set_field0(p, len) ; 
                cdp = restart_label ;

                if ( TEST2(NR) != TWO_DOUBLES ) cast2_to_d(NR) ;

                NR->dval += 1.0 ;
                FNR->dval += 1.0 ;
              }
            }
            break ;


        case  _RANGE :
/* test a range pattern:  pat1, pat2 { action }
   entry :
       cdp[0].op -- a flag, test pat1 if on else pat2
       cdp[1].op -- offset of pat2 code from cdp
       cdp[2].op -- offset of action code from cdp
       cdp[3].op -- offset of code after the action from cdp
       cdp[4] -- start of pat1 code
*/

#define FLAG    cdp[0].op
#define PAT2    cdp[1].op
#define ACTION    cdp[2].op
#define FOLLOW    cdp[3].op
#define PAT1      4

            if ( FLAG )  /* test again pat1 */
            { 
              execute(cdp + PAT1,sp, fp) ;
              t = test(sp+1) ;
              cell_destroy(sp+1) ;
              if ( t )  FLAG = 0 ;
              else
              { cdp += FOLLOW ;
                break ;  /* break the switch */
              }
            }

            /* test against pat2 and then perform the action */
            execute(cdp + PAT2, sp, fp) ;
            FLAG  = test(sp+1) ;
            cell_destroy(sp+1) ; 
            cdp += ACTION ;
            break ;

/* function calls  */

      case  _RET0  :
            inc_sp() ;
            sp->type = C_NOINIT ;
            /* fall thru */

      case  _RET   :

#ifdef  DEBUG 
            if ( sp != entry_sp+1 ) bozo("ret") ;
#endif
            if ( old_stack_base ) /* reset stack */
            {
              /* move the return value */
              (void) cellcpy(old_sp+1, sp) ;
              cell_destroy(sp) ;
              zfree(stack_base, sizeof(CELL)*EVAL_STACK_SIZE) ;
              stack_base = old_stack_base ;
              stack_danger = old_stack_base + DANGER ;
            }
              
            return  ;

      case  _CALL  :

            { FBLOCK *fbp = (FBLOCK*) cdp++->ptr ;
              int a_args = cdp++->op ; /* actual number of args */
              CELL *nfp = sp - a_args + 1 ; /* new fp for callee */
              CELL *local_p = sp+1; /* first local argument on stack */
              char *type_p ;  /* pts to type of an argument */

              if ( fbp->nargs ) type_p = fbp->typev + a_args ;

              /* create space for locals */
              if ( t = fbp->nargs - a_args ) /* have local args */
              {
                while ( t-- )  
                { (++sp)->type = C_NOINIT ;
                  if ( *type_p++ == ST_LOCAL_ARRAY )
                        sp->ptr = (PTR) new_ARRAY() ;
                }
              }
              type_p-- ; /* *type_p is type of last arg */ 

              execute(fbp->code, sp, nfp) ;

              /* cleanup the callee's arguments */
              if ( sp >= nfp ) 
              {
                cp = sp+1 ;  /* cp -> the function return */

                do
                {
                  if ( *type_p-- == ST_LOCAL_ARRAY )
                  {  if ( sp >= local_p ) array_free(sp->ptr) ; }
                  else  cell_destroy(sp) ;

                } while ( --sp >= nfp ) ;
                    
                (void) cellcpy(++sp, cp) ;
                cell_destroy(cp) ;
              }
              else  sp++ ; /* no arguments passed */
            }
            break ;

        default :
            bozo("bad opcode") ;
    }
}

int test( cp )  /* test if a cell is null or not */
  register CELL *cp ;
{ 
reswitch :

  switch ( cp->type )
  {
    case C_NOINIT :  return  0 ;
    case C_STRNUM :  /* test as a number */
    case C_DOUBLE :  return  cp->dval != 0.0 ;
    case C_STRING :  return  string(cp)->len ;
    case C_MBSTRN :  check_strnum(cp) ; goto reswitch ;

    default :
      bozo("bad cell type in call to test") ;
  }
}

/* compare cells at cp and cp+1 and
   frees STRINGs at those cells
*/
static int compare(cp)
  register CELL *cp ;
{ int k ;
  CELL *dp, *sp ;

reswitch :

  switch( TEST2(cp) )
  { case TWO_NOINITS :  return 0 ; 
    
    case TWO_DOUBLES :
    two_d:
            return  cp->dval > (cp+1)->dval ? 1 :
                    cp->dval < (cp+1)->dval ? -1 : 0 ;
    
    case TWO_STRINGS :
            k = strcmp(string(cp)->str, string(cp+1)->str) ;
            free_STRING( string(cp) ) ;
            free_STRING( string(cp+1) ) ;
            return k ;

    case STRING_AND_STRNUM :  /* posix numeric string bozosity */
    case  NOINIT_AND_STRING  :
    case  DOUBLE_AND_STRING  :
            if ( cp->type  == C_STRING ) { sp = cp ; dp = cp+1 ; }
            else { dp = cp ; sp = cp+1 ; }

            check_strnum(sp) ;
            if ( sp->type == C_STRING ) cast1_to_s(dp) ;
            goto reswitch ;


    case  NOINIT_AND_DOUBLE  :
    case  NOINIT_AND_STRNUM  :
    case  DOUBLE_AND_STRNUM  :
    case TWO_STRNUMS :
            cast2_to_d(cp) ; goto two_d ;


    case  TWO_MBSTRNS :
            check_strnum(cp) ; check_strnum(cp+1) ;
            goto reswitch ;

    case  NOINIT_AND_MBSTRN :
    case  DOUBLE_AND_MBSTRN :
    case  STRING_AND_MBSTRN :
    case  STRNUM_AND_MBSTRN :
            check_strnum( cp->type == C_MBSTRN ? cp : cp+1 ) ;
            goto reswitch ;

    default :  /* there are no default cases */
            bozo("bad cell type passed to compare") ;
  }
}

/* does not assume target was a cell, if so
   then caller should have made a previous
   call to cell_destroy  */

CELL *cellcpy(target, source)
  register CELL *target, *source ;
{ switch( target->type = source->type )
  { case C_NOINIT : 
    case C_SPACE  : 
    case C_SNULL  :
            break ;

    case C_DOUBLE :
            target->dval = source->dval ;
            break ;

    case C_STRNUM :
            target->dval = source->dval ;
            /* fall thru */

    case C_REPL    :
    case C_MBSTRN  :
    case C_STRING  :
            string(source)->ref_cnt++ ;
            /* fall thru */

    case C_RE  :
            target->ptr = source->ptr ;
            break ;

    case  C_REPLV :
            (void)  replv_cpy(target, source) ;
            break ;

    default :
            bozo("bad cell passed to cellcpy()") ;
            break ;
  }
  return  target ;
}

#ifdef   DEBUG

void  DB_cell_destroy(cp)    /* HANGOVER time */
  register CELL *cp ;
{
  switch( cp->type )
  { case C_NOINIT :
    case C_DOUBLE :  break ;

    case C_MBSTRN :
    case C_STRING :
    case C_STRNUM :
            if ( -- string(cp)->ref_cnt == 0 )
                zfree(string(cp) , string(cp)->len+STRING_OH) ;
            break ;

    case  C_RE :
            bozo("cell destroy called on RE cell") ;
    default :
            bozo("cell destroy called on bad cell type") ;
  }
}

#endif
