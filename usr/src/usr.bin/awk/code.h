
/********************************************
code.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	code.h,v $
 * Revision 5.1  91/12/05  07:59:07  brennan
 * 1.1 pre-release
 * 
*/


/*  code.h  */

#ifndef  CODE_H
#define  CODE_H

#include "memory.h"

/* coding scope */
#define   SCOPE_MAIN    0
#define   SCOPE_BEGIN   1  
#define   SCOPE_END     2
#define   SCOPE_FUNCT   3


extern  INST  *code_ptr ;
extern  INST  *main_start, *main_code_ptr ;
extern  unsigned main_size ;

extern struct be_code {
INST *start , *ptr ;
unsigned size ; } begin_code , end_code ;

void PROTO(be_shrink, (struct be_code *)) ;
void PROTO(be_expand, (struct be_code *)) ;

#define INST_BYTES(x) (sizeof(INST)*(x))

extern  CELL  eval_stack[] ;


#define  code1(x)  code_ptr++ -> op = (x)

#define  code2(x,y)    (void)( code_ptr++ -> op = (x) ,\
                         code_ptr++ -> ptr = (PTR)(y) )

extern int exit_code ;

/*  the machine opcodes  */
/* to avoid confusion with a ptr FE_PUSHA must have op code 0 */
/* unfortunately enums are less portable than defines */

#define  FE_PUSHA     0
#define  FE_PUSHI     1
#define  F_PUSHA      2
#define  F_PUSHI      3
#define  NF_PUSHI     4
#define  _HALT        5
#define  _STOP        6
#define  _PUSHC       7
#define  _PUSHD       8
#define  _PUSHS       9
#define  _PUSHINT    10
#define  _PUSHA      11
#define  _PUSHI      12
#define  L_PUSHA     13
#define  L_PUSHI     14
#define  AE_PUSHA    15
#define  AE_PUSHI    16
#define  A_PUSHA     17
#define  LAE_PUSHA   18
#define  LAE_PUSHI   19
#define  LA_PUSHA    20
#define  _POP        21
#define  _DUP        22
#define  _ADD        23
#define  _SUB        24
#define  _MUL        25
#define  _DIV        26
#define  _MOD        27
#define  _POW        28
#define  _NOT        29
#define  _TEST       30
#define  A_TEST      31
#define  A_DEL       32
#define  ALOOP       33
#define  A_CAT       34
#define  _UMINUS     35
#define  _UPLUS      36
#define  _ASSIGN     37
#define  _ADD_ASG    38
#define  _SUB_ASG    39
#define  _MUL_ASG    40
#define  _DIV_ASG    41
#define  _MOD_ASG    42
#define  _POW_ASG    43
#define  F_ASSIGN    44
#define  F_ADD_ASG   45
#define  F_SUB_ASG   46
#define  F_MUL_ASG   47
#define  F_DIV_ASG   48
#define  F_MOD_ASG   49
#define  F_POW_ASG   50
#define  _CAT        51
#define  _BUILTIN    52
#define  _PRINT      53
#define  _POST_INC   54
#define  _POST_DEC   55
#define  _PRE_INC    56
#define  _PRE_DEC    57
#define  F_POST_INC  58
#define  F_POST_DEC  59
#define  F_PRE_INC   60
#define  F_PRE_DEC   61
#define  _JMP        62
#define  _JNZ        63
#define  _JZ         64
#define  _EQ         65
#define  _NEQ        66
#define  _LT         67
#define  _LTE        68
#define  _GT         69
#define  _GTE        70
#define  _MATCH0     71
#define  _MATCH1     72
#define  _MATCH2     73
#define  _EXIT       74
#define  _EXIT0      75
#define  _NEXT       76
#define  _RANGE      77
#define  _CALL       78
#define  _RET        79
#define  _RET0       80
#define  SET_ALOOP   81
#define  OL_GL       82
#define  OL_GL_NR    83
#define  _OMAIN      84
#define  _JMAIN      85


#endif  /* CODE_H */
