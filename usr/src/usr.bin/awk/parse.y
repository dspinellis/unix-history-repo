
/********************************************
parse.y
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	parse.y,v $
 * Revision 5.2  92/01/08  16:11:42  brennan
 * code FE_PUSHA carefully for MSDOS large mode
 * 
 * Revision 5.1  91/12/05  07:50:22  brennan
 * 1.1 pre-release
 * 
*/


%{
#include <stdio.h>
#include "mawk.h"
#include "code.h"
#include "symtype.h"
#include "memory.h"
#include "bi_funct.h"
#include "bi_vars.h"
#include "jmp.h"
#include "field.h"
#include "files.h"

#ifdef  YYXBYACC
#define YYBYACC		1
#endif

#define  YYMAXDEPTH	200

/* Bison's use of MSDOS and ours clashes */
#undef   MSDOS

extern void  PROTO( eat_nl, (void) ) ;
static void  PROTO( resize_fblock, (FBLOCK *, INST *) ) ;
static void  PROTO( code_array, (SYMTAB *) ) ;
static void  PROTO( code_call_id, (CA_REC *, SYMTAB *) ) ;
static void  PROTO( field_A2I, (void)) ;
static int   PROTO( current_offset, (void) ) ;
static void  PROTO( check_var, (SYMTAB *) ) ;
static void  PROTO( check_array, (SYMTAB *) ) ;
static void  PROTO( RE_as_arg, (void)) ;

static int scope ;
static FBLOCK *active_funct ;
      /* when scope is SCOPE_FUNCT  */

#define  code_address(x)  if( is_local(x) )\
                          { code1(L_PUSHA) ; code1((x)->offset) ; }\
                          else  code2(_PUSHA, (x)->stval.cp) 

/* this nonsense caters to MSDOS large model */
#define  CODE_FE_PUSHA()  code_ptr->ptr = (PTR) 0 ; code1(FE_PUSHA)

%}

%union{
CELL *cp ;
SYMTAB *stp ;
INST  *start ; /* code starting address */
PF_CP  fp ;  /* ptr to a (print/printf) or (sub/gsub) function */
BI_REC *bip ; /* ptr to info about a builtin */
FBLOCK  *fbp  ; /* ptr to a function block */
ARG2_REC *arg2p ;
CA_REC   *ca_p  ;
int   ival ;
PTR   ptr ;
}

/*  two tokens to help with errors */
%token   UNEXPECTED   /* unexpected character */
%token   BAD_DECIMAL

%token   NL
%token   SEMI_COLON
%token   LBRACE  RBRACE
%token   LBOX     RBOX
%token   COMMA
%token   <ival> IO_OUT    /* > or output pipe */

%right  ASSIGN  ADD_ASG SUB_ASG MUL_ASG DIV_ASG MOD_ASG POW_ASG
%right  QMARK COLON
%left   OR
%left   AND
%left   IN
%left   <ival> MATCH   /* ~  or !~ */
%left   EQ  NEQ  LT LTE  GT  GTE
%left   CAT
%left   GETLINE
%left   PLUS      MINUS  
%left   MUL      DIV    MOD
%left   NOT   UMINUS
%nonassoc   IO_IN PIPE
%right  POW
%left   <ival>   INC_or_DEC
%left   DOLLAR    FIELD  /* last to remove a SR conflict
                                with getline */
%right  LPAREN   RPAREN     /* removes some SR conflicts */

%token  <ptr> DOUBLE STRING_ RE  
%token  <stp> ID   D_ID
%token  <fbp> FUNCT_ID
%token  <bip> BUILTIN 
%token   <cp>  FIELD 

%token  PRINT PRINTF SPLIT MATCH_FUNC SUB GSUB 
/* keywords */
%token  DO WHILE FOR BREAK CONTINUE IF ELSE  IN
%token  DELETE  BEGIN  END  EXIT NEXT RETURN  FUNCTION

%type <start>  block  block_or_separator
%type <start>  statement_list statement mark
%type <ival>   pr_args
%type <arg2p>  arg2  
%type <start>  builtin  
%type <start>  getline_file
%type <start>  lvalue field  fvalue
%type <start>  expr cat_expr p_expr
%type <start>  while_front  if_front 
%type <start>  for1 for2
%type <start>  array_loop_front
%type <start>  return_statement
%type <start>  split_front  re_arg sub_back
%type <ival>   arglist args 
%type <fp>     print   sub_or_gsub
%type <fbp>    funct_start funct_head
%type <ca_p>   call_args ca_front ca_back
%type <ival>   f_arglist f_args

%%
/*  productions  */

program :       program_block
        |       program  program_block 
        ;

program_block :  PA_block   /* pattern-action */
              |  function_def
              |  error block
                 { if (scope == SCOPE_FUNCT)
                   { restore_ids() ; scope = SCOPE_MAIN ; }
                   code_ptr = main_code_ptr ;
                 }
              ;

PA_block  :  block 
             { /* this do nothing action removes a vacuous warning
                  from Bison */
             }

          |  BEGIN  
                { 
		  be_expand(&begin_code) ;
                  scope = SCOPE_BEGIN ;
                }

             block
                { be_shrink(&begin_code) ;
                  scope = SCOPE_MAIN ;
                }

          |  END    
                { 
		  be_expand(&end_code) ;
                  scope = SCOPE_END ;
                }

             block
                { be_shrink(&end_code) ;
                  scope = SCOPE_MAIN ;
                }

          |  expr  /* this works just like an if statement */
             { code_jmp(_JZ, (INST*)0) ; }

             block_or_separator
             { patch_jmp( code_ptr ) ; }

    /* range pattern, see comment in execute.c near _RANGE */
          |  expr COMMA 
             { code_push($1, code_ptr - $1) ;
               code_ptr = $1 ;
               code1(_RANGE) ; code1(1) ;
               code_ptr += 3 ;
               code_ptr += code_pop(code_ptr) ;
               code1(_STOP) ;
               $1[2].op = code_ptr - ($1+1) ;
             }
             expr
             { code1(_STOP) ; }

             block_or_separator
             { $1[3].op = $6 - ($1+1) ;
               $1[4].op = code_ptr - ($1+1) ;
             }
          ;



block   :  LBRACE   statement_list  RBRACE
            { $$ = $2 ; }
        |  LBRACE   error  RBRACE 
            { $$ = code_ptr ; /* does nothing won't be executed */
              print_flag = getline_flag = paren_cnt = 0 ;
              yyerrok ; }
        ;

block_or_separator  :  block
                  |  separator     /* default print action */
                     { $$ = code_ptr ;
                       code1(_PUSHINT) ; code1(0) ;
                       code2(_PRINT, bi_print) ;
                     }

statement_list :  statement
        |  statement_list   statement
        ;


statement :  block
          |  expr   separator
             { code1(_POP) ; }
          |  /* empty */  separator
             { $$ = code_ptr ; }
          |  error  separator
              { $$ = code_ptr ;
                print_flag = getline_flag = 0 ;
                paren_cnt = 0 ;
                yyerrok ;
              }
          |  BREAK  separator
             { $$ = code_ptr ; BC_insert('B', code_ptr+1) ;
               code2(_JMP, 0) /* don't use code_jmp ! */ ; }
          |  CONTINUE  separator
             { $$ = code_ptr ; BC_insert('C', code_ptr+1) ;
               code2(_JMP, 0) ; }
          |  return_statement
             { if ( scope != SCOPE_FUNCT )
                     compile_error("return outside function body") ;
             }
          |  NEXT  separator
              { if ( scope != SCOPE_MAIN )
                   compile_error( "improper use of next" ) ;
                $$ = code_ptr ; 
                code1(_NEXT) ;
              }
          ;

separator  :  NL | SEMI_COLON
           ;

expr  :   cat_expr
      |   lvalue   ASSIGN   expr { code1(_ASSIGN) ; }
      |   lvalue   ADD_ASG  expr { code1(_ADD_ASG) ; }
      |   lvalue   SUB_ASG  expr { code1(_SUB_ASG) ; }
      |   lvalue   MUL_ASG  expr { code1(_MUL_ASG) ; }
      |   lvalue   DIV_ASG  expr { code1(_DIV_ASG) ; }
      |   lvalue   MOD_ASG  expr { code1(_MOD_ASG) ; }
      |   lvalue   POW_ASG  expr { code1(_POW_ASG) ; }
      |   expr EQ expr  { code1(_EQ) ; }
      |   expr NEQ expr { code1(_NEQ) ; }
      |   expr LT expr { code1(_LT) ; }
      |   expr LTE expr { code1(_LTE) ; }
      |   expr GT expr { code1(_GT) ; }
      |   expr GTE expr { code1(_GTE) ; }

      |   expr MATCH expr
          {
            if ( $3 == code_ptr - 2 )
            {
               if ( $3->op == _MATCH0 )  $3->op = _MATCH1 ;

               else /* check for string */
               if ( $3->op == _PUSHS )
               { CELL *cp = ZMALLOC(CELL) ;

                 cp->type = C_STRING ; 
                 cp->ptr = $3[1].ptr ;
                 cast_to_RE(cp) ;
                 code_ptr -= 2 ;
                 code2(_MATCH1, cp->ptr) ;
                 ZFREE(cp) ;
               }
               else  code1(_MATCH2) ;
            }
            else code1(_MATCH2) ;

            if ( !$2 ) code1(_NOT) ;
          }

/* short circuit boolean evaluation */
      |   expr  OR
              { code1(_DUP) ;
                code_jmp(_JNZ, (INST*)0) ;
                code1(_POP) ;
              }
          expr
          { patch_jmp(code_ptr) ; code1(_TEST) ; }

      |   expr AND
              { code1(_DUP) ; code_jmp(_JZ, (INST*)0) ;
                code1(_POP) ; }
          expr
              { patch_jmp(code_ptr) ; code1(_TEST) ; }

      |  expr QMARK  { code_jmp(_JZ, (INST*)0) ; }
         expr COLON  { code_jmp(_JMP, (INST*)0) ; }
         expr
         { patch_jmp(code_ptr) ; patch_jmp($7) ; }
      ;

cat_expr :  p_expr             %prec CAT
         |  cat_expr  p_expr   %prec CAT 
            { code1(_CAT) ; }
         ;

p_expr  :   DOUBLE
          {  $$ = code_ptr ; code2(_PUSHD, $1) ; }
      |   STRING_
          { $$ = code_ptr ; code2(_PUSHS, $1) ; }
      |   ID   %prec AND /* anything less than IN */
          { check_var($1) ;
            $$ = code_ptr ;
            if ( is_local($1) )
            { code1(L_PUSHI) ; code1($1->offset) ; }
            else code2(_PUSHI, $1->stval.cp) ;
          }
                            
      |   LPAREN   expr  RPAREN
          { $$ = $2 ; }
      ;

p_expr  :   RE     
            { $$ = code_ptr ; code2(_MATCH0, $1) ; }
        ;

p_expr  :   p_expr  PLUS   p_expr { code1(_ADD) ; } 
      |   p_expr MINUS  p_expr { code1(_SUB) ; }
      |   p_expr  MUL   p_expr { code1(_MUL) ; }
      |   p_expr  DIV  p_expr { code1(_DIV) ; }
      |   p_expr  MOD  p_expr { code1(_MOD) ; }
      |   p_expr  POW  p_expr { code1(_POW) ; }
      |   NOT  p_expr  
                { $$ = $2 ; code1(_NOT) ; }
      |   PLUS p_expr  %prec  UMINUS
                { $$ = $2 ; code1(_UPLUS) ; }
      |   MINUS p_expr %prec  UMINUS
                { $$ = $2 ; code1(_UMINUS) ; }
      |   builtin
      ;

p_expr  :  ID  INC_or_DEC
           { check_var($1) ;
             $$ = code_ptr ;
             code_address($1) ;

             if ( $2 == '+' )  code1(_POST_INC) ;
             else  code1(_POST_DEC) ;
           }
        |  INC_or_DEC  lvalue
            { $$ = $2 ; 
              if ( $1 == '+' ) code1(_PRE_INC) ;
              else  code1(_PRE_DEC) ;
            }
        ;

p_expr  :  field  INC_or_DEC   
           { if ($2 == '+' ) code1(F_POST_INC ) ; 
             else  code1(F_POST_DEC) ;
           }
        |  INC_or_DEC  field
           { $$ = $2 ; 
             if ( $1 == '+' ) code1(F_PRE_INC) ;
             else  code1( F_PRE_DEC) ; 
           }
        ;

lvalue :  ID
        { $$ = code_ptr ; 
          check_var($1) ;
          code_address($1) ;
        }
       ;


arglist :  /* empty */
            { $$ = 0 ; }
        |  args
        ;

args    :  expr        %prec  LPAREN
            { $$ = 1 ; }
        |  args  COMMA  expr
            { $$ = $1 + 1 ; }
        ;

builtin :
        BUILTIN mark  LPAREN  arglist RPAREN
        { BI_REC *p = $1 ;
          $$ = $2 ;
          if ( p-> min_args > $4 || p->max_args < $4 )
            compile_error(
            "wrong number of arguments in call to %s" ,
            p->name ) ;
          if ( p->min_args != p->max_args ) /* variable args */
              { code1(_PUSHINT) ;  code1($4) ; }
          code2(_BUILTIN , p->fp) ;
        }
        ;

/* an empty production to store the code_ptr */
mark : /* empty */
         { $$ = code_ptr ; }

/* print_statement */
statement :  print mark pr_args pr_direction separator
            { code2(_PRINT, $1) ; $$ = $2 ;
              if ( $1 == bi_printf && $3 == 0 )
                    compile_error("no arguments in call to printf") ;
              print_flag = 0 ;
              $$ = $2 ;
            }
            ;

print   :  PRINT  { $$ = bi_print ; print_flag = 1 ;}
        |  PRINTF { $$ = bi_printf ; print_flag = 1 ; }
        ;

pr_args :  arglist { code1(_PUSHINT) ; code1($1) ; }
        |  LPAREN  arg2 RPAREN
           { $$ = $2->cnt ; zfree($2,sizeof(ARG2_REC)) ; 
             code1(_PUSHINT) ; code1($$) ; 
           }
	|  LPAREN  RPAREN
	   { $$=0 ; code1(_PUSHINT) ; code1(0) ; }
        ;

arg2   :   expr  COMMA  expr
           { $$ = (ARG2_REC*) zmalloc(sizeof(ARG2_REC)) ;
             $$->start = $1 ;
             $$->cnt = 2 ;
           }
        |   arg2 COMMA  expr
            { $$ = $1 ; $$->cnt++ ; }
        ;

pr_direction : /* empty */
             |  IO_OUT  expr
                { code1(_PUSHINT) ; code1($1) ; }
             ;


/*  IF and IF-ELSE */

if_front :  IF LPAREN expr RPAREN
            {  $$ = $3 ; eat_nl() ; code_jmp(_JZ, (INST*)0) ; }
         ;

/* if_statement */
statement : if_front statement
                { patch_jmp( code_ptr ) ;  }
              ;

else    :  ELSE { eat_nl() ; code_jmp(_JMP, (INST*)0) ; }
        ;

/* if_else_statement */
statement :  if_front statement else statement
                { patch_jmp(code_ptr) ; patch_jmp($4) ; }


/*  LOOPS   */

do      :  DO
        { eat_nl() ; BC_new() ; }
        ;

/* do_statement */
statement : do statement WHILE LPAREN expr RPAREN separator
        { $$ = $2 ;
          code_jmp(_JNZ, $2) ; 
          BC_clear(code_ptr, $5) ; }
        ;

while_front :  WHILE LPAREN expr RPAREN
                { eat_nl() ; BC_new() ;
                  $$ = $3 ;

                  /* check if const expression */
                  if ( code_ptr - 2 == $3 &&
                       code_ptr[-2].op == _PUSHD &&
                       *(double*)code_ptr[-1].ptr != 0.0 
                     )
                     code_ptr -= 2 ;
                  else
		  {
		    code_push($3, code_ptr-$3) ;
		    code_ptr = $3 ;
                    code2(_JMP, (INST*)0) ; /* code2() not code_jmp() */
		  }
                }
            ;

/* while_statement */
statement  :    while_front  statement
                { 
		  INST *c_addr ; int len ;

                  if ( $1 != $2 )  /* real test in loop */
		  {
		    $1[1].op = code_ptr-($1+1) ;
		    c_addr = code_ptr ;
		    len = code_pop(code_ptr) ;
		    code_ptr += len ;
		    code_jmp(_JNZ, $2) ;
		    BC_clear(code_ptr, c_addr) ;
		  }
		  else /* while(1) */
		  {
		    code_jmp(_JMP, $1) ;
		    BC_clear(code_ptr, $2) ;
		  }
                }
                ;


/* for_statement */
statement   :   for1 for2 for3 statement
                { 
                  INST *cont_address = code_ptr ;
                  unsigned len = code_pop(code_ptr) ;

                  code_ptr += len ;

		  if ( $2 != $4 )  /* real test in for2 */
		  {
                    $4[-1].op = code_ptr - $4 + 1 ;
		    len = code_pop(code_ptr) ;
		    code_ptr += len ;
                    code_jmp(_JNZ, $4) ;
		  }
		  else /*  for(;;) */
		  code_jmp(_JMP, $4) ;

		  BC_clear(code_ptr, cont_address) ;

                }
              ;

for1    :  FOR LPAREN  SEMI_COLON   { $$ = code_ptr ; }
        |  FOR LPAREN  expr SEMI_COLON
           { $$ = $3 ; code1(_POP) ; }
        ;

for2    :  SEMI_COLON   { $$ = code_ptr ; }
        |  expr  SEMI_COLON
           { 
             if ( code_ptr - 2 == $1 &&
                  code_ptr[-2].op == _PUSHD &&
                  * (double*) code_ptr[-1].ptr != 0.0
                )
                    code_ptr -= 2 ;
             else   
	     {
	       code_push($1, code_ptr-$1) ;
	       code_ptr = $1 ;
	       code2(_JMP, (INST*)0) ;
	     }
           }
        ;

for3    :  RPAREN 
           { eat_nl() ; BC_new() ; code_push((INST*)0,0) ; }
        |  expr RPAREN
           { eat_nl() ; BC_new() ; 
             code1(_POP) ;
             code_push($1, code_ptr - $1) ;
             code_ptr -= code_ptr - $1 ;
           }
        ;


/* arrays  */

expr    :  expr IN  ID
           { check_array($3) ;
             code_array($3) ; 
             code1(A_TEST) ; 
            }
        |  LPAREN arg2 RPAREN IN ID
           { $$ = $2->start ;
             code1(A_CAT) ; code1($2->cnt) ;
             zfree($2, sizeof(ARG2_REC)) ;

             check_array($5) ;
             code_array($5) ;
             code1(A_TEST) ;
           }
        ;

lvalue  :  ID mark LBOX  args  RBOX
           { 
             if ( $4 > 1 )
             { code1(A_CAT) ; code1($4) ; }

             check_array($1) ;
             if( is_local($1) )
             { code1(LAE_PUSHA) ; code1($1->offset) ; }
             else code2(AE_PUSHA, $1->stval.array) ;
             $$ = $2 ;
           }
        ;

p_expr  :  ID mark LBOX  args  RBOX   %prec  AND
           { 
             if ( $4 > 1 )
             { code1(A_CAT) ; code1($4) ; }

             check_array($1) ;
             if( is_local($1) )
             { code1(LAE_PUSHI) ; code1($1->offset) ; }
             else code2(AE_PUSHI, $1->stval.array) ;
             $$ = $2 ;
           }

        |  ID mark LBOX  args  RBOX  INC_or_DEC
           { 
             if ( $4 > 1 )
             { code1(A_CAT) ; code1($4) ; }

             check_array($1) ;
             if( is_local($1) )
             { code1(LAE_PUSHA) ; code1($1->offset) ; }
             else code2(AE_PUSHA, $1->stval.array) ;
             if ( $6 == '+' )  code1(_POST_INC) ;
             else  code1(_POST_DEC) ;

             $$ = $2 ;
           }
        ;

/* delete A[i] */
statement :  DELETE  ID mark LBOX args RBOX separator
             { 
               $$ = $3 ;
               if ( $5 > 1 ) { code1(A_CAT) ; code1($5) ; }
               check_array($2) ;
               code_array($2) ;
               code1(A_DEL) ;
             }

          ;

/*  for ( i in A )  statement */

array_loop_front :  FOR LPAREN ID IN ID RPAREN
                    { eat_nl() ; BC_new() ;
                      $$ = code_ptr ;

                      check_var($3) ;
                      code_address($3) ;
                      check_array($5) ;
                      code_array($5) ;

                      code2(SET_ALOOP, (INST*)0) ;
                    }
                 ;

/* array_loop */
statement  :  array_loop_front  statement
              { 
	        $2[-1].op = code_ptr - $2 + 1 ;
                BC_clear( code_ptr+3 , code_ptr) ;
		code_jmp(ALOOP, $2) ;
		code_ptr++->ptr = (PTR) ZMALLOC(ALOOP_STATE) ;
              }
           ;

/*  fields   
    D_ID is a special token , same as an ID, but yylex()
    only returns it after a '$'.  In essense,
    DOLLAR D_ID is really one token.
*/

field   :  FIELD
           { $$ = code_ptr ; code2(F_PUSHA, $1) ; }
        |  DOLLAR  D_ID
           { check_var($2) ;
             $$ = code_ptr ;
             if ( is_local($2) )
             { code1(L_PUSHI) ; code1($2->offset) ; }
             else code2(_PUSHI, $2->stval.cp) ;

	     CODE_FE_PUSHA() ;
           }
        |  DOLLAR  D_ID mark LBOX  args RBOX
           { 
             if ( $5 > 1 )
             { code1(A_CAT) ; code1($5) ; }

             check_array($2) ;
             if( is_local($2) )
             { code1(LAE_PUSHI) ; code1($2->offset) ; }
             else code2(AE_PUSHI, $2->stval.array) ;

	     CODE_FE_PUSHA()  ;

             $$ = $3 ;
           }
        |  DOLLAR p_expr
           { $$ = $2 ;  CODE_FE_PUSHA() ; }
        |  LPAREN field RPAREN
           { $$ = $2 ; }
        ;

p_expr   :  field   %prec CAT /* removes field (++|--) sr conflict */
            { field_A2I() ; }
        ;

expr    :  field   ASSIGN   expr { code1(F_ASSIGN) ; }
        |  field   ADD_ASG  expr { code1(F_ADD_ASG) ; }
        |  field   SUB_ASG  expr { code1(F_SUB_ASG) ; }
        |  field   MUL_ASG  expr { code1(F_MUL_ASG) ; }
        |  field   DIV_ASG  expr { code1(F_DIV_ASG) ; }
        |  field   MOD_ASG  expr { code1(F_MOD_ASG) ; }
        |  field   POW_ASG  expr { code1(F_POW_ASG) ; }
        ;

/* split is handled different than a builtin because
   it takes an array and optionally a regular expression as args */

p_expr  :   split_front  split_back 
            { code2(_BUILTIN, bi_split) ; }
        ;

split_front : SPLIT LPAREN expr COMMA ID 
            { $$ = $3 ;
              check_array($5) ;
              code_array($5)  ;
            }
            ;

split_back  :   RPAREN
                { code2(_PUSHI, &fs_shadow) ; }
            |   COMMA expr  RPAREN
                { 
                  if ( $2 == code_ptr - 2 )
                  {
                    if ( code_ptr[-2].op == _MATCH0 )
                        RE_as_arg() ;
                    else
                    if ( code_ptr[-2].op == _PUSHS )
                    { CELL *cp = ZMALLOC(CELL) ;

                      cp->type = C_STRING ;
                      cp->ptr = code_ptr[-1].ptr ;
                      cast_for_split(cp) ;
                      code_ptr[-2].op = _PUSHC ;
                      code_ptr[-1].ptr = (PTR) cp ;
                    }
                  }
                }
            ;



/*  match(expr, RE) */

p_expr : MATCH_FUNC LPAREN expr COMMA re_arg RPAREN
        { $$ = $3 ; 
          code2(_BUILTIN, bi_match) ;
        }
     ;


re_arg   :   expr
             {
               if ( $1 == code_ptr - 2 ) 
               {
                 if ( $1->op == _MATCH0 ) RE_as_arg() ;
                 else
                 if ( $1->op == _PUSHS )
                 { CELL *cp = ZMALLOC(CELL) ;

                   cp->type = C_STRING ;
                   cp->ptr = $1[1].ptr ;
                   cast_to_RE(cp) ;
                   $1->op = _PUSHC ;
                   $1[1].ptr = (PTR) cp ;
                 } 
               }
             }
                


/* exit_statement */
statement      :  EXIT   separator
                    { $$ = code_ptr ;
                      code1(_EXIT0) ; }
               |  EXIT   expr  separator
                    { $$ = $2 ; code1(_EXIT) ; }

return_statement :  RETURN   separator
                    { $$ = code_ptr ;
                      code1(_RET0) ; }
               |  RETURN   expr  separator
                    { $$ = $2 ; code1(_RET) ; }

/* getline */

p_expr :  getline      %prec  GETLINE
          { $$ = code_ptr ;
            code2(F_PUSHA, &field[0]) ;
            code1(_PUSHINT) ; code1(0) ; 
            code2(_BUILTIN, bi_getline) ;
            getline_flag = 0 ;
          }
       |  getline  fvalue     %prec  GETLINE
          { $$ = $2 ;
            code1(_PUSHINT) ; code1(0) ;
            code2(_BUILTIN, bi_getline) ;
            getline_flag = 0 ;
          }
       |  getline_file  p_expr    %prec IO_IN
          { code1(_PUSHINT) ; code1(F_IN) ;
            code2(_BUILTIN, bi_getline) ;
            /* getline_flag already off in yylex() */
          }
       |  p_expr PIPE GETLINE  
          { code2(F_PUSHA, &field[0]) ;
            code1(_PUSHINT) ; code1(PIPE_IN) ;
            code2(_BUILTIN, bi_getline) ;
          }
       |  p_expr PIPE GETLINE   fvalue
          { 
            code1(_PUSHINT) ; code1(PIPE_IN) ;
            code2(_BUILTIN, bi_getline) ;
          }
       ;

getline :   GETLINE  { getline_flag = 1 ; }

fvalue  :   lvalue  |  field  ;

getline_file  :  getline  IO_IN
                 { $$ = code_ptr ;
                   code2(F_PUSHA, field+0) ;
                 }
              |  getline fvalue IO_IN
                 { $$ = $2 ; }
              ;

/*==========================================
    sub and gsub  
  ==========================================*/

p_expr  :  sub_or_gsub LPAREN re_arg COMMA  expr  sub_back
           {
             if ( $6 - $5 == 2 && $5->op == _PUSHS  )
             { /* cast from STRING to REPL at compile time */
               CELL *cp = ZMALLOC(CELL) ;
               cp->type = C_STRING ;
               cp->ptr = $5[1].ptr ;
               cast_to_REPL(cp) ;
               $5->op = _PUSHC ;
               $5[1].ptr = (PTR) cp ;
             }
             code2(_BUILTIN, $1) ;
             $$ = $3 ;
           }
        ;

sub_or_gsub :  SUB  { $$ = bi_sub ; }
            |  GSUB { $$ = bi_gsub ; }
            ;


sub_back    :   RPAREN    /* substitute into $0  */
                { $$ = code_ptr ;
                  code2(F_PUSHA, &field[0]) ; 
                }

            |   COMMA fvalue  RPAREN
                { $$ = $2 ; }
            ;

/*================================================
    user defined functions
 *=================================*/

function_def  :  funct_start  block
                 { resize_fblock($1, code_ptr) ;
                   code_ptr = main_code_ptr ;
                   scope = SCOPE_MAIN ;
                   active_funct = (FBLOCK *) 0 ;
                   restore_ids() ;
                 }
              ;
                   

funct_start   :  funct_head  LPAREN  f_arglist  RPAREN
                 { eat_nl() ;
                   scope = SCOPE_FUNCT ;
                   active_funct = $1 ;
                   main_code_ptr = code_ptr ;

                   if ( $1->nargs = $3 )
                        $1->typev = (char *)
			memset( zmalloc($3), ST_LOCAL_NONE, SIZE_T($3)) ;
                   else $1->typev = (char *) 0 ;
                   code_ptr = $1->code = 
                       (INST *) zmalloc(PAGE_SZ*sizeof(INST)) ;
                 }
              ;
                  
funct_head    :  FUNCTION  ID
                 { FBLOCK  *fbp ;

                   if ( $2->type == ST_NONE )
                   {
                         $2->type = ST_FUNCT ;
                         fbp = $2->stval.fbp = 
                             (FBLOCK *) zmalloc(sizeof(FBLOCK)) ;
                         fbp->name = $2->name ;
                   }
                   else
                   {
                         type_error( $2 ) ;

                         /* this FBLOCK will not be put in
                            the symbol table */
                         fbp = (FBLOCK*) zmalloc(sizeof(FBLOCK)) ;
                         fbp->name = "" ;
                   }
                   $$ = fbp ;
                 }

              |  FUNCTION  FUNCT_ID
                 { $$ = $2 ; 
                   if ( $2->code ) 
                       compile_error("redefinition of %s" , $2->name) ;
                 }
              ;
                         
f_arglist  :  /* empty */ { $$ = 0 ; }
           |  f_args
           ;

f_args     :  ID
              { $1 = save_id($1->name) ;
                $1->type = ST_LOCAL_NONE ;
                $1->offset = 0 ;
                $$ = 1 ;
              }
           |  f_args  COMMA  ID
              { if ( is_local($3) ) 
                  compile_error("%s is duplicated in argument list",
                    $3->name) ;
                else
                { $3 = save_id($3->name) ;
                  $3->type = ST_LOCAL_NONE ;
                  $3->offset = $1 ;
                  $$ = $1 + 1 ;
                }
              }
           ;

/* a call to a user defined function */
             
p_expr  :  FUNCT_ID mark  call_args
           { $$ = $2 ;
             code2(_CALL, $1) ;

             if ( $3 )  code1($3->arg_num+1) ;
             else  code1(0) ;
               
             check_fcall($1, scope, active_funct, 
                         $3, token_lineno) ;
           }
        ;

call_args  :   LPAREN   RPAREN
               { $$ = (CA_REC *) 0 ; }
           |   ca_front  ca_back
               { $$ = $2 ;
                 $$->link = $1 ;
                 $$->arg_num = $1 ? $1->arg_num+1 : 0 ;
               }
           ;

/* The funny definition of ca_front with the COMMA bound to the ID is to
   force a shift to avoid a reduce/reduce conflict
   ID->id or ID->array

   Or to avoid a decision, if the type of the ID has not yet been
   determined
*/

ca_front   :  LPAREN
              { $$ = (CA_REC *) 0 ; }
           |  ca_front  expr   COMMA
              { $$ = (CA_REC *) zmalloc(sizeof(CA_REC)) ;
                $$->link = $1 ;
                $$->type = CA_EXPR  ;
                $$->arg_num = $1 ? $1->arg_num+1 : 0 ;
              }
           |  ca_front  ID   COMMA
              { $$ = (CA_REC *) zmalloc(sizeof(CA_REC)) ;
                $$->link = $1 ;
                $$->arg_num = $1 ? $1->arg_num+1 : 0 ;

                code_call_id($$, $2) ;
              }
           ;

ca_back    :  expr   RPAREN
              { $$ = (CA_REC *) zmalloc(sizeof(CA_REC)) ;
                $$->type = CA_EXPR ;
              }

           |  ID    RPAREN
              { $$ = (CA_REC *) zmalloc(sizeof(CA_REC)) ;
                code_call_id($$, $1) ;
              }
           ;


    

%%

/* resize the code for a user function */

static void  resize_fblock( fbp, code_ptr )
  FBLOCK *fbp ;
  INST *code_ptr ;
{ int size ;

  code1(_RET0) ; /* make sure there is always a return statement */

#if !SM_DOS
  if ( dump_code )  
  { code1(_HALT) ; /*stops da() */
    add_to_fdump_list(fbp) ;
  }
#endif

  if ( (size = code_ptr - fbp->code) > PAGE_SZ-1 )
        overflow("function code size", PAGE_SZ ) ;

  /* resize the code */
  fbp->code = (INST*) zrealloc(fbp->code, PAGE_SZ*sizeof(INST),
                       size * sizeof(INST) ) ;

}


/* convert FE_PUSHA  to  FE_PUSHI
   or F_PUSH to F_PUSHI
*/

static void  field_A2I()
{ CELL *cp ;

  if ( code_ptr[-1].op == FE_PUSHA &&
       code_ptr[-1].ptr == (PTR) 0)
  /* On most architectures, the two tests are the same; a good
     compiler might eliminate one.  On LM_DOS, and possibly other
     segmented architectures, they are not */
  { code_ptr[-1].op = FE_PUSHI ; }
  else
  {
    cp = (CELL *) code_ptr[-1].ptr ;

    if ( cp == field  ||

#if  LM_DOS
	 SAMESEG(cp,field) &&
#endif
         cp > NF && cp <= LAST_PFIELD )
    {
         code_ptr[-2].op = _PUSHI  ;
    }
    else if ( cp == NF )
    { code_ptr[-2].op = NF_PUSHI ; code_ptr-- ; }

    else
    { 
      code_ptr[-2].op = F_PUSHI ;
      code_ptr -> op = field_addr_to_index( code_ptr[-1].ptr ) ;
      code_ptr++ ;
    }
  }
}

/* we've seen an ID in a context where it should be a VAR,
   check that's consistent with previous usage */

static void check_var( p )
  register SYMTAB *p ;
{
      switch(p->type)
      {
        case ST_NONE : /* new id */
            p->type = ST_VAR ;
            p->stval.cp = new_CELL() ;
            p->stval.cp->type = C_NOINIT ;
            break ;

        case ST_LOCAL_NONE :
            p->type = ST_LOCAL_VAR ;
            active_funct->typev[p->offset] = ST_LOCAL_VAR ;
            break ;

        case ST_VAR :
        case ST_LOCAL_VAR :  break ;

        default :
            type_error(p) ;
            break ;
      }
}

/* we've seen an ID in a context where it should be an ARRAY,
   check that's consistent with previous usage */
static  void  check_array(p)
  register SYMTAB *p ;
{
      switch(p->type)
      {
        case ST_NONE :  /* a new array */
            p->type = ST_ARRAY ;
            p->stval.array = new_ARRAY() ;
            break ;

        case  ST_ARRAY :
        case  ST_LOCAL_ARRAY :
            break ;

        case  ST_LOCAL_NONE  :
            p->type = ST_LOCAL_ARRAY ;
            active_funct->typev[p->offset] = ST_LOCAL_ARRAY ;
            break ;

        default : type_error(p) ; break ;
      }
}

static void code_array(p)
  register SYMTAB *p ;
{ if ( is_local(p) )
  { code1(LA_PUSHA) ; code1(p->offset) ; }
  else  code2(A_PUSHA, p->stval.array) ;
}


static  int  current_offset()
{
  switch( scope )
  { 
    case  SCOPE_MAIN :  return code_ptr - main_start ;
    case  SCOPE_BEGIN :  return code_ptr - begin_code.start ;
    case  SCOPE_END   :  return code_ptr - end_code.start ;
    case  SCOPE_FUNCT :  return code_ptr - active_funct->code ;
  }
  /* can't get here */
  return 0 ;
}

/* we've seen an ID as an argument to a user defined function */

static void  code_call_id( p, ip )
  register CA_REC *p ;
  register SYMTAB *ip ;
{ static CELL dummy ;

  switch( ip->type )
  {
    case  ST_VAR  :
            p->type = CA_EXPR ;
            code2(_PUSHI, ip->stval.cp) ;
            break ;

    case  ST_LOCAL_VAR  :
            p->type = CA_EXPR ;
            code1(L_PUSHI) ;
            code1(ip->offset) ;
            break ;

    case  ST_ARRAY  :
            p->type = CA_ARRAY ;
            code2(A_PUSHA, ip->stval.array) ;
            break ;

    case  ST_LOCAL_ARRAY :
            p->type = CA_ARRAY ;
            code1(LA_PUSHA) ;
            code1(ip->offset) ;
            break ;

    /* not enough info to code it now; it will have to
       be patched later */

    case  ST_NONE :
            p->type = ST_NONE ;
            p->call_offset = current_offset() ;
            p->sym_p = ip ;
            code2(_PUSHI, &dummy) ;
            break ;

    case  ST_LOCAL_NONE :
            p->type = ST_LOCAL_NONE ;
            p->call_offset = current_offset() ;
            p->type_p = & active_funct->typev[ip->offset] ;
            code1(L_PUSHI) ; 
            code1(ip->offset) ;
            break ;

  
#ifdef   DEBUG
    default :
            bozo("code_call_id") ;
#endif

  }
}

/* an RE by itself was coded as _MATCH0 , change to
   push as an expression */

static void RE_as_arg()
{ CELL *cp = ZMALLOC(CELL) ;

  code_ptr -= 2 ;
  cp->type = C_RE ;
  cp->ptr = code_ptr[1].ptr ;
  code2(_PUSHC, cp) ;
}


int parse()
{ int yy = yyparse() ;

#if  YYBYACC
  extern struct yacc_mem *yacc_memp ;

  yacc_memp++  ; /* puts parser tables in mem pool */
#endif

  if ( resolve_list )  resolve_fcalls() ;
  return yy ;
}

