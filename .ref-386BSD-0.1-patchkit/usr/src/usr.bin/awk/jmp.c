
/********************************************
jmp.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	jmp.c,v $
 * Revision 5.1  91/12/05  07:56:10  brennan
 * 1.1 pre-release
 * 
*/

/* this module deals with back patching jumps, breaks and continues,
   and with save and restoring code when we move code.
   There are three stacks.  If we encounter a compile error, the
   stacks are frozen, i.e., we do not attempt error recovery
   on the stacks
*/


#include "mawk.h"
#include "jmp.h"
#include "code.h"
#include "sizes.h"
#include "init.h"
#include "memory.h"

extern unsigned compile_error_count ;
#define error_state  (compile_error_count>0)


/*---------- back patching jumps  ---------------*/

typedef  struct jmp {
struct jmp *link ;
INST *source ;
} JMP ;

static JMP *jmp_top ;

void code_jmp( jtype, target)
  int jtype ; 
  INST *target ;
{ 
  register INST *source ;

  if (error_state)  return ;

  code1(jtype) ;
  source = code_ptr++ ;

  if ( target ) source->op = target - source ;
  else  /* save source on jump stack */
  {
    register JMP *p = (JMP*) zmalloc(sizeof(JMP)) ;
    p->source = source ;
    p->link = jmp_top ;
    jmp_top = p ;
  }
}

void patch_jmp(target)  /* patch a jump on the jmp_stack */
  INST *target ;
{ register JMP *p ;

  if ( ! error_state )
  {
#ifdef  DEBUG
    if (!jmp_top) bozo("jmp stack underflow") ;
#endif

    p = jmp_top ; jmp_top = p->link ;

    p->source->op = target - p->source ;

    zfree(p, sizeof(JMP)) ;
  }
}


/*-- break and continue -------*/

typedef struct bc {
struct bc *link ;  /* stack as linked list */
int type ;         /* 'B' or 'C' or mark start with 0 */
INST *source ;     /* position of _JMP  */
} BC ;

static BC *bc_top ;  



void BC_new()  /* mark the start of a loop */
{ 
  BC_insert(0, (INST*) 0 ) ;  
}

void BC_insert(type, address)
  int type ; INST *address ;
{ register BC * p  ;

  if ( error_state )  return ;

  if ( type && ! bc_top )
  {
    compile_error("%s statement outside of loop",
      type == 'B' ? "break" : "continue" ) ;
    
    return ;
  }
  else
  {
    p = (BC*) zmalloc(sizeof(BC)) ;
    p->type = type ;
    p->source = address ;
    p->link = bc_top ;
    bc_top = p ;
  }
}


void BC_clear(B_address, C_address)  
/* patch all break and continues for one loop */
INST *B_address, *C_address ;
{ register  BC *p , *q ;

  if (error_state) return ;

  p = bc_top ;
  /* pop down to the mark node */
  while ( p->type )
  {
    p->source->op = (p->type == 'B' ? B_address : C_address)
		      - p->source ;

    q = p ; p = p->link ; zfree(q, sizeof(BC)) ;
  }
  /* remove the mark node */
  bc_top = p->link ;
  zfree(p, sizeof(BC)) ;
}

/*-----  moving code --------------------------*/

/* a stack to hold some pieces of code while 
   reorganizing loops .
   This used to be used on all loops.  Now it is used
   only for the 3rd expression on a for loop and
   for the fist part of a range pattern
*/

typedef  struct mc {  /* mc -- move code */
struct mc *link ;
INST *code ;
unsigned len ;
}  MC ;

static MC *mc_top ;


void code_push( code, len)
  INST *code ; unsigned len ;
{ 
  register MC *p ;

  if (! error_state ) 
  {
    p = (MC*) zmalloc(sizeof(MC)) ;
    p->len = len ;
    p->link = mc_top ;
    mc_top = p ;

    if ( len )
    {
      p->code = (INST*) zmalloc(sizeof(INST)*len) ;
      (void) memcpy(p->code, code, SIZE_T(sizeof(INST)*len)) ;
    }
  }
}

/* copy the code at the top of the mc stack to target.
   return the number of INSTs moved */

unsigned code_pop(target) 
  INST *target ;
{ 
  register MC *p ;
  unsigned retval ;

  if (error_state)  return 0 ;

#ifdef  DEBUG
  if ( ! mc_top ) bozo("mc underflow") ;
#endif

  p = mc_top ; mc_top = p->link ;
  
  if ( retval = p->len )
  {
    (void) memcpy(target, p->code, SIZE_T(p->len*sizeof(INST))) ;
    zfree(p->code, p->len*sizeof(INST)) ; 
  }

  zfree(p, sizeof(MC)) ;
  return retval ;
}
