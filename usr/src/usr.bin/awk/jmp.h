
/********************************************
jmp.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	jmp.h,v $
 * Revision 5.1  91/12/05  07:59:24  brennan
 * 1.1 pre-release
 * 
*/

#ifndef   JMP_H
#define   JMP_H

void  PROTO(BC_new, (void) ) ;
void  PROTO(BC_insert, (int, INST*) ) ;
void  PROTO(BC_clear, (INST *, INST *) ) ;
void  PROTO(code_push, (INST *, unsigned) ) ;
unsigned  PROTO(code_pop, (INST *) ) ;
void  PROTO(code_jmp, (int, INST *) ) ;
void  PROTO(patch_jmp, (INST *) ) ;


#endif  /* JMP_H  */

