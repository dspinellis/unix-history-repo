
/********************************************
init.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	init.h,v $
 * Revision 5.1  91/12/05  07:59:22  brennan
 * 1.1 pre-release
 * 
*/

/* init.h  */


#ifndef  INIT_H
#define  INIT_H

#include "symtype.h"

/* nodes to link file names for multiple
   -f option */

typedef struct pfile {
struct pfile *link ;
char *fname ;
} PFILE ;

extern PFILE *pfile_list ;

extern char *sprintf_buff, *sprintf_limit ;


void  PROTO( initialize, (int, char **) ) ;
void  PROTO( code_init, (void) ) ;
void  PROTO( code_cleanup, (void) ) ;
void  PROTO( compile_cleanup, (void) ) ;
void PROTO(scan_init, ( char *) ) ;
void PROTO(scan_cleanup, (void) ) ;
void PROTO(bi_vars_init, (void) ) ;
void PROTO(bi_funct_init, (void) ) ;
void PROTO(print_init, (void) ) ;
void PROTO(kw_init, (void) ) ;
void  PROTO( field_init, (void) ) ;
void  PROTO( fpe_init, (void) ) ;
void  PROTO( load_environ, (ARRAY)) ;
void  PROTO( set_stderr, (void)) ;

#endif   /* INIT_H  */
