/* 
 *  init.cc:          Initialization and Finalization Code for Shared
 *                    Libraries.
 *  Author:           James Kempf
 *  Created On:       Fri Mar 29 14:01:48 1991
 *  Last Modified By: James Kempf
 *  Last Modified On: Wed Apr 24 10:34:21 1991
 *  Update Count:     7
 */

#include "dldefs.h"


/*************************************************************************
 *              void INIT_FUN()
 *              void FINI_FUN()
 * Shared libraries should link a copy of this module into them. The
 * dynamic initialization/finalization function can look up these
 * functions and call them to do static ctor/dtor work. Also, the
 * main uses these for statically linked programs.
 ************************************************************************/


static void __do_global_fn( int * );

/* 
 * Semaphore for indicating whether the library has already been initialized. 
 */
static int initialized = 0;

/*
 * We do this, because init.c, in addition to being compiled standalone, 
 * is included in drt0.cc. 
 */
extern int * __function_list_addr;

void INIT_FUN()
{
    if ( __function_list_addr && ! initialized ) {
	initialized = 1;
	__do_global_fn( __function_list_addr);
    }
}

void FINI_FUN()
{
    if ( __function_list_addr && initialized ) {
	initialized = 0;
	__do_global_fn( __function_list_addr );
    }
}

inline static
int * _do_local_fn( int * off_ptr)
{
  void_fn fn;
  int i;

  i = *off_ptr;
  off_ptr++;

  while( --i >= 0) {
    fn = (void_fn)(((int)off_ptr) - ((int)(*off_ptr)));
    fn();
    off_ptr++;
  }

  return (* off_ptr ? (int*) *off_ptr : off_ptr);
}

static
void __do_global_fn( int * ip)
{

  while( *ip > 0) {
    ip = _do_local_fn(ip);
  }
  
}
