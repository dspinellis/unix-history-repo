/*  init_main.c:     Static Constructor Initialization for Dynamically
 *                    Linked Libraries
 *  Author:           James Kempf
 *  Created On:       Fri Dec 21 08:31:07 1990
 *  Last Modified By: James Kempf
 *  Last Modified On: Wed Apr 24 10:41:25 1991
 *  Update Count:     46
*/


#include "dldefs.h"

/* This global is used to communicate the ctor/dtor list address to 
 * dynamically linked libraries. Some static linkers may have trouble
 * with initialized data in PIC code. gcc can generate initialization
 * functions for any user data, but the ctor/dtor lists are generated
 * by the linker. So we use this ruse to make sure the address gets
 * to the initialization function. This code should *always* run single
 * threaded.
*/
int * __function_list_addr = 0;

extern int __CTOR_LIST__;
extern int __DTOR_LIST__;

int __main();
void INIT_FUN();
void FINI_FUN();
void _initialize_dynamic_libs();
void _finalize_dynamic_libs();
void exit(/*int*/);
void _exit(/*int*/);
void _cleanup();

/*************************************************************************
 * Main initialization and finalization
 ************************************************************************/

/* __main-Initialize dynamically linked libraries, then main. */

int __main()
{
    _initialize_dynamic_libs();
    __function_list_addr = &__CTOR_LIST__;
    INIT_FUN();
    return(0);
}

void exit( int status)
{
    __function_list_addr = &__DTOR_LIST__;
    FINI_FUN(); 
    _finalize_dynamic_libs();
    _cleanup();
    _exit(status);
}
