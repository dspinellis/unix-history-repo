/* 
 *  dynamic_lib.c:    Dynamic Library Initialization for Unix
 *  Author:           James Kempf
 *  Created On:       Fri Apr  5 08:34:47 1991
 *  Last Modified By: James Kempf
 *  Last Modified On: Tue Aug  6 15:00:47 1991
 *  Update Count:     14
 */

/* Exports:
 *   void _initialize_dynamic_libs()
 *   void _finalize_dynamic_libs()
 *
 * In case of static linking these are just dummy functions.
 * In case of dynamic shared objects (libraries):
 * The dynamic shared objects are already linked with the main program and 
 * control was passed from `start' (in `crt0.o') to `_main' and from that
 * to `__main' in `drt0.o'.
 *
 * The functions here have to do the _initialization_ and _finalization_ of
 * all the dynamic shared objects (the initialization and finalization of the 
 * main program is done by the calling function).
 */


#ifndef DYNAMIC_LIBS
/*
 * static case
 */

void _initialize_dynamic_libs() {}
void _finalize_dynamic_libs() {}

#else
/*
 * dynamic case
 *
 *   
 */

#include <sys/types.h>
#include <link.h>
#include <dlfcn.h>
#include <malloc.h>
#include "dldefs.h"

extern struct link_dynamic _DYNAMIC;

/* Unix linker looks up global symbols in first one down the line  (?? H.S.)*/
const char CTOR_LIST_NAME[] = "___CTOR_LIST__";
const char DTOR_LIST_NAME[] = "___DTOR_LIST__";

extern int * __function_list_addr;

/* "Handles" to the dynamic shared objects can be obtained via the _DYNAMIC 
 * structure; but since be have to do finalization in the opposite direction
 * as initialization, we construct a doubly linked list of these handles.
 */

struct _sll {
    struct _sll* next;
    struct _sll* prev;
    struct link_map* lmp;
};

#define NULLP (struct _sll*)0

static struct _sll* _sll_head = NULLP;
static struct _sll* _sll_tail = NULLP;

static inline
void new(struct link_map* nlmp)
{
    struct _sll * this = (struct _sll *) malloc (sizeof (struct _sll));

    this->lmp  = nlmp;
    this->next = NULLP;
    this->prev = NULLP;

    if( !_sll_head ) {
	_sll_head = this;
	_sll_tail = this;
    } else {
	this->prev = _sll_tail;
	_sll_tail->next = this;
	_sll_tail = this;
    }
}

static
void build_sll()
/*
 *  -- Build doubly linked list of dynamic libraries
 */
{
    /* extern struct link_dynamic _DYNAMIC; */
    struct link_dynamic_2* ldd;
    struct link_map* lmp;

    ldd  = _DYNAMIC.ld_un.ld_2;

    for( lmp = ldd->ld_loaded; lmp; lmp = lmp->lm_next ) {
	new (lmp);
    }
}

static
void delete_sll()
/*
 *  -- Delete doubly linked list of dynamic libraries
 */
{
    struct _sll * this = _sll_tail;
    struct _sll * prev;

    while ( this ) {
    	    prev = this->prev;
	    free ( this);
	    this = prev;
    }
}


static
void __do_dynamic_lib( struct _sll * sllp,
		       char * fname,  const char* lname, int forwardp )
/* 
 *   -- Fake out dlsym() into giving me the names of any initialization or
 *      finalization functions and call them.
 */
{

    struct _sll * p    = sllp;
    void *        dobj;
    void_fn       f;

    while( p ) {

	/* Set up the dl_object first */

	dobj = dlopen( p->lmp->lm_name, 1 );

	/* Call dlsym() looking for the function symbol. */

	f = (void_fn)dlsym( dobj, fname);

	/*
	 * Because the Unix dynamic linker resolves global
	 * symbols in the shared library to symbols in main,
	 * we need to use this kludge to get the address of
	 * the function list to the initialization and
	 * finalization functions. If there is no symbol
	 * for the CTOR or DTOR lists, we call the function
	 * anyway, because the library creator may have written their
	 * own initialization function.
	*/

	__function_list_addr = (int*)dlsym((void*)dobj,lname);

	/* If nonnull, call the function. */

	if( f ) {
	    f();
	}

	/*Increment to next list entry, depending on flag.*/

	p = ( forwardp ? p->next : p->prev);
    }
}
	
void _initialize_dynamic_libs()
/* _initialize_dynamic_libs
 *
 *  -- Initialize the dynamically linked libraries.
 */
{
    build_sll();

    __do_dynamic_lib( _sll_tail, INIT_FUN_NAME, CTOR_LIST_NAME, 0);

    delete_sll();
}


void _finalize_dynamic_libs()
/*
 * _finalize_dynamic_libs
 *
 *  -- Finalize the dynamically linked libraries.
 */
{
    /* Have to rebuild sll list, the user may have added something */
    _sll_head = _sll_tail = NULLP;

    build_sll();

    __do_dynamic_lib( _sll_head, FINI_FUN_NAME, DTOR_LIST_NAME, 1);

    delete_sll();
}

#endif /* DYNAMIC_LIBS */
