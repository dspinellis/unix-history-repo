/*	kdb_access.c	7.1	86/11/20	*/

/*
 * Access data in kernel address space.
 */

#include "../kdb/defs.h"

/*
 * Primitives: put a value in a space, get a value from a space
 * and get a word or byte not returning if an error occurred.
 */
put(addr, space, value) 
    off_t addr; { (void) kdbpoke(addr, value); }

u_int
get(addr, space)
    off_t addr; { return (kdbpeek(addr)); };

u_int
chkget(addr, space)
    off_t addr; { return (get(addr, space)); }

u_int
bchkget(addr, space) 
    off_t addr; { return (byte(chkget(addr, space))); }
