/*  drt0.c:     Static Constructor Initialization for Dynamically
 *                    Linked Libraries
 *  Author:           James Kempf
 *  Created On:       Fri Dec 21 08:31:07 1990
 *  Last Modified By: James Kempf
 *  Last Modified On: Wed Apr 24 10:41:25 1991
 *  Update Count:     46
*/


/*This horrible, crufty hack is necessary because the people who */
/*  implemented PIC code didn't have enough time to change the   */
/*  linker to make it properly create relocatable files using    */
/*  the -r switch when PIC code was involved.                    */

#include "init_main.c"  /* That's right, .c */
#include "init.c" 
#define DYNAMIC_LIBS
#include "dynamic_lib.c"
