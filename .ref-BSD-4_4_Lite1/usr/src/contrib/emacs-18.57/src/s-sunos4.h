#include "s-bsd4-2.h"

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#define LD_SWITCH_SYSTEM -e __start -Bstatic

/* In SunOS 4.1, a static function called by tzsetwall reportedly
   clears the byte just past an eight byte region it mallocs, corrupting
   GNU malloc's memory pool.  But Sun's malloc doesn't seem to mind. */

#define SYSTEM_MALLOC

/* 4.1.1 makes these system calls interruptable.  */

#define read sys_read
#define write sys_write
#define open sys_open
#define close sys_close

#define INTERRUPTABLE_OPEN
#define INTERRUPTABLE_CLOSE
#define INTERRUPTABLE_IO
