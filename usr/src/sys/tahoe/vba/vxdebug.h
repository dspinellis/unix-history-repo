/*	vxdebug.h	1.1	85/07/21	*/

#ifdef VX_DEBUG
#define VXERR4		1
#define VXNOBUF		2
extern long vxintr4;

extern long vxdebug;
#define VXVCM	1
#define VXVCC	2
#define VXVCX	4

#include "../sna/snadebug.h"
#endif
