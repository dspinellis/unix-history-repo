/*	vmsys.c	2.1	1/5/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/reg.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/pte.h"

vfork()
{

	fork1(1);
}

vread()
{

	rdwr(FREAD, 1);
}

vwrite()
{

	rdwr(FWRITE, 1);
}
