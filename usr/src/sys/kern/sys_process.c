/*	sys_process.c	5.1	82/07/15	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/reg.h"
#include "../h/text.h"
#include "../h/seg.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/psl.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/vlimit.h"
#include "../h/acct.h"

