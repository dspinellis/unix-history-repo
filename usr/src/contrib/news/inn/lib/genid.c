/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include "clibrary.h"
#include "libinn.h"


/* Scale time back a bit, for shorter Message-ID's. */
#define OFFSET	673416000L

char *
GenerateMessageID()
{
    static char		buff[SMBUF];
    char		*p;
    char		sec32[10];
    char		pid32[10];
    TIMEINFO		Now;

    if (GetTimeInfo(&Now) < 0)
	return NULL;
    Radix32((unsigned long)Now.time - OFFSET, sec32);
    Radix32((unsigned long)getpid(), pid32);
    if ((p = GetFQDN()) == NULL)
	return NULL;
    (void)sprintf(buff, "<%s$%s@%s>", sec32, pid32, p);
    return buff;
}
