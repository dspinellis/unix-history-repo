/*  $Revision: 1.6 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "configdata.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Allocate some memory or call the memory failure handler.
*/
ALIGNPTR
xmalloc(i)
    unsigned int	i;
{
    POINTER		new;

    while ((new = malloc(i)) == NULL)
	(*xmemfailure)("malloc", i);
    return CAST(ALIGNPTR, new);
}
