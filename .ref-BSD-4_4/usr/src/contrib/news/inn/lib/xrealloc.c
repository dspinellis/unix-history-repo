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
**  Reallocate some memory or call the memory failure handler.
*/
ALIGNPTR
xrealloc(p, i)
    char		*p;
    unsigned int	i;
{
    POINTER		new;

    while ((new = realloc((POINTER)p, i)) == NULL)
	(*xmemfailure)("remalloc", i);
    return CAST(ALIGNPTR, new);
}
