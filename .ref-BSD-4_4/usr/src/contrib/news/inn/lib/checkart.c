/*  $Revision: 1.1 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#include "nntp.h"
#include "clibrary.h"


/*
**  See if an article is longer than the NNTP line-length limits.
*/
int
NNTPcheckarticle(p)
    register char	*p;
{
    register char	*next;

    for (; p && *p; p = next) {
	if ((next = strchr(p, '\n')) == NULL)
	    break;
	if (next - p > NNTP_STRLEN)
	    return -1;
    }
    return 0;
}
