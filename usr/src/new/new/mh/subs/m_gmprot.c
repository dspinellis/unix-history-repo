#include "mh.h"
#include <stdio.h>

m_gmprot()
{
	register char *cp;
	register int prot;

	if((cp = m_find("msg-protect")) != NULL)
		prot = atooi(cp);
	else
		prot = atooi(msgprot);
	return(prot);
}
