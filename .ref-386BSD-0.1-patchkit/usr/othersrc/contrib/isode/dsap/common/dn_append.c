#include "quipu/util.h"
#include "quipu/name.h"

extern LLog * log_dsap;

dn_append (a,b)
DN a,b;
{
register DN ptr;
register DN eptr;

	if (a == NULLDN)
		DLOG (log_dsap,LLOG_DEBUG,("appending to null dn!"));
	else {
		for (eptr = a; eptr != NULLDN; eptr = eptr->dn_parent)
			ptr = eptr;
		ptr->dn_parent = b;
	}
}

