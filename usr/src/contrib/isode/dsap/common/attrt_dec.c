#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/syntaxes.h"

extern LLog * log_dsap;

#ifdef AttrT_decode
#undef AttrT_decode
#endif

AttributeType AttrT_decode_aux (oid)
OID oid;
{
register AttributeType x;

	if (oid == NULLOID) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Null oid to decode"));
		return NULLTABLE_ATTR;
	}

	if ((x = oid2attr (oid)) == NULLTABLE_ATTR) {
		if ((x = AttrT_new (sprintoid(oid))) == NULLTABLE_ATTR) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("Unknown attribute type oid %s",sprintoid(oid)));
			return NULLTABLE_ATTR;
		}
	}

	return x;
}

