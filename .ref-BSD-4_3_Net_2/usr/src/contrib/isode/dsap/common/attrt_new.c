#include "quipu/util.h"
#include "quipu/name.h"

AttributeType AttrT_new (name)
register char * name;
{
oid_table * Current;
extern oid_table_attr attrOIDTable [];
oid_table_attr  *res;
extern attrNumEntries;
char * get_oid ();

	if ((res = name2attr (name)) == NULLTABLE_ATTR) {
		/* attribute not in tables, add as "ASN" if possible */
		char * ptr;
		OID oid;

		if ((ptr = get_oid (name)) == NULLCP) 
			return (NULLTABLE_ATTR);

		Current = &attrOIDTable[attrNumEntries].oa_ot;
		Current->ot_name = strdup(name);
		add_entry_aux (Current->ot_name,(caddr_t)&attrOIDTable[attrNumEntries],2,NULLCP);
		Current->ot_stroid = strdup(ptr);

		oid = str2oid (Current->ot_stroid);
		if (oid == NULLOID)
			Current->ot_oid = NULLOID;
		else
			Current->ot_oid = oid_cpy (oid);
		attrOIDTable[attrNumEntries].oa_syntax = 0;
		return (&attrOIDTable[attrNumEntries++]);
	}
	return (res);
}

