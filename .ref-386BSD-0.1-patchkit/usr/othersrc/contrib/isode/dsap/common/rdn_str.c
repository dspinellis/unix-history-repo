#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/malloc.h"

RDN str2rdn_aux (str)
char * str;
{
AttributeType at;
char * ptr;
char * save, val;
char * TidyString();
RDN rdn;
unsigned last_heap;

	/* look for "type = value" */

	if (str == NULLCP) {
		parse_error ("NULL rdn component",NULLCP);
		return (NULLRDN);
	}

	if ((ptr=index(str,'=')) == 0) {
		parse_error ("Equals missing in RDN '%s'",str);
		return (NULLRDN);
	}

	save = ptr++;
	save--;
	if (! isspace (*save))
		save++;
	val = *save;
	*save = 0;

	if ((at = AttrT_new (TidyString(str))) == NULLTABLE_ATTR) {
		parse_error ("Unknown attribute type in RDN '%s'",str);
		*save = val;
		return (NULLRDN);
	}

	rdn = rdn_comp_alloc();
	rdn->rdn_next = NULLRDN;
	rdn->rdn_at = at;

	if ((last_heap = mem_heap) == 1)
		mem_heap = 2 + attr_index;

	if (str_at2AttrV_aux (ptr,rdn->rdn_at,&rdn->rdn_av) == NOTOK) {
		*save = val;
		mem_heap = last_heap;
		free ((char *) rdn);
		return (NULLRDN);
	}

	mem_heap = last_heap;
	*save = val;
	return (rdn);
}


RDN str2rdn (str)
char * str;
{
register char *ptr;
register char *save,val;
RDN rdn = NULLRDN, newrdn;

	/* look for "rdn % rdn % rdn" */

	if (str == NULLCP)
		return (NULLRDN);

	while ( (ptr = index (str,'%')) != 0) {
		save = ptr++;
		save--;
		if (! isspace (*save))
			save++;
		val = *save;
		*save = 0;
		if ((newrdn = str2rdn_aux (str)) == NULLRDN) {
			rdn_free (rdn);
			return (NULLRDN);
		}

		rdn = rdn_merge (rdn,newrdn);
		*save = val;
		str = ptr;
	}

	if ((newrdn = str2rdn_aux (str)) == NULLRDN) {
		rdn_free (rdn);
		return (NULLRDN);
	}

	return (rdn_merge (rdn,newrdn));
}
