/* $Header: CHstrtoname.c,v 2.1 87/03/10 17:22:06 jqj Exp $ */

/* contains:
 * CH_NameToString
 * CH_StringToName
 */

/* $Log:	CHstrtoname.c,v $
 * Revision 2.1  87/03/10  17:22:06  jqj
 * added CH_NameToString
 * 
 * Revision 2.0  85/11/21  07:22:34  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/03/26  06:27:02  jqj
 * Initial revision
 * 
 * Revision 1.1  85/03/26  06:27:02  jqj
 * Initial revision
 * 
 */

#include <xnscourier/Clearinghouse2.h>

static char *copy();

/*
 * Given an ObjectName, produce a string in standard format.
 * N.B. should escape ":" in the components!
 */
char *
CH_NameToString(chs_name)
	Clearinghouse2_ObjectName chs_name;
{
	static char buf[84];
	extern char *sprintf();
	return sprintf(buf,"%.40s:%.20s:%.20s", 
			chs_name.object,
			chs_name.domain,
			chs_name.organization);
}

/*
 * Given a string in standard format, return an ObjectName.
 * If the string is incomplete, e.g. "jqj" or "::cornell-univ", fill 
 * in default values from defaults.
 * N.B. should handle escaped ":" in the compenents.
 */
Clearinghouse2_ObjectName
CH_StringToName(str,defaults)
	char *str;
	Clearinghouse2_ObjectName *defaults;
{
	register char *s1;
	extern char *index();
	register Clearinghouse2_ObjectName result;

	s1 = result.object = (str ? copy(str) : "");
	if (s1 = index(s1,':')) {
		*s1 = '\0';
		result.domain = ++s1;
		if (s1 = index(s1,':')) {
			*s1 = '\0';
			result.organization = ++s1;
		}
		else
			result.organization = "";
	}
	else
		result.organization = result.domain = "";
	/* now fill in defaults if any */
	if (defaults == (Clearinghouse2_ObjectName*) NULL)
		return(result);
	if (*result.object == '\0' && defaults->object != NULL)
		result.object = copy(defaults->object);
	if (*result.domain == '\0' && defaults->domain != NULL)
		result.domain = copy(defaults->domain);
	if (*result.organization == '\0' && defaults->organization != NULL)
		result.organization = copy(defaults->organization);
	return(result);
}


static char *
copy(s)
	char *s;
{
	char *p;
	extern char *malloc();

	if ((p = malloc(strlen(s) + 1)) == NULL) {
		exit(1);
	}
	(void) strcpy(p, s);
	return (p);
}

