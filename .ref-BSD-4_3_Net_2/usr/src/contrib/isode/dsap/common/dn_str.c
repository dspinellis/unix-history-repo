#include "quipu/util.h"
#include "quipu/name.h"

static char dn_alias;

extern int dn_print ();
extern int dn_cmp ();
extern int dn_free ();
short syntax_dn = 0;

DN str2dn (str)
register char * str;
{
register char *ptr;
char *save,val;
char * aliasptr;
DN dn = NULLDN, newdn;
RDN rdn;
char * alias2name ();
char * SkipSpace ();

	if (str == NULLCP)
		return (NULLDN);

	if (*str == '@')
		str++;		/* Skip leading '@' signs for dish compat */

	while ( (ptr = index (str,'@')) != 0) {
		save = ptr++;
		save--;
		if (! isspace (*save))
			save++;
		val = *save;
		*save = 0;


		if (dn == NULLDN)
			/* try str as an alias */
			if ((aliasptr = alias2name (SkipSpace(str))) != NULLCP) {
				dn_alias = TRUE;
				if ((newdn = str2dn(aliasptr)) == NULLDN) {
					parse_error ("Invalid alias '%s'",aliasptr);
					dn_free (dn);
					return (NULLDN);
				}
				dn = newdn;
				*save = val;
				str = ptr;
				continue;	
			}

		if ((rdn = str2rdn (str)) == NULLRDN) {
			dn_free (dn);
			return (NULLDN);
		} 
		if (dn == NULLDN)
			dn = dn_comp_new (rdn);
		else
			dn_append (dn,dn_comp_new (rdn));
		*save = val;
		str = ptr;
	}


	/* try str as an alias */
	if (dn == NULLDN)
		if ((aliasptr = alias2name (SkipSpace(str))) != NULLCP) {
			dn_alias = TRUE;
			if ((newdn = str2dn(aliasptr)) == NULLDN) {
				parse_error ("Invalid alias '%s'",aliasptr);
				dn_free (dn);
				return (NULLDN);
			}
			return (newdn);
		}

	if ((rdn = str2rdn (str)) == NULLRDN) {
		dn_free (dn);
		return (NULLDN);
	}

	if (dn == NULLDN)
		dn = dn_comp_new (rdn);
	else
		dn_append (dn,dn_comp_new (rdn));

	return (dn);
}

DN str2dn_aux (str,alias)
char * str;
char *alias;
{
DN dn;
	dn_alias = FALSE;
	dn = str2dn (str);
	*alias = dn_alias;
	return (dn);
}

DN str2dnX (str)
register char * str;
{
register char * ptr;

	if ((ptr = rindex (str,'#')) != 0) {
		/* a bit or reverse compatability... */
		if (*++ptr != 0) {
			parse_error ("invalid # in '%s'",str);
			return (NULLDN);
		} else
			*--ptr = 0;
	}
	return (str2dn(str));
}

DN dn_dec (pe)
PE pe;
{
DN adn;
	
	if (decode_IF_DistinguishedName(pe,1,NULLIP,NULLVP,&adn) == NOTOK)
		return (NULLDN);

	return (adn);
}

PE dn_enc (dn)
DN dn;
{
PE ret_pe;

	(void) encode_IF_DistinguishedName (&ret_pe,0,0,NULLCP,dn);
	return (ret_pe);
}

dn_syntax ()
{
	syntax_dn = add_attribute_syntax ("dn",
		(IFP) dn_enc,	(IFP) dn_dec,
		(IFP) str2dnX,	dn_print,
		(IFP) dn_cpy,	dn_cmp,
		dn_free,	NULLCP,
		NULLIFP,	TRUE );
}
