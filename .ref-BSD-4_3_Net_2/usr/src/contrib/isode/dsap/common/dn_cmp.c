#include "quipu/util.h"
#include "quipu/name.h"

dn_cmp (a,b)
register DN  a,b;
{
int res;

	for (; (a != NULLDN) && (b != NULLDN) ; a = a->dn_parent, b = b->dn_parent)
		if ( (res = rdn_cmp (a->dn_rdn,b->dn_rdn)) != OK) {
			return res;
		}

	if (( a == NULLDN) && (b == NULLDN)) {
		return OK;
	} else {
		return ( a == NULLDN ? 1 : -1 );
	}

}

dn_cmp_prefix (a,b)
register DN  a,b;
{
	for (; a != NULLDN && b != NULLDN ; a = a->dn_parent, b = b->dn_parent)
		if ( dn_comp_cmp (a,b) == NOTOK) {
			return NOTOK;
		}

	if ( a == NULLDN) {
		return OK;
	} else {
		return NOTOK;
	}

}

dn_order_cmp (a,b)
register DN  a,b;
{
        int     i;

        for ( ; (a != NULLDN) && (b != NULLDN); a = a->dn_parent,
            b = b->dn_parent ) {
                if ( (i = rdn_cmp( a->dn_rdn, b->dn_rdn )) != 0 ) {
                        return( i );
                }
        }

        if ( ( a == NULLDN) && (b == NULLDN) ) {
                return( 0 );
        } else if ( b ) {       /* b longer, so a is less */
                return( -1 );
        } else {                /* a must be longer */
                return( 1 );
        }
        /* NOTREACHED */
}

