#include "quipu/util.h"
#include "quipu/name.h"

dn_print (ps,dn,format)
DN  dn;
PS   ps;
int  format;
{
register DN eptr;

	if (dn == NULLDN) {
		if (format == READOUT)
			ps_print (ps,"NULL DN");
		return ;
	}

	if (format == UFNOUT) {
	    ufn_dn_print (ps, dn, 1);
	    return;
	}

	dn_comp_print (ps,dn,format);
		for (eptr = dn->dn_parent; eptr != NULLDN; eptr = eptr->dn_parent) {
			switch (format) {
				case DIROUT:  ps_print (ps,"/"); break;
				case FILEOUT:
				case RDNOUT:
				case EDBOUT:  ps_print (ps,"@"); break;
				case READOUT: ps_print (ps,"\n\t\t\t"); break;
			}
			dn_comp_print (ps,eptr,format);
		}
}

/*  */

static DN localdn = NULLDN;
extern char * local_dit;

ufn_dn_print (ps,dn,multiline)
PS  ps;
DN  dn;
int multiline;
{
	if (dn == NULLDN)
		return;
		
	if (localdn == NULLDN) 
		localdn = str2dn (local_dit);

	(void) ufn_dn_print_aux (ps,dn,localdn,multiline);
}


int	ufn_indent = 16;

ufn_dn_print_aux (ps,dn,marker,multiline)
PS  ps;
DN  dn;
DN marker;
int	multiline;
{
DN next = NULLDN;
char res = 0;
char this_one = TRUE;

	if ((marker != NULLDN) && (dn_comp_cmp(dn,marker) == 0)) {
		next = marker->dn_parent;
		if (dn->dn_parent != NULLDN)
			this_one = FALSE;
	}

	if (dn->dn_parent != NULLDN)
		res = ufn_dn_print_aux (ps,dn->dn_parent,next,multiline);

	if (this_one) {
		if (res) {
			if (multiline < 0 || (multiline && res > 1)) {
					ps_printf (ps,",\n");
					if (ufn_indent > 0)
					    ps_printf (ps, "%*s", ufn_indent, "");
			} else
				ps_print (ps,", ");
		} 
		ufn_rdn_print (ps,dn->dn_rdn);
	}
	return ++res;
}


ufn_rdn_print (ps,rdn)
RDN  rdn;
PS   ps;
{
register RDN eptr;

	if (rdn ==  NULLRDN) 
		return;

	AttrV_print (ps,&rdn->rdn_av,READOUT);

	if (rdn->rdn_next != NULLRDN)
		for (eptr=rdn->rdn_next; eptr!=NULLRDN; eptr=eptr->rdn_next) {
			ps_print (ps," + "); 
			AttrV_print (ps,&eptr->rdn_av,READOUT);
		}

}

static PS ps = NULLPS;

char   *dn2str (dn)
DN	dn;
{
    char       *cp;

    if (ps == NULL
	    && ((ps = ps_alloc (str_open)) == NULLPS)
		    || str_setup (ps, NULLCP, BUFSIZ, 0) == NOTOK) {
	if (ps)
	    ps_free (ps), ps = NULLPS;

	return NULLCP;
    }

    dn_print (ps, dn, EDBOUT);
    ps_print (ps, " ");
    *--ps -> ps_ptr = NULL, ps -> ps_cnt++;

    cp = ps -> ps_base;

    ps -> ps_base = NULL, ps -> ps_cnt = 0;
    ps -> ps_ptr = NULL, ps -> ps_bufsiz = 0;

    return cp;
}

char   *dn2ufn (dn,multiline)
DN	dn;
int    multiline;
{
    char       *cp;

    if (ps == NULL
	    && ((ps = ps_alloc (str_open)) == NULLPS)
		    || str_setup (ps, NULLCP, BUFSIZ, 0) == NOTOK) {
	if (ps)
	    ps_free (ps), ps = NULLPS;

	return NULLCP;
    }

    ufn_dn_print (ps, dn, multiline);
    ps_print (ps, " ");
    *--ps -> ps_ptr = NULL, ps -> ps_cnt++;

    cp = ps -> ps_base;

    ps -> ps_base = NULL, ps -> ps_cnt = 0;
    ps -> ps_ptr = NULL, ps -> ps_bufsiz = 0;

    return cp;
}
