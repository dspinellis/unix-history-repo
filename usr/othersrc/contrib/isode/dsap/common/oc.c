/* oc.c - Object Class routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/oc.c,v 7.6 91/02/22 09:19:43 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/oc.c,v 7.6 91/02/22 09:19:43 mrose Interim $
 *
 *
 * $Log:	oc.c,v $
 * Revision 7.6  91/02/22  09:19:43  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/11/20  15:29:15  mrose
 * cjr
 * 
 * Revision 7.4  90/10/17  11:42:19  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:34:47  mrose
 * sync
 * 
 * Revision 7.2  90/03/15  11:17:41  mrose
 * quipu-sync
 * 
 * Revision 7.1  90/01/11  23:49:43  mrose
 * lint
 * 
 * Revision 7.0  89/11/23  21:42:31  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "tailor.h"

extern LLog * log_dsap;
extern short oc_sntx; 
extern IFP oc_hier;
extern IFP oc_avsprint;

objectclass * oc_add (oid)
OID oid;
{
oid_table * Current;
extern objectclass ocOIDTable[];
extern int ocNumEntries;

	Current = &ocOIDTable[ocNumEntries].oc_ot;
	if (oid == NULLOID)
		Current->ot_oid = NULLOID;
	else
		Current->ot_oid = oid_cpy (oid);
	Current->ot_name = strdup(oid2ode_aux(oid,0));
	Current->ot_stroid = strdup(sprintoid(oid));
	add_entry_aux (Current->ot_name,(caddr_t)&ocOIDTable[ocNumEntries],3,NULLCP);
	ocOIDTable[ocNumEntries].oc_hierachy = NULLOCSEQ;
	ocOIDTable[ocNumEntries].oc_may  = NULLTABLE_SEQ;
	ocOIDTable[ocNumEntries].oc_must = NULLTABLE_SEQ;
	return (&ocOIDTable[ocNumEntries++]);
}

objectclass * str2oc (str)
char * str;
{
char * ptr;
char * get_oid ();
objectclass *oc;

	if ((oc = name2oc (str)) != NULLOBJECTCLASS) 
		return (oc);

	/* unknown object class -- need to add to table */
	if ((ptr = get_oid (str)) == NULLCP) {
		parse_error ("Object class %s unknown",str);
		return (NULLOBJECTCLASS);
	}

	return (oc_add (str2oid(ptr)));
}

static AV_Sequence new_oc_avs (oc)
objectclass * oc;
{
AV_Sequence avs;

	avs = avs_comp_alloc();
	avs->avseq_next = NULLAV;
	avs->avseq_av.av_syntax = oc_sntx;
	avs->avseq_av.av_struct = (caddr_t) oc;
	return (avs);
}

static AV_Sequence str2oc_hier (str)
char * str;
{
AV_Sequence avs = NULLAV;
objectclass * oc;
char * ptr, *save, val;

	str = SkipSpace (str);

	while ((ptr = index (str,'&')) != 0) {
		save = ptr++;
		save--;
		if (! isspace (*save))
			save++;
		val = *save;
		*save = 0;

		if ((oc = str2oc (str)) == NULLOBJECTCLASS)
			return (NULLAV);
		if (avs == NULLAV)
			avs = new_oc_avs (oc);
		else
			add_oc_avs (oc,&avs);
		add_hierarchy (oc,&avs);

		*save = val;
		str = SkipSpace(ptr);
	}

	if ((oc = str2oc (str)) == NULLOBJECTCLASS)
		return (NULLAV);
	if (avs == NULLAV)
		avs = new_oc_avs (oc);
	else
		add_oc_avs (oc,&avs);
	add_hierarchy (oc,&avs);

	return (avs);
}

add_oc_avs (oc,avsp)
objectclass * oc;
AV_Sequence *avsp;
{
AV_Sequence loop;
objectclass *ocp;

	/* see if we already have oc in heirarchy ... */

	for (loop = *avsp; loop != NULLAV; loop = loop->avseq_next) {
		ocp = (objectclass *)loop->avseq_av.av_struct;
		if (oc == ocp)
			return;
	}
	*avsp = avs_merge (*avsp,new_oc_avs(oc));
}

static add_hierarchy (oc,avsp)
objectclass * oc;
AV_Sequence *avsp;
{
struct oc_seq * oidseq;

	for (oidseq = oc->oc_hierachy; 
			oidseq != NULLOCSEQ; oidseq = oidseq->os_next) {
		add_oc_avs (oidseq->os_oc,avsp);
		add_hierarchy (oidseq->os_oc,avsp);
	}
}


static in_hierarchy (a,b)
AV_Sequence a, b;
{
struct oc_seq * oidseq;
objectclass *oca, *ocb;

	if ((a == NULLAV) || (a->avseq_av.av_syntax != oc_sntx) || (a->avseq_av.av_struct == NULL))
		return (FALSE);

	if ((b == NULLAV) || (b->avseq_av.av_syntax != oc_sntx) || (b->avseq_av.av_struct == NULL))
		return (FALSE);

	oca = (objectclass *) a->avseq_av.av_struct;
	ocb = (objectclass *) b->avseq_av.av_struct;

	for (oidseq = ocb->oc_hierachy; 
			oidseq != NULLOCSEQ; oidseq = oidseq->os_next) 
		if (objclass_cmp(oca,oidseq->os_oc) == 0)
			return (TRUE);

	return (FALSE);
}

static oc_print_avs (ps,avs,format)  /* need to use this somehow !!! */
PS ps;
AV_Sequence avs;
int format;
{
AV_Sequence newavs;
char found;
char printed = FALSE;

	if (avs == NULLAV)
		return;

	if (format != READOUT)
		DLOG (log_dsap,LLOG_EXCEPTIONS,("invalid call to oc_print"));

	for ( ; avs->avseq_next != NULLAV ; avs=avs->avseq_next) {
		found = FALSE;
		for (newavs = avs->avseq_next; newavs != NULLAV; newavs=newavs->avseq_next)
			if (in_hierarchy(avs,newavs) == TRUE) {
				found = TRUE;
				break;
			}

		if (found == FALSE) {
			if (printed == TRUE) 
				ps_print (ps," & ");
			AttrV_print (ps,&avs->avseq_av,format);
			printed = TRUE;
		}
	}

	if (printed == TRUE)
		ps_print (ps," & ");
	AttrV_print (ps,&avs->avseq_av,format);
}

objectclass_cmp (a,b)
objectclass *a, *b;
{
	/* macro ! */

	return objclass_cmp(a,b);
}

static objectclass * oc_cpy (oc)
objectclass * oc;
{
	return (oc);	/* static table !!! */
}

check_in_oc (oid,avs)
OID oid;
AV_Sequence avs;
{
objectclass * oc;

	for (; avs != NULLAV; avs = avs->avseq_next) {
		oc = (objectclass *) avs->avseq_av.av_struct;
		if (oc == NULLOBJECTCLASS)
			continue;
		if (oid_cmp(oid,oc->oc_ot.ot_oid) == 0)
			return (TRUE);
	}

	return (FALSE);
}

/* ARGSUSED */
static oc_free (oc)
objectclass * oc;
{
	;	/* static table !!! */
}

static PE oc_enc (oc)
objectclass *oc;
{
	return (oid2prim(oc->oc_ot.ot_oid));
}


static objectclass * oc_dec (pe)
PE pe;
{
OID oid;
objectclass *oc;

        if (! test_prim_pe (pe,PE_CLASS_UNIV,PE_PRIM_OID))
		return NULLOBJECTCLASS;

	if ((oid = prim2oid (pe)) == NULLOID)
		return NULLOBJECTCLASS;

	if ((oc = oid2oc (oid)) != NULLOBJECTCLASS)
		return (oc);

	return (oc_add(oid));
}



oc_print (ps,oc,format)
PS ps;
objectclass * oc;
int format;
{
extern int oidformat;

	if ( format != READOUT)
		ps_printf (ps,"%s",oc2name (oc,OIDPART));
	else
		ps_printf (ps,"%s",oc2name (oc,oidformat));
}

objectclass_syntax ()
{

	oc_sntx = add_attribute_syntax ("objectclass",
		(IFP) oc_enc,	(IFP) oc_dec,
		(IFP) str2oc,	oc_print,
		(IFP) oc_cpy,	objectclass_cmp,
		oc_free,	NULLCP,
		NULLIFP,	FALSE );

	oc_hier = (IFP) str2oc_hier;
	oc_avsprint = (IFP) oc_print_avs;
	want_oc_hierarchy ();

}
