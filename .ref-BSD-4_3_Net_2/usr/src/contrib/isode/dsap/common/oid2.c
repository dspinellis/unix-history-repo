#include "quipu/util.h"
#include "quipu/entry.h"
#include "cmd_srch.h"
#include "tailor.h"

extern char chrcnv [];

extern LLog * log_dsap;
int load_obj_hier();
int add_oc_macro();

extern oid_table OIDTable[];
extern objectclass ocOIDTable[];
extern oid_table_attr attrOIDTable[];
extern int NumEntries;
extern int attrNumEntries;
extern int ocNumEntries;
static table_seq table_seq_new ();

struct mac_buf {                        /* for handling macros */
	char name [BUFSIZE];
	char value [LINESIZE];
} macro [BUFSIZE];

int NumMacro            = 0;

want_oc_hierarchy ()
{
extern IFP oc_load;
extern IFP oc_macro_add;

	oc_load = load_obj_hier;
	oc_macro_add = add_oc_macro;
}

load_obj_hier (sep,newname)
char * sep;
char * newname;
{
	if (sep == 0) {
		LLOG (log_dsap,LLOG_FATAL,("hierarchy missing %s",newname));
		return NOTOK;
	}
	if (get_oc_bits (sep) != OK) {
		LLOG (log_dsap,LLOG_FATAL,("(%s)",newname));
		return NOTOK;
	}

	return OK;
}

static struct oc_seq *oc_seq_merge (a,b)
struct oc_seq *a;
struct oc_seq *b;
{
register struct oc_seq  *aptr, *bptr, *result, *trail;

	if ( a == NULLOCSEQ )
		return (b);
	if ( b == NULLOCSEQ )
		return (a);

	/* start sequence off, make sure 'a' is the first */
	switch (objclass_cmp (a,b)) {
		case 0: /* equal */
			result = a;
			free ((char *) b);
			aptr = a->os_next;
			bptr = b->os_next;
			break;
		case -1:
			result = b;
			aptr = a;
			bptr = b->os_next;
			break;
		case 1:
			result = a;
			aptr = a->os_next;
			bptr = b;
			break;
		}

	trail = result;
	while (  (aptr != NULLOCSEQ) && (bptr != NULLOCSEQ) ) {

	   switch (objclass_cmp (aptr,bptr)) {
		case 0: /* equal */
			trail->os_next = aptr;
			trail = aptr;
			free ((char *) bptr);
			aptr = aptr->os_next;
			bptr = bptr->os_next;
			break;
		case -1:
			trail->os_next = bptr;
			trail = bptr;
			bptr = bptr->os_next;
			break;
		case 1:
			trail->os_next = aptr;
			trail = aptr;
			aptr = aptr->os_next;
			break;
	    }
	}
	if (aptr == NULLOCSEQ)
		trail->os_next = bptr;
	else
		trail->os_next = aptr;

	return (result);
}


static get_oc_bits (str)
register char * str;
{
register char * ptr;
register char * ptr2;
struct oc_seq * oidseq = NULLOCSEQ, *oidseqptr = oidseq;
objectclass * oc;

	if ((ptr = index (str,SEPERATOR)) == 0) {
		LLOG (log_dsap,LLOG_FATAL,("must missing"));
		return (NOTOK);
	}
	*ptr++ = 0;

	while (( ptr2 = index (str,COMMA)) != 0) {
		*ptr2++ = 0;
		oidseqptr = (struct oc_seq *) smalloc (sizeof (struct oc_seq));
		if ((oc = name2oc(str)) == NULLOBJECTCLASS) {
			LLOG (log_dsap,LLOG_FATAL,("unknown objectclass in hierachy %s",str));
			return (NOTOK);
		}
		oidseqptr->os_oc = oc;
		oidseqptr->os_next = NULLOCSEQ;
		oidseq = oc_seq_merge (oidseq,oidseqptr);
		str = ptr2;
	}
	if (*str != 0) {
		oidseqptr = (struct oc_seq *) smalloc (sizeof (struct oc_seq));
					/* no logging -> never freed */
		if ((oc = name2oc(str)) == NULLOBJECTCLASS) {
			LLOG (log_dsap,LLOG_FATAL,("unknown objectclass in hierachy %s",str));
			return (NOTOK);
		}
		oidseqptr->os_oc = oc;
		oidseqptr->os_next = NULLOCSEQ;
		oidseq = oc_seq_merge (oidseq,oidseqptr);
		ocOIDTable[ocNumEntries].oc_hierachy = oidseq;
	} else
		ocOIDTable[ocNumEntries].oc_hierachy = NULLOCSEQ;

	str = ptr;
	if ((ptr = index (str,SEPERATOR)) == 0) {
		LLOG (log_dsap,LLOG_FATAL,("may element missing"));
		return (NOTOK);
	}
	*ptr++ = 0;

	ocOIDTable[ocNumEntries].oc_may  = table_seq_new (ptr);
	ocOIDTable[ocNumEntries].oc_must = table_seq_new (str);

	return (OK);
}

static table_seq undo_macro (top,ptr)
table_seq top;
register char * ptr;
{
register int i;
table_seq tab;
table_seq tab_top;
table_seq trail = NULLTABLE_SEQ;

	if (*ptr == 0)
		return (top);

	for (i=0; i<NumMacro; i++)
		if (lexequ (macro[i].name,ptr) == 0) {
			tab_top= table_seq_new (macro[i].value);
			for (tab=tab_top; tab!=NULLTABLE_SEQ; tab=tab->ts_next)
				trail = tab;
			if (trail != NULLTABLE_SEQ) {
				trail->ts_next = top;
				return (tab_top);
			} else
				return (top);
		}

	LLOG (log_dsap,LLOG_FATAL,("can't interpret %s in must/may field",ptr));
	return (top);
}

static table_seq table_seq_new (str)
register char * str;
{
register char * ptr;
table_seq tptr;
table_seq top = NULLTABLE_SEQ;
oid_table_attr * at;

	if (*str == 0)
		return (NULLTABLE_SEQ);

	while ((ptr = index (str,COMMA)) != 0) {
		*ptr = 0;
		if ((at = name2attr (str)) == NULLTABLE_ATTR)
			top = undo_macro (top,str);
		else {
			tptr = (table_seq) smalloc (sizeof (*tptr));
					/* no logging -> never freed */
			tptr->ts_oa = at;
			tptr->ts_next = top;
			top = tptr;
		}
		*ptr = COMMA;
		str = ptr + 1;
	}

	if (str != 0) {
		if ((at = name2attr (str)) == NULLTABLE_ATTR)
			return (undo_macro (top,str));
		else {
			tptr = (table_seq) smalloc (sizeof (*tptr));
					/* no logging -> never freed */
			tptr->ts_oa = at;
			tptr->ts_next = top;
			return (tptr);
		}
	} else
		return (NULLTABLE_SEQ);

}



void	dumpalloid ()
{
register int i;
register objectclass      * oc = &ocOIDTable[0];
register oid_table_attr   * at = &attrOIDTable[0];
register oid_table        * oi = &OIDTable[0];
 
	for (i=0;i<ocNumEntries;i++,oc++)
		(void) printf("\"%s\"\t\t%s\n", oc->oc_ot.ot_name, oc->oc_ot.ot_stroid);
 
	for (i=0;i<attrNumEntries;i++,at++)
		(void) printf("\"%s\"\t\t%s\n", at->oa_ot.ot_name, at->oa_ot.ot_stroid);
 
	for (i=0;i<NumEntries;i++,oi++)
		(void) printf("\"%s\"\t\t%s\n", oi->ot_name, oi->ot_stroid);
  
  }


add_oc_macro (buf,ptr)
char * buf, *ptr;
{
	(void) strcpy(macro[NumMacro].name,buf);
	(void) strcpy(macro[NumMacro++].value,ptr);
}
