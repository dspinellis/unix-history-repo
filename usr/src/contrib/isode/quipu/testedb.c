#include "quipu/util.h"
#include "quipu/entry.h"
#include "psap.h"

LLog * log_dsap;
#ifndef	NO_STATS
LLog * log_stat;
#endif

main ()
{
extern IFP unrav_fn;
extern IFP schema_fn;
int real_unravel_attribute ();
int real_check_schema ();
extern PS opt;
extern char dsa_mode;

	dsa_mode = TRUE;

	unrav_fn = (IFP) real_unravel_attribute;
	schema_fn = (IFP) real_check_schema;

	quipu_syntaxes();
	load_oid_table ("oidtable");
	check_dsa_known_oids ();

	(void) ll_close (log_dsap);
	ll_dbinit (log_dsap, "testedb");
	log_dsap -> ll_events = LLOG_FATAL | LLOG_EXCEPTIONS;

	if (getentry_block (NULLENTRY,"./EDB") != NULL)
		(void) printf ("EDB ok\n"), exit (0);

	exit (1);
}

/* stubs for unused external synbols */

int refreshing = FALSE;

shadow_entry()
{;}


