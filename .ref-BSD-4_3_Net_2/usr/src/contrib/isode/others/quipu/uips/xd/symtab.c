/* $Header: /f/osi/others/quipu/uips/xd/RCS/symtab.c,v 7.2 91/02/22 09:33:08 mrose Interim $ */
#ifndef lint
	static char *rcsid = "$Id: symtab.c,v 7.2 91/02/22 09:33:08 mrose Interim $";
#endif
/*
 $Log:	symtab.c,v $
 * Revision 7.2  91/02/22  09:33:08  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:50:36  mrose
 * sync
 * 
 * Revision 7.0  90/06/12  13:10:53  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:48  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:18  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:19  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:36  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:42  emsrssn
 * Initial revision
 * 
 * 
*/

#include <malloc.h>
#include "quipu/util.h"
#include "symtab.h"

put_symbol_value(table, name, val)
table_entry table;
char *name, *val;
{
	if (!name) return;

  	while(table && strcmp(name, table->name)) {
		table = table->next;
	}
	if (table) {
		free(table->val);
		if (val) {
		  	table->val = (char *) malloc((unsigned int) (strlen(val) + 1) );
			(void) strcpy((char *) table->val, val);
		} else
		  	table->val = (char *) 0;
	} else {
		table = (table_entry ) malloc((unsigned int) (sizeof(table_entry)) );
		table->next = NULLSYM;
		table->name = (char *) malloc((unsigned int) (strlen(name) + 1) );
		(void) strcpy((char *) table->name, name);
                if (val) {
                        table->val = (char *) malloc((unsigned int) (strlen(val) + 1) );
                        (void) strcpy((char *) table->val, val);
	        } else
                        table->val = (char *) 0;
	}
}
		
char *
get_symbol_value(table, name)
table_entry table;
char *name;
{
  	while(table && strcmp(name, table->name)) table = table->next;
  	if (table)
	  	return table->val;
	return (char *) 0;
}
	  	

free_table(table)
table_entry table;
{
  	table_entry  entry;

  	while(table) {
	  	if (table->val)
		  	free(table->val);
		free(table->name);
		entry = table;
		table = table->next;
		free((char *) entry);
	}
}

