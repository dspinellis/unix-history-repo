
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/symtab.c,v 7.2 91/02/22 09:32:26 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/symtab.c,v 7.2 91/02/22 09:32:26 mrose Interim $
 */

/*
 * $Log:	symtab.c,v $
 * Revision 7.2  91/02/22  09:32:26  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:49:58  mrose
 * sync
 * 
 * Revision 1.8  90/09/14  14:06:46  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.7  90/05/23  11:41:11  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.6  90/04/26  10:36:33  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/25  15:07:46  emsrdsm
 * i) lint'ed
 * 
 * Revision 1.4  90/04/20  17:58:02  emsrdsm
 * i) no more freeing
 * 
 * Revision 1.3  90/04/18  18:28:08  emsrdsm
 * fixed i) does not move to leaves
 *       ii) added default mechanism described using "typeDefaults" file.
 * 	 iii) added 'sorting' to attribute display
 * 
 * Revision 1.2  90/03/15  16:32:12  emsrdsm
 * fixes i) Prints messages correctly on exit.
 *      ii) Added rfc822 to greybook mailbox conversion
 *     iii) Removed bug that caused crash if 'local_dit' undefined
 * 
 * Revision 1.1  90/03/09  17:40:38  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  13:37:47  emsrdsm
 * Initial revision
 * 
 */

#include <string.h>
#include <malloc.h>

#include "symtab.h"

put_symbol_value(table, name, val)
table_entry table;
char *name;
char *val;
{
	if (!name) return;

  	while(table && strcmp(name, table->name)) {
		table = table->next;
	}
	if (table) {
		free(table->val);
		if (val) {
		  	table->val = 
			  (char *) malloc((unsigned) strlen(val) + 1);
			(void) strcpy(table->val, val);
		} else
		  	table->val = (char *) 0;
	} else {
		table = (table_entry) malloc(sizeof(table_entry));
		table->next = NULLSYM;
		table->name = (char *) malloc((unsigned) strlen(name) + 1);
		(void) strcpy(table->name, name);
                if (val) {
                        table->val = 
			  (char *) malloc((unsigned) strlen(val) + 1);
                        (void) strcpy(table->val, val);
	        } else
                        table->val = 0;
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

