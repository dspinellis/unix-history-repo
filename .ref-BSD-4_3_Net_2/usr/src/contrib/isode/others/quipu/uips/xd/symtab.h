/* $Header: /f/osi/others/quipu/uips/xd/RCS/symtab.h,v 7.1 91/02/22 09:33:09 mrose Interim $ */
/*
 $Log:	symtab.h,v $
 * Revision 7.1  91/02/22  09:33:09  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/12  13:10:57  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:49  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:19  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:21  emsrssn
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
 * Revision 1.1  90/03/08  13:18:49  emsrssn
 * Initial revision
 * 
 * 
*/

#ifndef SYMTAB
#define SYMTAB


typedef struct tab_entry {
  	char *val;
	char *name;
	struct tab_entry *next;
      } *table_entry;

#define NULLSYM (table_entry) 0

#endif
