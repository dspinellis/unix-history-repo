/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/symtab.h,v 7.2 91/02/22 09:32:27 mrose Interim $
 */

/*
 * $Log:	symtab.h,v $
 * Revision 7.2  91/02/22  09:32:27  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:49:59  mrose
 * sync
 * 
 * Revision 1.8  90/09/14  14:07:06  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.7  90/05/23  11:41:35  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.6  90/04/26  10:36:51  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/25  15:08:03  emsrdsm
 * i) lint'ed
 * 
 * Revision 1.4  90/04/20  17:58:14  emsrdsm
 * i) no more freeing
 * 
 * Revision 1.3  90/04/18  18:28:28  emsrdsm
 * fixed i) does not move to leaves
 *       ii) added default mechanism described using "typeDefaults" file.
 * 	 iii) added 'sorting' to attribute display
 * 
 * Revision 1.2  90/03/15  16:33:29  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.1  90/03/09  17:41:09  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  13:39:03  emsrdsm
 * Initial revision
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
