
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/xd/RCS/filt.c,v 7.2 91/02/22 09:32:47 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/xd/RCS/filt.c,v 7.2 91/02/22 09:32:47 mrose Interim $
 */

/*
 * $Log:	filt.c,v $
 * Revision 7.2  91/02/22  09:32:47  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/27  08:45:57  mrose
 * update
 * 
 * Revision 7.0  90/06/12  13:10:49  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:40  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:10  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:11  emsrssn
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
 * Revision 1.2  90/03/16  11:29:41  emsrdsm
 * *** empty log message ***
 * 
 * Revision 1.1  90/03/09  16:48:58  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  12:10:17  emsrdsm
 * Initial revision
 * 
 * Revision 1.1  90/03/09  11:36:42  emsrdsm
 * Initial revision
 * 
 */

#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include "filt.h"
#include "y.tab.h"
#include "symtab.h"

extern unsigned int curr_filt;
extern unsigned int filt_num;
extern unsigned int typeindx;
extern filt_struct *filt_arr[];
extern char *filtvalue[];
extern char *filttype[];
extern char *default_arr[];

extern char svalue[], mvalue[];

make_type(name_val, filt)
char * name_val;
filt_struct * filt;
{
  filttype[curr_filt] = (char *) malloc((unsigned int) (strlen(name_val) + 1));
  (void) strcpy(filttype[curr_filt], name_val);
  
/*  if (default_val) {
    default_arr[curr_filt] = 
      (char *) malloc((unsigned int) (strlen(default_val)+1));
    (void) strcpy(default_arr[curr_filt], default_val);
  } else {
    default_arr[curr_filt] = (char *) malloc((unsigned int) 2);
    *default_arr[curr_filt] = '\0';
  }*/
  filt_arr[curr_filt] = filt;
}

filt_struct *
make_item_filter(oid, match, value)
char *oid;
int match;
char *value;
{
  register filt_struct * filt = (filt_struct *) malloc(sizeof(filt_struct));
  
  filt->flt_type = ITEM;
  filt->next = 0;
  
  filt->fu_cont.item.fi_type = match;
  filt->fu_cont.item.stroid = 
    (char *) malloc((unsigned int) (strlen(oid) + 1));
  (void) strcpy(filt->fu_cont.item.stroid, oid);
  
  if (*value == '*') filt->fu_cont.item.name = (char *) 0;
  else {
    filt->fu_cont.item.name = 
      (char *) malloc((unsigned int) (strlen(value) + 1));
    (void) strcpy(filt->fu_cont.item.name, value);
  }
  return filt;
}

filt_struct *
link_filters(filt1, filt2)
filt_struct *filt1;
filt_struct *filt2;
{
  filt1->next = filt2;
  return filt1;
}

filt_struct *
make_parent_filter(filt_type, filt1, filt2, filt3)
int filt_type;
filt_struct * filt1;
filt_struct * filt2;
filt_struct * filt3;
{
  filt_struct * parent = (filt_struct *) malloc(sizeof(filt_struct));

  switch (filt_type) {

  case NOT:
    parent->flt_type = NOT;
    parent->fu_cont.sub_filt = filt1;
    parent->next = 0;
    break;

  case AND:
    parent->flt_type = AND;
    parent->fu_cont.sub_filt = filt1;
    filt1->next = filt2;
    filt2->next = filt3;
    parent->next = 0;
    break;

  default:
    parent->flt_type = OR;
    parent->fu_cont.sub_filt = filt1;
    filt1->next = filt2;
    filt2->next = filt3;
    parent->next = 0;
    break;
  }

  return parent;
}

free_filt(filt)
filt_struct *filt;
{
  if (filt) {
    free_filt(filt->next);

    if (filt->flt_type = ITEM) {
      free(filt->fu_cont.item.stroid);
      if (filt->fu_cont.item.name) free(filt->fu_cont.item.name);
    } else
      free_filt(filt->fu_cont.sub_filt);

    free((char *) filt);
  } else
    return;
}

Filter
make_filter(filt)
filt_struct *filt;
{
  int type;
  Filter rfilt, sfilt = filter_alloc();
  
  if (!filt)
    return 0;
  
  switch(filt->flt_type) {

  case ITEM:
    sfilt->flt_type = FILTER_ITEM;
    sfilt->flt_next = make_filter(filt->next);
    
    (void) strcpy(svalue, (filt->fu_cont.item.name? 
			   filt->fu_cont.item.name:
			   mvalue));
    
    type = filt->fu_cont.item.fi_type;

    switch(type) {
    case APPROX:
    case EQUAL:
      sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type = 
	AttrT_new(filt->fu_cont.item.stroid);

      if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value =
	   str2AttrV(svalue,
		     sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
		     oa_syntax)) == NULL) {
	
	rfilt = sfilt->flt_next;
	sfilt->flt_next = NULLFILTER;
	filter_free(sfilt);
	return rfilt;
      }

      if (type == EQUAL)
	sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_EQUALITY;
      else
	sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_APPROX;

      break;

    case SUBSTRING:
      sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_SUBSTRINGS;
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_type =
	AttrT_new(filt->fu_cont.item.stroid);
      
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_initial =
	NULLAV;
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_final = 
	NULLAV;
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any =
	avs_comp_new(str2AttrV(svalue,
			       sfilt->flt_un.flt_un_item.fi_un.
			       fi_un_substrings.fi_sub_type->
			       oa_syntax));
      break;
      
    default:
      sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_APPROX;
      break;
    }
    return sfilt;

    
  case AND:
    sfilt->flt_type = FILTER_AND;
    sfilt->flt_un.flt_un_filter = make_filter(filt->fu_cont.sub_filt);
    sfilt->flt_next = make_filter(filt->next);
    return sfilt;

    
  case OR:
    sfilt->flt_type = FILTER_OR;
    sfilt->flt_un.flt_un_filter = make_filter(filt->fu_cont.sub_filt);
    sfilt->flt_next = make_filter(filt->next);
    return sfilt;


  case NOT:
    sfilt->flt_type = FILTER_NOT;
    sfilt->flt_next = make_filter(filt->next);
    sfilt->flt_un.flt_un_filter = make_filter(filt->fu_cont.sub_filt);
    return sfilt;


  default:
    return NULLFILTER;
  }
}
