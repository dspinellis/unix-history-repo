
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/pod/RCS/filt.c,v 7.2 91/02/22 09:31:31 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/filt.c,v 7.2 91/02/22 09:31:31 mrose Interim $
 */

#include <string.h>
#include <malloc.h>
#include <ctype.h>

#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"

#include "filt.h"
#include "y.tab.h"
#include "defs.h"

extern unsigned int curr_filt;
extern unsigned int filt_num;
extern unsigned int typeindx;
extern filt_struct *filt_arr[];
extern char *filtvalue[];
extern char *filttype[];
extern char *default_arr[];

extern char mvalue[];

make_type(name_val, filt)
     char * name_val;
     filt_struct * filt;
{
  filttype[curr_filt] = (char *) malloc((unsigned int) (strlen(name_val) + 1));
  (void) strcpy(filttype[curr_filt], name_val);
  
  filt_arr[curr_filt] = filt;
}

filt_struct *make_item_filter(oid, match, value)
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

filt_struct *link_filters(filt1, filt2)
     filt_struct *filt1, *filt2;
{
  filt1->next = filt2;
  return filt1;
}

filt_struct *make_parent_filter(filt_type, filt1, filt2, filt3)
     int filt_type;
     filt_struct *filt1, *filt2, *filt3;
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

Filter make_attr_filter()
{
  int match_type;
  char attr_name[STRINGLEN], 
       attr_val[STRINGLEN], sub_val_initial[STRINGLEN], 
       sub_val_final[STRINGLEN], sub_val_any[STRINGLEN];
  register char *end, *start, *next;
  char save;
  Filter sfilt = filter_alloc();

  sfilt->flt_type = FILTER_ITEM;
  sfilt->flt_next = NULLFILTER;
  sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_EQUALITY;
  sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value = NULLAttrV;
  sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type = NULLAttrT;

  if (end = index(mvalue, '~')) match_type = APPROX;
  else if (end = index(mvalue, '*')) match_type = SUBSTRING;
  else match_type = EQUAL;

  start = mvalue;
  while (isspace(*start) && *start != '\0') start++;
  end = start;
  while (!isspace(*end) && *end != '=' && *end != '~' 
	 && *end != '*' && *end != '\0') end++;
  save = *end;
  *end = '\0';
  (void) strcpy(attr_name, start);
  *end = save;

  if (attr_name [0] == '\0') {
    (void) fprintf (stderr,
	     "Error: Cannot search, invalid syntax on filter '%s'.\n",
	     mvalue);
    filter_free(sfilt);
    return NULLFILTER;
  }

  start = end + 1;
  if (match_type != SUBSTRING) {
    while (!isalnum(*start) && *start != '\0') start++;
    
    end = start;
    
    while (*end != '\0') end++;
    while (!isalnum(*end) && end > start) end--;
    if (*end != '\0') end++;
    
    save = *end;
    *end = '\0';
    (void) strcpy(attr_val, start);
    *end = save;

    if (attr_val[0] == '\0') {
      (void) fprintf (stderr,
	       "Error: Cannot search, invalid syntax on filter '%s'.\n",
	       mvalue);
      filter_free(sfilt);
      return NULLFILTER;
    }
  } else {
    while (!isalnum(*start) && *start != '*' && *start != '\0') start++;
    
    if (*start == '\0') {
      (void) fprintf (stderr,
	       "Error: Cannot search, invalid syntax on filter '%s'.\n",
	       mvalue);
      filter_free(sfilt);
      return NULLFILTER;
    }

    if (*start == '*') {
      sub_val_initial[0] = '*';
      ++start;
      while (isspace(*start) && *start != '\0') start++;
      
      if (*start == '\0' || !isalnum(*start)) {
	(void) fprintf (stderr,
		 "Error: Cannot search, invalid syntax on filter '%s'.\n",
		 mvalue);
	filter_free(sfilt);
	return NULLFILTER;
      }
      
      end = start;
      while (isalnum(*end) && *end != '\0') end++;
      
      if (*end == '\0') {
	(void) strcpy (sub_val_final, start);
	sub_val_any[0] = '*';
      } else {
	next = end;
	
	while (*next != '*' && *next != '\0') next++;
	
	if (*next == '*') {
	  sub_val_final[0] = '*';
	  
	  save = *end;
	  *end = '\0';
	  (void) strcpy(sub_val_any, start);
	  *end = save;
	} else {
	  sub_val_any[0] = '*';
	  
	  save = *end;
	  *end = '\0';
	  (void) strcpy(sub_val_final, start);
	  *end = save;
	}
      }
    } else if (isalnum(*start)) {
      end = start;
      while (!isspace(*end) && *end != '\0') end++;
      
      if (*end == '\0') {
	(void) fprintf (stderr,
		 "Error: Cannot search, invalid syntax on filter '%s'.\n",
		 mvalue);
	filter_free(sfilt);
	return NULLFILTER;
      }
      
      save = *end;
      *end = '\0';
      
      if (index ((char *) (end + 1), '*') == NULLCP) {
	(void) fprintf (stderr,
		 "Error: Cannot search, invalid syntax on filter '%s'.\n",
		 mvalue);
	filter_free(sfilt);
	return NULLFILTER;
      } else {
	(void) strcpy(sub_val_initial, start);
	sub_val_any[0] = sub_val_final[0] = '*';
      }
    }
  }

  switch (match_type) {
  case APPROX:
  case EQUAL:
    sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type = AttrT_new(attr_name);

    if (match_type == EQUAL)
      sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_EQUALITY;
    else
      sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_APPROX;

    if (!sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type ||
	sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->oa_syntax
	== 0) {
      (void) fprintf (stderr,
	       "Error: Cannot search, invalid attribute type '%s'.\n",
	       attr_name);
      sfilt->flt_next = NULLFILTER;
      filter_free(sfilt);
      return NULLFILTER;
    }

    if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value =
	 str2AttrV(attr_val, 
		   sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type
		   ->oa_syntax)) == NULL) {
      (void) fprintf (stderr,
	 "Error: Cannot search, invalid value '%s' for attribute type '%s'.\n",
	   attr_val, attr_name);
      filter_free(sfilt);
      return NULLFILTER;
    }
    return sfilt;

  case SUBSTRING:
    sfilt->flt_un.flt_un_item.fi_type = FILTERITEM_SUBSTRINGS;

    sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_initial = NULLAV;
    sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_final = NULLAV;
    sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any = NULLAV;

    sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_type =
      AttrT_new(attr_name);

    if (!sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type ||
	sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->oa_syntax
	== 0) {
      (void) fprintf (stderr,
	       "Error: Cannot search, invalid attribute type '%s'.\n", 
	       attr_name);
      filter_free(sfilt);
      return NULLFILTER;
    }
    
    if (sub_val_initial[0] != '*')
      if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_initial =
	   avs_comp_new(str2AttrV(sub_val_initial, 
				  sfilt->flt_un.flt_un_item.fi_un.
				  fi_un_substrings.fi_sub_type
				  ->oa_syntax)))
	  == NULLAV) {
	(void) fprintf (stderr,
	 "Error: Cannot search, invalid value '%s' for attribute type '%s'.\n",
		 sub_val_initial, attr_name);
	filter_free(sfilt);
	return NULLFILTER;
      }

    if (sub_val_any[0] != '*')
      if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any =
	   avs_comp_new(str2AttrV(sub_val_any, 
				  sfilt->flt_un.flt_un_item.fi_un.
				  fi_un_substrings.fi_sub_type->
				  oa_syntax)))
	  == NULLAV) {
	(void) fprintf (stderr,
	 "Error: Cannot search, invalid value '%s' for attribute type '%s'.\n",
		 sub_val_any, attr_name);
	filter_free(sfilt);
	return NULLFILTER;
      }

    if (sub_val_final[0] != '*')
      if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_final =
	   avs_comp_new(str2AttrV(sub_val_final, 
				  sfilt->flt_un.flt_un_item.fi_un.
				  fi_un_substrings.fi_sub_type->
				  oa_syntax)))
	  == NULLAV) {
	(void) fprintf (stderr,
	 "Error: Cannot search, invalid value '%s' for attribute type '%s'.\n",
		 sub_val_final, attr_name);
	filter_free(sfilt);
	return NULLFILTER;
      }
    return sfilt;
  default:
    return NULLFILTER;
  }
}

Filter make_filter(filt)
     filt_struct *filt;
{
  int type;
  char svalue[STRINGLEN];
  Filter rfilt, sfilt;
  
  if (!filt) return NULLFILTER;

  if (index(mvalue, '=')) return make_attr_filter();

  sfilt = filter_alloc();

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

      if (!sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type ||
	  sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
	  oa_syntax == 0) {
        rfilt = sfilt->flt_next;
	sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value = NULLAttrV;
        sfilt->flt_next = NULLFILTER;
        filter_free(sfilt);
        return rfilt;
      }

      if ((sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value =
	   str2AttrV(svalue,
		     sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
		     oa_syntax)) == NULLAttrV) {
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
      
      if (!sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type ||
	  sfilt->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
	  oa_syntax == 0) {
        rfilt = sfilt->flt_next; 
	sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_initial =
	  NULLAV;
	sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_final = 
	  NULLAV;
	sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any =
	  NULLAV;
	sfilt->flt_next = NULLFILTER;
        filter_free(sfilt);
        return rfilt;
      }

      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_initial =
	NULLAV;
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_final = 
	NULLAV;
      sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any =
	avs_comp_new(str2AttrV(svalue,
			       sfilt->flt_un.flt_un_item.fi_un.
			       fi_un_substrings.fi_sub_type->
			       oa_syntax));
      if (sfilt->flt_un.flt_un_item.fi_un.fi_un_substrings.fi_sub_any ==
	  NULLAV) {
	rfilt = sfilt->flt_next;
        sfilt->flt_next = NULLFILTER;
        filter_free(sfilt);
        return rfilt;
      }

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














