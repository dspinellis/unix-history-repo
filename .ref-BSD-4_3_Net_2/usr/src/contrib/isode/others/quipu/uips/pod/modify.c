#include <sys/types.h>
#include <malloc.h>
#include <string.h>
#include <ctype.h>

#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include "quipu/modify.h"
#include "quipu/name.h"

#include "sequence.h"
#include "dir_entry.h"
#include "defs.h"
#include "util.h"

#define AS_SYNTAX(attrSeq) attrSeq->attr_type->oa_syntax
#define AS_STROID(attrSeq) attrSeq->attr_type->oa_ot.ot_stroid
#define FOREACH(a) for (eptr = a; eptr != NULLATTR; eptr=eptr->attr_link)

extern bool photo_on;

char *modify_error();
Attr_Sequence as_sort();
void char_map(), char_unmap();

static Attr_Sequence eptr;

#ifndef NO_STATS
extern LLog    *log_stat;
#endif

struct entrymod * ems_append (a,b)
     struct entrymod *a;
     struct entrymod *b;
{
  struct entrymod *ptr;

  if ((ptr = a) == NULLMOD)
    return b;
  
  for ( ; ptr->em_next != NULLMOD; ptr = ptr->em_next)
    ;
  
  ptr->em_next = b;
  return a;
}


dsErrorStruct modify_entry(mods)
     dirEntry mods;
{
  struct ds_modifyentry_arg mod_arg;
  dsErrorStruct mod_error;
  struct DSError error;
  struct entrymod *curr_mod = 0, *entrymods = 0;
  AttributeType attr_type = 0;
  AttributeValue attr_val = 0;
  AV_Sequence attrVal_seq = 0;
  register modVals curr_val;
  dirAttrs attrs;
  char err_buf[STRINGLEN], attr_val_buf[STRINGLEN];
  short attr_removed;
  
  if (get_default_service(&mod_arg.mea_common) != 0) {
    mod_error.error = serviceerror;
    mod_error.err_mess = strdup("Directory Service Error!\n");
  }

  mod_arg.mea_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
  mod_arg.mea_object = str2dn(mods->entry_name);

  for (attrs = mods->attrs; attrs; attrs = attrs->next) {
    if (attrs->mod_flag) {
      attr_type = str2AttrT(attrs->attr_name);

      for (curr_val = attrs->val_seq; 
	   curr_val && curr_val->mod_flag;
	   curr_val = curr_val->next)
	;

      if (!curr_val) {
	if (attrs->in_flag) {
	  curr_mod = em_alloc();
	  curr_mod->em_next = NULLMOD;
	  curr_mod->em_type = EM_REMOVEATTRIBUTE;
	  curr_mod->em_what = NULLATTR;
	  
	  curr_mod->em_what = as_comp_new(attr_type, NULLAV, NULLACL_INFO);
	  entrymods = ems_append(entrymods, curr_mod);
	}

	curr_mod = em_alloc();
	curr_mod->em_next = NULLMOD;
	curr_mod->em_type = EM_ADDATTRIBUTE;
	curr_mod->em_what = NULLATTR;
	
	for (curr_val = attrs->val_seq; curr_val; curr_val = curr_val->next) {
	  char_unmap(attr_val_buf, curr_val->new_value);
	  if (attr_val_buf[0] != '\0') {
	    attr_val = str2AttrV(attr_val_buf, attr_type->oa_syntax);
	    if (attr_val) {
	      if (attrVal_seq) 
		attrVal_seq = avs_merge(attrVal_seq, avs_comp_new(attr_val));
	      else 
		attrVal_seq = avs_comp_new(attr_val);
	    } else {
	      if (entrymods) ems_free(entrymods);
	      AttrT_free(attr_type);
	      mod_error.error = attributerror;
	      (void) sprintf(err_buf,
		     "Attribute Error!\nInvalid syntax for value %s type %s.", 
		      curr_val->new_value, attrs->attr_name);
	      mod_error.err_mess = strdup(err_buf);
	      return mod_error;
	    }
	  }
	}

	if (attrVal_seq) {
	  curr_mod->em_what = as_comp_new(AttrT_cpy(attr_type),
					  attrVal_seq, 
					  NULLACL_INFO);
	  entrymods = ems_append(entrymods, curr_mod);
	  attrVal_seq = 0;
	  curr_mod = 0;
	  AttrT_free(attr_type);
	}
      } else {
	if (attrs->in_flag) {
	  for (curr_val = attrs->val_seq;
	       curr_val;
	       curr_val = curr_val->next) {
	    char_unmap(attr_val_buf, curr_val->value);
	    if (curr_val->mod_flag && attr_val_buf[0] != '\0') {
	      attrVal_seq = avs_comp_new(str2AttrV(attr_val_buf,
						   attr_type->
						   oa_syntax));
	      curr_mod = em_alloc();
	      curr_mod->em_next = NULLMOD;
	      curr_mod->em_type = EM_REMOVEVALUES;
	      curr_mod->em_what = as_comp_new(AttrT_cpy(attr_type),
					      attrVal_seq,
					      NULLACL_INFO);
	      entrymods = ems_append(entrymods, curr_mod);
	    }
	  }
	  
	  for (curr_val = attrs->val_seq;
	       curr_val;
	       curr_val = curr_val->next) {
	    char_unmap(attr_val_buf, curr_val->new_value);
	    if (curr_val->mod_flag && attr_val_buf[0] != '\0') {
	      attr_val = str2AttrV(attr_val_buf, 
				   attr_type->oa_syntax);
	      if (attr_val) {
		if (attrVal_seq) 
		  attrVal_seq = avs_merge(attrVal_seq, 
					  avs_comp_new(attr_val));
		else 
		  attrVal_seq = avs_comp_new(attr_val);
	      } else {
		if (entrymods) ems_free(entrymods);
		AttrT_free(attr_type);
		mod_error.error = attributerror;
		(void) sprintf(err_buf,
		     "Attribute Error!\nInvalid syntax for value %s type %s.", 
		      curr_val->new_value, attrs->attr_name);
		mod_error.err_mess = strdup(err_buf);
		return mod_error;
	      }

	      curr_mod = em_alloc();
	      curr_mod->em_next = NULLMOD;
	      curr_mod->em_type = EM_ADDVALUES;
	      curr_mod->em_what = as_comp_new(AttrT_cpy(attr_type),
					      attrVal_seq,
					      NULLACL_INFO);
	      entrymods = ems_append(entrymods, curr_mod);
	    }
	  }
	} else {
	  curr_mod = em_alloc();
	  curr_mod->em_next = NULLMOD;
	  curr_mod->em_type = EM_ADDATTRIBUTE;
	  
	  for (curr_val = attrs->val_seq; 
	       curr_val; 
	       curr_val = curr_val->next) {
	    char_unmap(attr_val_buf, curr_val->new_value);
	    if (attr_val_buf[0] != '\0') {
	      attr_val = str2AttrV(attr_val_buf, 
				   attr_type->oa_syntax);
	      if (attr_val) {
		if (attrVal_seq) 
		  attrVal_seq = avs_merge(attrVal_seq, 
					  avs_comp_new(attr_val));
		else 
		  attrVal_seq = avs_comp_new(attr_val);
	      } else {
		if (entrymods) ems_free(entrymods);
		AttrT_free(attr_type);
		mod_error.error = attributerror;
		(void) sprintf(err_buf,
		     "Attribute Error!\nInvalid syntax for value %s type %s.", 
		      curr_val->new_value, attrs->attr_name);
		mod_error.err_mess = strdup(err_buf);
		return mod_error;
	      }
	    }
	  }

	  if (attrVal_seq) {
	    curr_mod->em_what = as_comp_new(AttrT_cpy(attr_type),
					    attrVal_seq, 
					    NULLACL_INFO);
	    entrymods = ems_append(entrymods, curr_mod);
	    attrVal_seq = 0;
	    curr_mod = 0;
	    AttrT_free(attr_type);
	  }
	}
      }
    }
  }

  if (entrymods) {
    mod_arg.mea_changes = entrymods;
    if (ds_modifyentry(&mod_arg, &error) != DS_OK) {
      mod_error.err_mess = modify_error(&error);

      switch (error.dse_type) {
      case DSE_LOCALERROR:
	mod_error.error = duaerror;
	if (!mod_error.err_mess) {
	  mod_error.err_mess = 
	    strdup("Internal Error, No modifications made. Sorry!");
	}
	break;
	
      case DSE_REMOTEERROR:
	mod_error.error = localdsaerror;
	break;
	
      case DSE_ATTRIBUTEERROR:
	mod_error.error = attributerror;
	if (!mod_error.err_mess) {
	  mod_error.err_mess = 
	    strdup("Attribute Error! No Modifications made.");
	}
	break;

      case DSE_REFERRAL:
      case DSE_DSAREFERRAL:
	mod_error.error = remotedsaerror;
	if (!mod_error.err_mess) {
	  mod_error.err_mess = 
	    strdup("Referral Error! No Modifications made.");
	}
	break;

      case DSE_SECURITYERROR:
	mod_error.error = security;
	if (!mod_error.err_mess) {
	  mod_error.err_mess = strdup("Security Error! Check access rights.");
	}
	break;

      case DSE_NAMEERROR:
	mod_error.error = namerror;
	if (!mod_error.err_mess) {
	  switch (error.dse_un.dse_un_name.DSE_na_problem) {
	  case DSE_NA_NOSUCHOBJECT:
	    mod_error.err_mess = 
	      strdup("Name Error! No such object.");
	    break;
	  case DSE_NA_ALIASPROBLEM:
	  case DSE_NA_ALIASDEREFERENCE:
	    mod_error.err_mess = strdup("Error! Alias problem.");
	    break;
	  case DSE_NA_INVALIDATTRIBUTESYNTAX:
	    mod_error.err_mess = 
	      strdup("Name Error! Invalid attribute syntax.");
	    break;
	  }
	}
	break;
	
      case DSE_SERVICEERROR:
	mod_error.error = serviceerror;
	break;
	
      case DSE_UPDATEERROR:
	mod_error.error = updaterror;
	break;
	
      default:
	mod_error.error = localdsaerror;
	break;
      }
      ds_error_free(&error);
      dn_free(mod_arg.mea_object);
      return mod_error;
    } else {
      ems_free(entrymods);
      delete_cache(mod_arg.mea_object);
    }
  } else {
    mod_error.error = updaterror;
    mod_error.err_mess = strdup("No modifications to make!");
    dn_free(mod_arg.mea_object);
    return mod_error;
  }

  for (attrs = mods->attrs; attrs; attrs = attrs->next) {
    if (attrs->mod_flag == TRUE && attrs->in_flag == FALSE) 
      attrs->in_flag = TRUE;
    attrs->mod_flag = FALSE;

    attr_removed = TRUE;
    for (curr_val = attrs->val_seq; curr_val; curr_val = curr_val->next) {
      if (curr_val->new_value != NULLCP) {
	if (curr_val->value != NULLCP) free(curr_val->value);
	curr_val->value = curr_val->new_value;
	curr_val->new_value = NULLCP;
	attr_removed = FALSE;
      }

      if (curr_val->value && *curr_val->value != '\0' && !curr_val->mod_flag)
	attr_removed = FALSE;

      curr_val->mod_flag = FALSE;
    }
    if (attr_removed) attrs->in_flag = FALSE;
  }
  mod_error.error = Okay;
  dn_free(mod_arg.mea_object);
  return mod_error;
}	  
  

void make_template(entry_name, attrs)
     char *entry_name;
     dirAttrs *attrs;
{
  static char buffer[RESBUF];
  PS ps;
  extern AttributeType at_objectclass;
  struct ds_read_arg read_arg;
  struct ds_read_result   result;
  struct DSError          error;
  Entry read_entry;
  Attr_Sequence   as, read_attrs;
  Attr_Sequence   nas = 0, tas = 0, templ_as, make_template_as();

  buffer[0] = '\0';
  *attrs = 0;

  if (*entry_name == '\0') return;

  if ((ps = ps_alloc(str_open)) == NULLPS) return;
  if (str_setup(ps, buffer, RESBUF, 1) == NOTOK) return;

  if (get_default_service (&read_arg.rda_common) != 0) return;

  read_arg.rda_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
  read_arg.rda_eis.eis_allattributes = TRUE;
  read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  
  read_arg.rda_object = str2dn(entry_name);

  photo_on = FALSE;
  
  if ((read_entry = local_find_entry(read_arg.rda_object, FALSE))
      != NULLENTRY &&
      read_entry->e_data != E_TYPE_CONSTRUCTOR) {
    read_attrs = read_entry->e_attributes;
  } else {
    if (ds_read(&read_arg, &error, &result) != DS_OK) {
      photo_on = TRUE;
      return;
    }
    if (result.rdr_entry.ent_attr == NULLATTR) {
      photo_on = TRUE;
      return;
    }

    read_attrs = result.rdr_entry.ent_attr;
  }

  for (as = read_attrs; as != NULLATTR; as = as->attr_link)
    if (as->attr_type == at_objectclass)
      break;

  templ_as = make_template_as(as->attr_value);
  
  for (as = read_attrs; 
       as != NULLATTR; 
       as = as->attr_link)
    if (!(AS_SYNTAX(as) == str2syntax("schema") ||
	  AS_SYNTAX(as) == str2syntax("objectclass") ||
	  AS_SYNTAX(as) == str2syntax("acl") ||
	  AS_SYNTAX(as) == str2syntax("edbinfo") ||
	  AS_SYNTAX(as) == str2syntax("octetstring") ||
	  !strcmp(AS_STROID(as), "0.9.2342.19200300.100.1.23") ||
	  !strcmp(AS_STROID(as), "0.9.2342.19200300.100.1.24"))) {
      nas = as_comp_new(AttrT_cpy(as->attr_type), 
			avs_cpy(as->attr_value),
			NULLACL_INFO);
      if (tas) tas = as_merge(tas, nas);
      else tas = nas;
    }

  as = templ_as;
  nas = templ_as = 0;
  for (; as != NULLATTR; as = as->attr_link)
    if (!(AS_SYNTAX(as) == str2syntax("schema") ||
          AS_SYNTAX(as) == str2syntax("objectclass") ||
          AS_SYNTAX(as) == str2syntax("acl") ||
          AS_SYNTAX(as) == str2syntax("edbinfo") ||
          AS_SYNTAX(as) == str2syntax("octetstring") ||
	  !strcmp(AS_STROID(as), "0.9.2342.19200300.100.1.23") ||
	  !strcmp(AS_STROID(as), "0.9.2342.19200300.100.1.24"))) {
      nas = as_comp_new(AttrT_cpy(as->attr_type), NULLAV, NULLACL_INFO);
      if (templ_as) templ_as = as_merge(templ_as, nas);
      else templ_as = nas;
    }

  as = as_merge(tas, templ_as);
  as = as_sort(as);
  
  my_as_print(ps, as, READOUT);
  *--ps->ps_ptr = NULL, ps->ps_cnt++;

  photo_on = TRUE;

  ps_free(ps);
  as_free(as);
  
  make_attr_sequence(buffer, attrs);
}

make_attr_sequence(entry_string, attrs)
     char *entry_string;
     dirAttrs *attrs;
{
  register char *str, *sptr;
  char save, buffer[RESBUF];
  modVals curr_val = 0;
  dirAttrs curr_attr = 0;
  AttributeType curr_attr_type;
  int count = 0;

  str = sptr = entry_string;
  while (1) {
    str = index(sptr, '-');
    if (!str || str == sptr) return;
    while (!isalpha(*str)) {
      if (str < sptr) return;
      else --str;
    }
    str++;

    save = *str;
    *str = '\0';
    
    if (curr_attr) {
      if (strcmp(curr_attr->attr_name, sptr)) {
	curr_val = 0;
	curr_attr->next = (dirAttrs) malloc(sizeof(dir_attrs));
	curr_attr = curr_attr->next;

	curr_attr->next = 0;
	curr_attr->val_seq = 0;
	curr_attr->mod_flag = FALSE;
	curr_attr->in_flag = TRUE;

	curr_attr->attr_name = strdup(sptr);

	curr_attr_type = str2AttrT(curr_attr->attr_name);
	if (!strcmp(curr_attr_type->oa_ot.ot_stroid, "2.5.4.35"))
	  curr_attr->hidden_flag = TRUE;
	else
	  curr_attr->hidden_flag = FALSE;
	AttrT_free(curr_attr_type);
      }
    } else {
      curr_attr = (dirAttrs) malloc(sizeof(dir_attrs));
      *attrs = curr_attr;

      curr_attr->next = 0;
      curr_attr->val_seq = (modVals) 0;
      curr_attr->mod_flag = FALSE;
      curr_attr->in_flag = TRUE;

      curr_attr->attr_name = strdup(sptr);

      curr_attr_type = str2AttrT(curr_attr->attr_name);
      if (!strcmp(curr_attr_type->oa_ot.ot_stroid, "2.5.4.35"))
	curr_attr->hidden_flag = TRUE;
      else
	curr_attr->hidden_flag = FALSE;
      AttrT_free(curr_attr_type);
    }
    
    *str = save;
    while ((isspace(*str) || (*str == '-')) && *str != '\n' && *str != '\0') 
      str++;

    sptr = str;
    count = 0;

    while (1) {
      while (*str != '\n' && *str != '\0') {
	buffer[count++] = *str;
	str++;
      }
      if (*str != '\0' && *(str + 1) == '\t') {
	buffer[count++] = '\n';
	while (isspace(*str)) str++;
      } else break;
    }

    buffer[count] = '\0';
    save = *str;
    *str = '\0';

    if (curr_val) {
      curr_val->next = (modVals) malloc(sizeof(struct mod_vals));
      curr_val = curr_val->next;
    } else {
      curr_val = (modVals) malloc(sizeof(struct mod_vals));
      curr_attr->val_seq = curr_val;

      if (!count) curr_attr->in_flag = FALSE;
    }
    curr_val->attr = curr_attr;
    curr_val->next = 0;
    curr_val->mod_flag = FALSE;
    curr_val->new_value = 0;
    curr_val->text_widg = 0;

    curr_val->value = strdup(buffer);
    char_map(buffer, curr_val->value);
    free(curr_val->value);
    curr_val->value = strdup(buffer);

    *str = save;
    if (*str == '\0' || *++str == '\0') return;
    sptr = str;
  }
}

void char_map(buffer, value)
     char *buffer, *value;
{
  if (value != NULLCP && *value != '\0') {
    while (*value != '\0') {
      switch (*value) {
      case '$':
	*buffer = '\n';
	break;
      case '\\':
	if (*((char *) (value + 1)) == '4' && 
	    *((char *) (value + 2)) == '0') {
	  *buffer = '@';
	  value += 2;
	}
	break;
      default:
	*buffer = *value;
      }
      value++;
      buffer++;
    }
  }
  
  *buffer = '\0';
}

void char_unmap(buffer, value)
     char *buffer, *value;
{
  if (value != NULLCP && *value != '\0') {
    while (*value != '\0') {
      switch (*value) {
      case '\n':
	*buffer = '$';
	break;
      default:
	*buffer = *value;
      }
      value++;
      buffer++;
    }
  }

  *buffer = '\0';
}
    

Attr_Sequence make_template_as(oc)
     AV_Sequence oc;
{
  AV_Sequence avs;
  Attr_Sequence newas;
  Attr_Sequence as = NULLATTR;
  table_seq optr;
  AttributeType at;
  objectclass * ocp;
  
  for (avs = oc; avs != NULLAV; avs = avs->avseq_next) {
    ocp = (objectclass *) avs->avseq_av.av_struct;
    for (optr = ocp->oc_must; optr != NULLTABLE_SEQ; optr = optr->ts_next) {
      at = optr->ts_oa;
      newas = as_comp_new(at, NULLAV, NULLACL_INFO);
      as = as_merge(as, newas);
    }
  }
  
  for (avs = oc; avs != NULLAV; avs = avs->avseq_next) {
    ocp = (objectclass *) avs->avseq_av.av_struct;
    for (optr = ocp->oc_may; optr != NULLTABLE_SEQ; optr = optr->ts_next) {
      at = optr->ts_oa;
      newas = as_comp_new(at, NULLAV, NULLACL_INFO);
      as = as_merge(as, newas);
    }
  }
  return(as);
}

/*Big Bodge!*/
char *modify_error(error)
     struct DSError *error;
{
  PS ps;
  char buffer[RESBUF];
  char *str, *message;
  
  if ((ps = ps_alloc(str_open)) == NULLPS) {
    return NULLCP;
  }

  if (str_setup(ps, buffer, RESBUF, 1) == NOTOK) {
    return NULLCP;
  }

  ds_error(ps, error);
  *ps->ps_ptr = 0;
  ps_free(ps);

  str = buffer;

  if (*str != '\0') {
    message = strdup(str);
  } else message = NULLCP;

  return message;
}

  
my_as_comp_print (ps,as,format)
     PS   ps;
     Attr_Sequence  as;
     int  format;
{
  AV_Sequence avs;
  char buffer[RESBUF];
  extern int oidformat;
  
  if (as!=NULLATTR) {
      if (format == READOUT)
	(void) sprintf(buffer,"%s",attr2name (as->attr_type, 1));
      else
	(void) sprintf (buffer,"%s",attr2name_aux (as->attr_type));
    
    if (split_attr (as)) {
      if ((as->attr_value == NULLAV) && (format != READOUT)) 
	ps_printf (ps, "%s=\n", buffer);
      else {
	if (as->attr_value == NULLAV) {
	  ps_printf(ps, "%-21s - \n", buffer);
	} else {
	  for (avs = as->attr_value; avs != NULLAV; avs = avs->avseq_next) {
	    if (format == READOUT)
	      ps_printf (ps, "%-21s - ", buffer);
	    else
	      ps_printf (ps, "%s= ", buffer);
	    avs_comp_print (ps, avs, EDBOUT);
	  ps_print (ps, "\n");
	  }
	}
      }
    } else {
      if (format == READOUT)
	ps_printf (ps, "%-21s - ", buffer);
      else
	ps_printf (ps, "%s= ", buffer);
      avs_print (ps, as->attr_value, format);
    }
  }
}

my_as_print (ps,as,format)
Attr_Sequence  as;
PS   ps;
int  format;
{
  if (as != NULLATTR)
    FOREACH(as)
      my_as_comp_print(ps,eptr,format);
}

Attr_Sequence as_sort(as)
     Attr_Sequence as;
{
  Attr_Sequence with_vals = NULLATTR, without_vals = NULLATTR, next_as;

  if (as == NULLATTR) return as;

  while (as != NULLATTR) {
    next_as = as->attr_link;
    as->attr_link = NULLATTR;

    if (as->attr_value) with_vals = as_merge(with_vals, as);
    else without_vals = as_merge(without_vals, as);
    
    as = next_as;
  }

  as = with_vals;

  if (as != NULLATTR) 
    for (; as->attr_link != NULLATTR; as = as->attr_link)
      ;
  else return without_vals;

  as->attr_link = without_vals;
  return with_vals;
}

    
