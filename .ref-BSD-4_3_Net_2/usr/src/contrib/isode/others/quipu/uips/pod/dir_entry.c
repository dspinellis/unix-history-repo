#include "dir_entry.h"

int free_dir_entry(entry)
     dirEntry entry;
{
  if (entry->entry_name) free(entry->entry_name);
  (void) free_ent_attrs(entry->attrs);
  free((char *) entry);
}

int free_ent_attrs(attrs)
     dirAttrs attrs;
{
  dirAttrs last_attr = 0;
  for (; attrs; attrs = attrs->next) {
    if (attrs->val_seq) free_mod_vals(attrs->val_seq);
    if (attrs->attr_name) free(attrs->attr_name);
    if (last_attr) free((char *) last_attr);
    last_attr = attrs;
  }
}

int free_mod_vals(vals)
     modVals vals;
{
  modVals last_val = 0;
  for (; vals; vals = vals->next) {
    if (vals->value) free(vals->value);
    if (vals->new_value) free(vals->new_value);
    if (last_val) free((char *)last_val);
    last_val = vals;
  }
}
