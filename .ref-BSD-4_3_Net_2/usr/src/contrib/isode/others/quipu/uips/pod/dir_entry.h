#ifndef ENTRYSEQ
#define ENTRYSEQ

#include <strings.h>
#include <X11/Intrinsic.h>
#include "defs.h"

#ifdef QUIPU_MALLOC

#else
extern char * malloc ();
extern char * smalloc ();
#endif

typedef struct mod_vals {
  char *value;
  char *new_value;
  Widget text_widg;
  bool mod_flag;
  struct dir_attrs *attr;
  struct mod_vals *next;
} mod_vals, *modVals;
  
typedef struct dir_attrs {
  char *attr_name;
  modVals val_seq;
  bool mod_flag;
  bool in_flag;
  bool hidden_flag;
  struct dir_attrs *next;
} dir_attrs, *dirAttrs;

typedef struct dir_entry {
  char *entry_name;
  dirAttrs attrs;
  bool mod_flag;
} dir_entry, *dirEntry;

#define NULLDIRENTRY ((dirEntry) 0)

int free_dir_entry();
int free_ent_attrs();
int free_mod_vals();

#endif

