/* hashcom.h - Common defines for hashing filenames. */
#include "../hash.h"

#define FILENAME_HASH_BUCKETS 631

HASH_TABLE *hashed_filenames;

typedef struct {
  char *path;		/* The full pathname of the file. */
  int check_dot;	/* Whether `.' appeared before this one in $PATH. */
} PATH_DATA;

#define pathdata(x) ((PATH_DATA *)(x)->data)
