
/* Alist matching source file names to GDB filenumbers.
   Used in output_source_line.  */

struct gdbfile
{
  struct gdbfile *next;
  char *name;			/* name of source file */
  int filenum;			/* Assigned number */
  int nlines;			/* # lines generated for this source file */
};

/* Chain of all `struct gdbfile's.  */

extern struct gdbfile *gdbfiles;
