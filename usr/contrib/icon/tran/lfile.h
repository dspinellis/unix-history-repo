/*
 * A linked list of files named by "link" directives is maintained using
 *  lfile structures.
 */
struct lfile {
   char *lf_name;		/* name of the file */
   struct lfile *lf_link;	/* pointer to next file */
   };
extern struct lfile *lfiles;
extern struct lfile *getlfile();

