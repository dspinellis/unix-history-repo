/*
 * Routines for handling file linking.
 */

#include "itran.h"
#include "lfile.h"

struct lfile *lfiles;

/*
 * alclfile allocates an lfile structure for the named file, fills
 *  in the name and returns a pointer to it.
 */
struct lfile *alclfile(name)
char *name;
   {
   struct lfile *p;
   char *np;
   int l;
   extern char *allocate();
   
   p = (struct lfile *)allocate(1,sizeof(struct lfile));
   if (!p)
      syserr("not enough memory for file list");
   p->lf_link = NULL;
   l = strlen(name);
   np = allocate(1,(l+1+sizeof(int *)) & ~(sizeof(int *)-1));
   if (!np)
      syserr("not enough memory for file list");
   strncpy(np,name,l);
   p->lf_name = np;
   return p;
   }
/*
 * dumplfiles - print the list of files to link.  Debugging only.
 */
dumplfiles()
{
   struct lfile *p,*lfls;

   printf("lfiles:\n");
   lfls = lfiles;
   while (p = getlfile(&lfls))
       printf("'%s'\n",p->lf_name);
}
/*
 * addlfile creates an lfile structure for the named file and add it to the
 *  end of the list of files (lfiles) to generate link instructions for.
 */
addlfile(name)
char *name;
{
   struct lfile *nlf, *p;
   
   nlf = alclfile(name);
   if (lfiles == NULL) {
      lfiles = nlf;
      }
   else {
      p = lfiles;
      while (p->lf_link != NULL) {
         p = p->lf_link;
         }
      p->lf_link = nlf;
      }
}
/*
 * getlfile returns a pointer (p) to the lfile structure pointed at by lptr
 *  and moves lptr to the lfile structure that p points at.  In other words,
 *  getlfile returns a pointer to the current (wrt. lptr) lfile and advances
 *  lptr.
 */
struct lfile *
getlfile(lptr)
struct lfile **lptr;
{
   struct lfile *p;
   
   if (*lptr == NULL)
      return NULL;
   else {
      p = *lptr;
      *lptr = p->lf_link;
      return p;
      }
}
