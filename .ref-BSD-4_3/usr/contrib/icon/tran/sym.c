/*
 * Routines for symbol table management.
 */

#include "itran.h"
#include "token.h"
#include "sym.h"
#include "char.h"
#include "lfile.h"

int alclflg = 0;		/* flag (counter) for local table overflow */
int alcgflg = 0;		/* flag (counter) for global table overflow */
int alccflg = 0;		/* flag (counter) for constant table overflow */

/*
 * instalid - copy the string s to the start of the string free space
 *  and call putident with the length of the string.
 */
char *instalid(s)
char *s;
   {
   register int l;
   register char *p1, *p2;
   extern char *putident();

   p1 = sfree;
   p2 = s;
   l = 1;
   while (*p1++ = *p2++) {
      if (p1 >= send)
         syserr("out of string space");
      l++;
      }
   return (putident(l));
   }

/*
 * putident - install the identifier named by the string starting at sfree
 *  and extending for len bytes.  The installation entails making an
 *  entry in the identifier hash table and then making an identifier
 *  table entry for it with alcident.  A side effect of installation
 *  is the incrementing of sfree by the length of the string, thus
 *  "saving" it.
 *
 * Nothing is changed if the identifier has already been installed.
 */
char *putident(len)
int len;
   {
   register int hash;
   register char *s;
   register struct ientry *ip;
   int l;
   extern struct ientry *alcident();

   /*
    * Compute hash value by adding bytes and masking result with imask.
    *  (Recall that imask is ihsize-1.)
    */
   s = sfree;
   hash = 0;
   l = len;
   while (l--)
      hash += *s++ & 0377;
   s = sfree;
   l = len;
   hash &= imask;
   /*
    * If the identifier hasn't been installed, install it.
    */
   if ((ip = ihash[hash]) != NULL) {	/* collision */
      for (;;) {	/* work down i_blink chain until id is found or the
			    end of the chain is reached */
         if (l == ip->i_length && streq(l, s, ip->i_name))
            return (ip->i_name); 	/* id is already installed */
         if (ip->i_blink == NULL) {	/* end of chain */
            ip->i_blink = alcident(NULL, s, l);
            sfree += l;
            return (s);
            }
         ip = ip->i_blink;
         }
      }
   /*
    * Hashed to an empty slot.
    */
   ihash[hash] = alcident(NULL, s, l);
   sfree += l;
   return (s);
   }

/*
 * streq - compare s1 with s2 for len bytes, and return 1 for equal,
 *  0 for not equal.
 */
streq(len, s1, s2)
register int len;
register char *s1, *s2;
   {
   while (len--)
      if (*s1++ != *s2++)
         return (0);
   return (1);
   }
/*
 * alcident - get the next free identifier table entry, and fill it in with
 *  the specified values.
 */
struct ientry *alcident(blink, nam, len)
struct ientry *blink;
char *nam;
int len;
   {
   register struct ientry *ip;

   ip = ifree++;
   ip->i_blink = blink;
   ip->i_name = nam;
   ip->i_length = len;
   return (ip);
   }

/*
 * loc_init - clear the local symbol table.
 */

loc_init()
   {
   register *p;
   static int maxlfree = 0;
   static int maxcfree = 0;
                                        /* clear local table */
   maxlfree = (maxlfree > lfree-ltable) ? maxlfree : lfree-ltable;
   if (alclflg) {
      fprintf(stderr, "  %d more entries needed in local symbol table\n",
              alclflg);
      alclflg = 0;
      }
   for (p = (int *) lhash; p < (int *) &lhash[lhsize]; p++)
      *p = NULL;
   lfree = ltable;
                                        /* clear constant table */
   maxcfree = (maxcfree > ctfree-ctable) ? maxcfree : ctfree-ctable;
   if (alccflg) {
      fprintf(stderr, "  %d more entries needed in literal symbol table\n",
              alccflg);
      alccflg = 0;
      }
   for (p = (int *) chash; p < (int *) &chash[chsize]; p++)
      *p = NULL;
   ctfree = ctable;
   }

/*
 * install - put an identifier into the global or local symbol table.
 *  The basic idea here is to look in the right table and install
 *  the identifier if it isn't already there.  Some semantic checks
 *  are performed.
 */
install(name, flag, argcnt)
char *name;
int flag, argcnt;
   {
   register union {
      struct gentry *gp;
      struct lentry *lp;
      } p;
   extern struct gentry *glocate();
   extern struct lentry *llocate();

   switch (flag) {
      case F_GLOBAL:	/* a variable in a global declaration */
         if ((p.gp = glocate(name)) == NULL)
            putglob(name, flag, argcnt);
         else
            p.gp->g_flag |= flag;
         break;

      case F_PROC|F_GLOBAL:	/* procedure declaration */
      case F_RECORD|F_GLOBAL:	/* record declaration */
      case F_BUILTIN|F_GLOBAL:	/* external declaration */
         if ((p.gp = glocate(name)) == NULL)
            putglob(name, flag, argcnt);
         else if ((p.gp->g_flag & (~F_GLOBAL)) == 0) { /* superfluous global
                                                           declaration for
                                                           record or proc */
            p.gp->g_flag |= flag;
            p.gp->g_nargs = argcnt;
            }
         else			/* the user can't make up his mind */
            err("inconsistent redeclaration", name);
         break;

      case F_STATIC: 	/* static declaration */
      case F_DYNAMIC:	/* local declaration (possibly implicit?) */
      case F_ARGUMENT:	/* formal parameter */
         if ((p.lp = llocate(name)) == NULL)
            putloc(name,flag);
         else if (p.lp->l_flag == flag) /* previously declared as same type */
            warn("redeclared identifier", name);
         else		/* previously declared as different type */
            err("inconsistent redeclaration", name);
         break;

      default:
         syserr("install: unrecognized symbol table flag.");
      }
   }

/*
 * putloc - make a local symbol table entry and return the index
 *  of the entry in lhash.  alcloc does the work if there is a collision.
 */
int putloc(id,id_type)
char *id;
int id_type;
   {
   register struct lentry *ptr;
   extern struct lentry *llocate(), *alcloc();

   if ((ptr = llocate(id)) == NULL) {	/* add to head of hash chain */
      ptr = lhash[lhasher(id)];
      lhash[lhasher(id)] = alcloc(ptr, id, id_type);
      return (lhash[lhasher(id)] - ltable);
      }
   return (ptr - ltable);
   }

/*
 * putglob makes a global symbol table entry and returns the index
 *  of the entry in ghash.  alcglob does the work if there is a collision.
 */

int putglob(id, id_type, n_args)
char *id;
int id_type, n_args;
   {
   register struct gentry *ptr;
   extern struct gentry *glocate(), *alcglob();

   if ((ptr = glocate(id)) == NULL) {	 /* add to head of hash chain */
      ptr = ghash[ghasher(id)];
      ghash[ghasher(id)] = alcglob(ptr, id, id_type, n_args);
      return (ghash[ghasher(id)] - gtable);
      }
   return (ptr - gtable);
   }

/*
 * putlit makes a constant symbol table entry and returns the index
 *  of the entry in chash.  alclit does the work if there is a collision.
 */
int putlit(id, idtype, len)
char *id;
int len, idtype;
   {
   register struct centry *ptr;
   extern struct centry	*clocate(), *alclit();

   if ((ptr = clocate(id,idtype)) == NULL) {   /* add to head of hash chain */
      ptr = chash[chasher(id)];
      chash[chasher(id)] = alclit(ptr, id, len, idtype);
      return (chash[chasher(id)] - ctable);
      }
   return (ptr - ctable);
   }

/*
 * llocate looks up id in local symbol table and returns pointer to
 *  to it if found or NULL if not present.
 */

struct lentry *llocate(id)
char *id;
   {
   register struct lentry *ptr;

   ptr = lhash[lhasher(id)];
   while (ptr != NULL && ptr->l_name != id)
      ptr = ptr->l_blink;
   return (ptr);
   }

/*
 * glocate looks up id in global symbol table and returns pointer to
 *  to it if found or NULL if not present.
 */
struct gentry *glocate(id)
char *id;
   {
   register struct gentry *ptr;

   ptr = ghash[ghasher(id)];
   while (ptr != NULL && ptr->g_name != id) {
      ptr = ptr->g_blink;
      }
   return (ptr);
   }

/*
 * clocate looks up id in constant symbol table and returns pointer to
 *  to it if found or NULL if not present.
 */
struct centry *clocate(id,flag)
char *id;
int flag;
   {
   register struct centry *ptr;

   ptr = chash[chasher(id)];
   while (ptr != NULL && (ptr->c_name != id || ptr->c_flag != flag))
      ptr = ptr->c_blink;

   return (ptr);
   }

/*
 * klocate looks up keyword named by id in keyword table and returns
 *  its number (keyid).
 */
klocate(id)
register int id;
   {
   register struct keyent *kp;

   for (kp = keytab; kp->keyid >= 0; kp++)
      if (strcmp(kp->keyname,id) == 0)
         return (kp->keyid);

   return (NULL);
   }

/*
 * ldump displays local symbol table to stdout.
 */

ldump()
   {
   register int i;
   register struct lentry *lptr;

   printf("Dump of local symbol table (%d entries)\n",lfree-ltable);
   printf(" loc   blink   id              (name)      flags\n");
   for (i = 0; i < lhsize; i++)
      for (lptr = lhash[i]; lptr != NULL; lptr = lptr->l_blink)
         printf("%5d  %5d  %5d  %20s  %7o\n", lptr-ltable,
                lptr->l_blink, lptr->l_name, lptr->l_name, lptr->l_flag);

   }

/*
 * gdump displays global symbol table to stdout.
 */

gdump()
   {
   register int i;
   register struct gentry *gptr;

   printf("Dump of global symbol table (%d entries)\n",gfree-gtable);
   printf(" loc   blink   id              (name)      flags       nargs\n");
   for (i = 0; i < ghsize; i++)
      for (gptr = ghash[i]; gptr != NULL; gptr = gptr->g_blink)
         printf("%5d  %5d  %5d  %20s  %7o   %8d\n", gptr-gtable,
                gptr->g_blink, gptr->g_name, gptr->g_name,
                gptr->g_flag, gptr->g_nargs);
   }

/*
 * cdump displays constant symbol table to stdout.
 */

cdump()
   {
   register int i;
   register struct centry *cptr;

   printf("Dump of constant symbol table (%d entries)\n",ctfree-ctable);
   printf(" loc   blink   id              (name)      flags\n");
   for (i = 0; i < chsize; i++)
      for (cptr = chash[i]; cptr != NULL; cptr = cptr->c_blink)
         printf("%5d  %5d  %5d  %20s  %7o\n", cptr-ctable,
                cptr->c_blink, cptr->c_name, cptr->c_name, cptr->c_flag);
   }

/*
 * alcloc allocates a local symbol table entry, fills in fields with
 *  specified values and returns offset of new entry.  
 */
struct lentry *alcloc(blink, name, flag)
struct lentry *blink;
char *name;
int flag;
   {
   register struct lentry *lp;

   if (lfree >= &ltable[lsize]) {	  /* need more room */
      if (alclflg == 0)
         syserr("out of local symbol table space");
      alclflg++;
      return (NULL);
      }
   lp = lfree++;
   lp->l_blink = blink;
   lp->l_name = name;
   lp->l_flag = flag;
   return (lp);
   }

/*
 * alcglob allocates a global symbol table entry, fills in fields with
 *  specified values and returns offset of new entry.  
 */
struct gentry *alcglob(blink, name, flag, nargs)
struct gentry *blink;
char *name;
int flag, nargs;
   {
   register struct gentry *gp;

   if (gfree >= &gtable[gsize]) {	  /* need more room */
      if (alcgflg == 0)
         syserr("out of global symbol table space");
      alcgflg++;
      return (NULL);
      }
   gp = gfree++;
   gp->g_blink = blink;
   gp->g_name = name;
   gp->g_flag = flag;
   gp->g_nargs = nargs;
   return (gp);
   }

/*
 * alclit allocates a constant symbol table entry, fills in fields with
 *  specified values and returns offset of new entry.  
 */
struct centry *alclit(blink, name, len, flag)
struct centry *blink;
char *name;
int len, flag;
   {
   register struct centry *cp;

   if (ctfree >= &ctable[csize]) {	   /* need more room */
      if (alccflg == 0)
         syserr("out of constant table space");
      alccflg++;
      return (NULL);
      }
   cp = ctfree++;
   cp->c_blink = blink;
   cp->c_name = name;
   cp->c_length = len;
   cp->c_flag = flag;
   return (cp);
   }

/*
 * lout dumps local symbol table to fd, which is a .u1 file.
 */
lout(fd)
FILE *fd;
   {
   register int i;
   register struct lentry *lp;

   i = 0;
   for (lp = ltable; lp < lfree; lp++)
      fprintf(fd, "\tlocal\t%d,%06o,%s\n",
         i++, lp->l_flag, lp->l_name);
   }


/*
 * cout dumps constant symbol table to fd, which is a .u1 file.
 */
cout(fd)
FILE *fd;
   {
   register int l;
   register char *c;
   register struct centry *cp;
   int i;

   i = 0;
   for (cp = ctable; cp < ctfree; cp++) {
      fprintf(fd, "\tcon\t%d,%06o", i++, cp->c_flag);
      if (cp->c_flag & (F_INTLIT|F_REALLIT))
         fprintf(fd, ",%s\n", cp->c_name);
      else {
         c = cp->c_name;
         l = cp->c_length - 1;
         fprintf(fd, ",%d", l);
         while (l--)
            fprintf(fd, ",%03o", *c++ & 0377);
         putc('\n', fd);
         }
      }
   }


/*
 * rout dumps a record declaration for name to file fd, which is a .u2 file.
 */
rout(fd,name)
FILE *fd;
char *name;
   {
   register int i;
   register struct lentry *lp;

   fprintf(fd, "record\t%s,%d\n", name, lfree-ltable);
   i = 0;
   for (lp = ltable; lp < lfree; lp++)
      fprintf(fd, "\t%d,%s\n", i++, lp->l_name);
   }


/*
 * gout writes various items to fd, which is a .u2 file.  These items
 *  include: implicit status, tracing activation, link directives,
 *  and the global table.
 */
gout(fd)
FILE *fd;
   {
   register int i;
   register char *name;
   register struct gentry *gp;
   struct lfile *lfl;
   
   if (implicit == LOCAL)
      name = "local";
   else
      name = "error";
   fprintf(fd, "impl\t%s\n", name);
   if (trace)
      fprintf(fd, "trace\n");
   
   lfl = lfiles;
   while (lfl) {
      fprintf(fd,"link\t%s.u1\n",lfl->lf_name);
      lfl = lfl->lf_link;
      }
   lfiles = 0;
   fprintf(fd, "global\t%d\n", gfree-gtable);
   i = 0;
   for (gp = gtable; gp < gfree; gp++)
      fprintf(fd, "\t%d,%06o,%s,%d\n", i++, gp->g_flag,
         gp->g_name, gp->g_nargs);
   }
