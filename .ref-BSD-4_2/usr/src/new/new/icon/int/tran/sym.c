#include "utran.h"
#include "token.h"
#include "sym.h"
#include "char.h"

int alclflg = 0;                /* flag (counter) for local table overflow */
int alcgflg = 0;                /* flag (counter) for global table overflow */
int alccflg = 0;                /* flag (counter) for constant table overflow */

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

char *putident(len)
int len;
   {
   register int hash;
   register char *s;
   register struct ientry *ip;
   int l;
   extern struct ientry *alcident();

   s = sfree;
   hash = 0;
   l = len;
   while (l--)
      hash += *s++ & 0377;
   s = sfree;
   l = len;
   hash &= imask;
   if ((ip = ihash[hash]) != NULL) {
      for (;;) {
         if (l == ip->i_length && streq(l, s, ip->i_name))
            return (ip->i_name);
         if (ip->i_blink == NULL) {
            ip->i_blink = alcident(NULL, s, l);
            sfree += l;
            return (s);
            }
         ip = ip->i_blink;
         }
      }
   ihash[hash] = alcident(NULL, s, l);
   sfree += l;
   return (s);
   }

streq(len, s1, s2)
register int len;
register char *s1, *s2;
   {
   while (len--)
      if (*s1++ != *s2++)
	 return (0);
   return (1);
   }

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
 * loc_init() -  clear local symbol table
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
   for (p = lhash; p < &lhash[lhsize]; p++)
      *p = NULL;
   lfree = ltable;
                                        /* clear constant table */
   maxcfree = (maxcfree > ctfree-ctable) ? maxcfree : ctfree-ctable;
   if (alccflg) {
      fprintf(stderr, "  %d more entries needed in literal symbol table\n",
	      alccflg);
      alccflg = 0;
      }
   for (p = chash; p < &chash[chsize]; p++)
      *p = NULL;
   ctfree = ctable;
   }

/*
 * install - put an identifier into the global or local symbol table
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
      case F_GLOBAL:
         if ((p.gp = glocate(name)) == NULL)
	    putglob(name, flag, argcnt);
         else
            p.gp->g_flag |= flag;
         break;

      case F_PROC|F_GLOBAL:
      case F_RECORD|F_GLOBAL:
      case F_BUILTIN|F_GLOBAL:
         if ((p.gp = glocate(name)) == NULL)
            putglob(name, flag, argcnt);
	 else if ((p.gp->g_flag & (~F_GLOBAL)) == 0) {
            p.gp->g_flag |= flag;
	    p.gp->g_nargs = argcnt;
	    }
         else
            err("inconsistent redeclaration", name);
         break;

      case F_STATIC:
      case F_DYNAMIC:
      case F_ARGUMENT:
         if ((p.lp = llocate(name)) == NULL)
            putloc(name,flag);
         else if (p.lp->l_flag == flag)
            warn("redeclared identifier", name);
         else
            err("inconsistent redeclaration", name);
         break;

      default:
         syserr("install: unrecognized symbol table flag.");
      }
   }

/*
 * putloc - make a local symbol table entry.
 */

int putloc(id,id_type)
char *id;
int id_type;
   {
   register struct lentry *ptr;
   extern struct lentry *llocate(), *alcloc();

   if ((ptr = llocate(id)) == NULL) {   /* add to head of hash chain */
      ptr = lhash[lhasher(id)];
      lhash[lhasher(id)] = alcloc(ptr, id, id_type);
      return (lhash[lhasher(id)] - ltable);
      }
   return (ptr - ltable);
   }

/*
 * putglob - make a global symbol table entry.
 */

int putglob(id, id_type, n_args)
char *id;
int id_type, n_args;
   {
   register struct gentry *ptr;
   extern struct gentry *glocate(), *alcglob();

   if ((ptr = glocate(id)) == NULL) {    /* add to head of hash chain */
      ptr = ghash[ghasher(id)];
      ghash[ghasher(id)] = alcglob(ptr, id, id_type, n_args);
      return (ghash[ghasher(id)] - gtable);
      }
   return (ptr - gtable);
   }

/*
 * putlit - make a constant symbol table entry.
 */

int putlit(id, idtype, len)
char *id;
int len, idtype;
   {
   register struct centry *ptr;
   extern struct centry	*clocate(), *alclit();

   if ((ptr = clocate(id,idtype)) == NULL) {    /* add to head of hash chain */
      ptr = chash[chasher(id)];
      chash[chasher(id)] = alclit(ptr, id, len, idtype);
      return (chash[chasher(id)] - ctable);
      }
   return (ptr - ctable);
   }

/*
 * llocate() - lookup identifier in local symbol table, NIL if not present.
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
 * glocate() - lookup identifier in global symbol table, NIL if not present.
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
 * clocate() - lookup identifier in constant symbol table, NIL if not present.
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
 * klocate - look up keyword in keyword table
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
 * ldump() - display local symbol table to stdout.
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
 * gdump() - display global symbol table to stdout.
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
 * cdump() - display constant symbol table to stdout.
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
 * alcloc - allocate a local symbol table entry, returns 0 if fails,
 *   else returns offset of new entry.
 */

struct lentry *alcloc(blink, name, flag)
struct lentry *blink;
char *name;
int flag;
   {
   register struct lentry *lp;

   if (lfree >= &ltable[lsize]) {         /* need more room */
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
 * alcglob - allocate a global symbol table entry, returns 0 if fails,
 *   else returns offset of new entry.
 */

struct gentry *alcglob(blink, name, flag, nargs)
struct gentry *blink;
char *name;
int flag, nargs;
   {
   register struct gentry *gp;

   if (gfree >= &gtable[gsize]) {         /* need more room */
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
 * alclit - allocate a constant symbol table entry, returns 0 if fails,
 *   else returns offset of new entry.
 */

struct centry *alclit(blink, name, len, flag)
struct centry *blink;
char *name;
int len, flag;
   {
   register struct centry *cp;

   if (ctfree >= &ctable[csize]) {         /* need more room */
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
 * lout - dump local symbol table to file.
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
 * cout(fd) - dump constant symbol table to file.
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
 * rout - dump a record declaration to file fd.
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
 * gout - dump global table to file fd.
 */

gout(fd)
FILE *fd;
   {
   register int i;
   register char *name;
   register struct gentry *gp;

   if (implicit == LOCAL)
      name = "local";
   else
      name = "error";
   fprintf(fd, "impl\t%s\n", name);
   if (trace)
      fprintf(fd, "trace\n");
   fprintf(fd, "global\t%d\n", gfree-gtable);
   i = 0;
   for (gp = gtable; gp < gfree; gp++)
      fprintf(fd, "\t%d,%06o,%s,%d\n", i++, gp->g_flag,
              gp->g_name, gp->g_nargs);
   }
