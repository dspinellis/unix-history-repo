#include "ulink.h"

int dynoff;			/* stack offset counter for locals */
int argoff;			/* stack offset counter for arguments */
int static1;			/* first static in procedure */
int statics = 0;		/* static variable counter */

int nlocal;			/* number of locals in local table */
int nconst;			/* number of constants in constant table */
int nfields = 0;		/* number of fields in field table */

/*
 * instalid - install string s into identifier table.
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
   while (*p1++ = *p2++)
      l++;
   return (putident(l));
   }

/*
 * putident - install string at sfree into identifier table,
 * if not already there.  Returns pointer to string.
 */

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
      hash += *s++;
   l = len;
   s = sfree;
   hash &= imask;
   if ((ip = ihash[hash]) != NULL) {
      for (;;) {
	 if (l == ip->i_length && lexeq(l, s, ip->i_name))
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

/*
 * lexeq - compare two strings of given length.  Returns
 * non-zero if equal, zero if not equal.
 */

lexeq(l, s1, s2)
register int l;
register char *s1, *s2;
   {
   while (l--)
      if (*s1++ != *s2++)
	 return (0);
   return (1);
   }

/*
 * alcident - create a new entry in the identifier table.
 */

struct ientry *alcident(blink, nam, len)
struct ientry *blink;
char *nam;
int len;
   {
   register struct ientry *ip;

   if (ifree >= &itable[isize])
      syserr("out of identifier table space");
   ip = ifree++;
   ip->i_blink = blink;
   ip->i_name = nam;
   ip->i_length = len;
   return (ip);
   }

/*
 * locinit -  clear local symbol table.
 */

locinit()
   {
   dynoff = 0;
   argoff = 0;
   nlocal = -1;
   nconst = -1;
   static1 = statics;
   }

/*
 * putloc - make a local symbol table entry.
 */

struct lentry *putloc(n, id, flags, imperror, procname)
int n;
char *id;
register int flags;
int imperror;
char *procname;
   {
   register struct lentry *lp;
   register union {
      struct gentry *gp;
      int bn;
      } p;
   extern struct gentry *glocate(), *putglob();

   if (n >= lsize)
      syserr("out of local symbol table space.");
   if (n > nlocal)
      nlocal = n;
   lp = &ltable[n];
   lp->l_name = id;
   lp->l_flag = flags;
   if (flags == 0) {                    /* undeclared */
      if ((p.gp = glocate(id)) != NULL) {	/* check global */
         lp->l_flag = F_GLOBAL;
         lp->l_val.global = p.gp;
         }
      else if ((p.bn = blocate(id)) != 0) {  /* check builtin */
         lp->l_flag = F_BUILTIN;
         lp->l_val.global = putglob(id, F_BUILTIN | F_PROC, -1, p.bn);
         }
      else {					/* implicit local */
	 if (imperror)
            warn(id, "undeclared identifier, procedure ", procname);
         lp->l_flag = F_DYNAMIC;
         lp->l_val.offset = ++dynoff;
         }
      }
   else if (flags & F_GLOBAL) {		/* global variable */
      if ((p.gp = glocate(id)) == NULL)
	 syserr("putloc: global not in global table");
      lp->l_val.global = p.gp;
      }
   else if (flags & F_ARGUMENT)		/* procedure argument */
      lp->l_val.offset = ++argoff;
   else if (flags & F_DYNAMIC)		/* local dynamic */
      lp->l_val.offset = ++dynoff;
   else if (flags & F_STATIC)		/* local static */
      lp->l_val.staticid = ++statics;
   else
      syserr("putloc: unknown flags");
   return (lp);
   }

/*
 * putglob - make a global symbol table entry.
 */

struct gentry *putglob(id, flags, nargs, procid)
char *id;
int flags;
int nargs;
int procid;
   {
   register struct gentry *p;
   extern struct gentry *glocate(), *alcglob();

   if ((p = glocate(id)) == NULL) {	/* add to head of hash chain */
      p = ghash[ghasher(id)];
      ghash[ghasher(id)] = alcglob(p, id, flags, nargs, procid);
      return (ghash[ghasher(id)]);
      }
   p->g_flag |= flags;
   p->g_nargs = nargs;
   p->g_procid = procid;
   return (p);
   }

/*
 * putconst - make a constant symbol table entry.
 */

struct centry *putconst(n, flags, len, pc, val)
int n;
int flags, len;
int pc;
union {
   long  ival;
   double rval;
   char *sval;
   } val;
   {
   register struct centry *p;

   if (n >= csize)
      syserr("out of literal table space");
   if (nconst < n)
      nconst = n;
   p = &ctable[n];
   p->c_flag = flags;
   p->c_pc = pc;
   if (flags & F_INTLIT) {
      p->c_val.ival = val.ival;
#ifndef BIT32
      if (val.ival < (long)(short)MINSHORT | val.ival > (long)(short)MAXSHORT)
	 p->c_flag |= F_LONGLIT;
#endif
      }
   else if (flags & F_STRLIT) {
      p->c_val.sval = val.sval;
      p->c_length = len;
      }
   else if (flags & F_CSETLIT) {
      p->c_val.sval = val.sval;
      p->c_length = len;
      }
   else	if (flags & F_REALLIT)
      p->c_val.rval = val.rval;
   else
      fprintf(stderr, "putconst: bad flags: %06o %011o\n", flags, val.ival);
   return (p);
   }

/*
 * putfield - make a record/field table entry.
 */

putfield(fname, rnum, fnum)
char *fname;
int rnum, fnum;
   {
   register struct fentry *fp;
   register struct rentry *rp, *rp2;
   int hash;
   extern struct fentry *flocate(), *alcfhead();
   extern struct rentry *alcfrec();

   fp = flocate(fname);
   if (fp == NULL) {			/* create a field entry */
      nfields++;
      hash = fhasher(fname);
      fp = fhash[hash];
      fhash[hash] = alcfhead(fp, fname, nfields, alcfrec(NULL, rnum, fnum));
      return;
      }
   rp = fp->f_rlist;			/* found field entry, look for */
   if (rp->r_recid > rnum) {		/*   spot in record list */
      fp->f_rlist = alcfrec(rp, rnum, fnum);
      return;
      }
   while (rp->r_recid < rnum) {		/* keep record list ascending */
      if (rp->r_link == NULL) {
	 rp->r_link = alcfrec(NULL, rnum, fnum);
	 return;
	 }
      rp2 = rp;
      rp = rp->r_link;
      }
   rp2->r_link = alcfrec(rp, rnum, fnum);
   }

/*
 * glocate - lookup identifier in global symbol table, NULL if not present.
 */

struct gentry *glocate(id)
char *id;
   {
   register struct gentry *p;

   p = ghash[ghasher(id)];
   while (p != NULL && p->g_name != id)
      p = p->g_blink;
   return (p);
   }

/*
 * flocate - lookup identifier in field table.
 */

struct fentry *flocate(id)
char *id;
   {
   register struct fentry *p;

   p = fhash[fhasher(id)];
   while (p != NULL && p->f_name != id)
      p = p->f_blink;
   return (p);
   }

/*
 * alcglob - create a new global symbol table entry.
 */

struct gentry *alcglob(blink, name, flag, nargs, procid)
struct gentry *blink;
char *name;
int flag;
int nargs;
int procid;
   {
   register struct gentry *gp;

   if (gfree >= &gtable[gsize])
      syserr("out of global symbol table space");
   gp = gfree++;
   gp->g_blink = blink;
   gp->g_name = name;
   gp->g_flag = flag;
   gp->g_nargs = nargs;
   gp->g_procid = procid;
   return (gp);
   }

/*
 * alcfhead - allocate a field table header.
 */

struct fentry *alcfhead(blink, name, fid, rlist)
struct fentry *blink;
char *name;
int fid;
struct rentry *rlist;
   {
   register struct fentry *fp;

   if (ffree >= &ftable[fsize])
      syserr("out of field table space");
   fp = ffree++;
   fp->f_blink = blink;
   fp->f_name = name;
   fp->f_fid = fid;
   fp->f_rlist = rlist;
   return (fp);
   }

/*
 * alcfrec - allocate a field table record list element.
 */

struct rentry *alcfrec(link, rnum, fnum)
struct rentry *link;
int rnum, fnum;
   {
   register struct rentry *rp;

   if (rfree >= &rtable[rsize])
      syserr("out of field table space for record lists");
   rp = rfree++;
   rp->r_link = link;
   rp->r_recid = rnum;
   rp->r_fnum = fnum;
   return (rp);
   }
