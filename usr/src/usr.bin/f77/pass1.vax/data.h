/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)data.h	5.1 (Berkeley) %G%
 */

/*  Forms of data list elements  */

#define	SIMPLE	1
#define NESTED	2


/*  data expression block types  */

#define	DVALUE	1
#define	DVAR	2
#define	DNAME	3
#define DEXPR	4
#define	DERROR	5


/*  status codes for integer values  */

#define	NORMAL	 1
#define MAXPLUS1 2
#define MINLESS1 3
#define	ERRVAL	 4



/*  data types for data statements  */


typedef
  struct Dvalue
    {
      char	tag;
      char	status;
      ftnint	value;
    }
  dvalue;


typedef
  struct Dvar
    {
      char	tag;
      struct Dvalue	*valp;
    }
  dvar;


typedef
  struct Dname
    {
      char	tag;
      int	len;
      char	*repr;
    }
  dname;


typedef
  struct Dexpr
    {
      char	tag;
      char	opcode;
      union Vexpr	*left;
      union Vexpr	*right;
    }
  dexpr;


typedef
  struct Derror
    {
      char	tag;
    }
  derror;


typedef
  union Vexpr
    {
      char	tag;
      struct Dvalue	dvalue;
      struct Dvar	dvar;
      struct Dname	dname;
      struct Dexpr	dexpr;
      struct Derror	derror;
    }
  vexpr;


typedef
  struct Elist
    {
      struct Elist	*next;
      union Delt	*elt;
    }
  elist;


typedef
  struct Aelt
    {
      char	tag;
      Namep	var;
      struct Vlist	*subs;
      struct Rpair	*range;
    }
  aelt;


typedef
  struct DoList
    {
      char	tag;
      struct Elist	*elts;
      union Vexpr	*dovar;
      union Vexpr	*init;
      union Vexpr	*limit;
      union Vexpr	*step;
    }
  dolist;


typedef
  union Delt
    {
      char	tag;
      struct Aelt	simple;
      struct DoList	nested;
    }
  delt;


typedef
  struct Rpair
    {
      union Vexpr	*low;
      union Vexpr	*high;
    }
  rpair;


typedef
  struct ValList
    {
      struct ValList	*next;
      char	status;
      int	repl;
      Constp	value;
    }
  vallist;


typedef
  struct Vlist
    {
      struct Vlist	*next;
      union Vexpr	*val;
    }
  vlist;


typedef
  struct DoVars
    {
      struct DoVars *next;
      int len;
      char *repr;
      dvalue *valp;
    }
  dovars;



/*  Global variables  */

extern int badvalue;
extern flag overlapflag;


/*  Global procedures  */

extern frvexpr();
extern frvlist();
extern elist *revelist();
extern vlist *revvlist();
extern vlist *prepvexpr();
extern elist *preplval();
extern delt *mkdlval();
extern delt *mkdatado();
extern rpair *mkdrange();
extern vallist *mkdrval();
extern vexpr *evicon();
extern vexpr *mkdname();
extern expptr ivaltoicon();
extern expptr evparam();
extern vexpr *evaldname();
extern vexpr *mkdexpr();
extern vexpr *addivals();
extern vexpr *subivals();
extern vexpr *mulivals();
extern vexpr *divivals();
extern vexpr *powivals();
extern vexpr *negival();
extern expptr convconst();
extern savedata();
