/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1btr.h,v 1.4 85/08/22 16:41:40 timo Exp $
*/

/* Private definitions for the b-tree module */

#ifndef INTEGRATION

extern bool comp_ok;
#define reqerr(s) error(s)

/*********************************************************************/
/* items
/*********************************************************************/

typedef char texitem;
typedef value lisitem;
typedef struct pair {value k, a;} tabitem;
typedef struct onpair {value ka, u;} keysitem;
typedef union itm {
    texitem c;
    lisitem l;
    tabitem t;
} item, *itemarray, *itemptr;

#define Charval(pitm) ((pitm)->c)
#define Keyval(pitm) ((pitm)->l)
#define Ascval(pitm) ((pitm)->t.a)

/* Xt = itemtype, do not change these, their order is used */
#define Ct (0)
#define Lt (1)
#define Tt (2)
#define Kt (3)

/* Itemwidth, used for offset in btreenodes */
typedef char width;
#define Itemwidth(it) (itemwidth[it])
extern char itemwidth[];	/*  uses: */
#define Cw (sizeof(texitem))
#define Lw (sizeof(lisitem))
#define Tw (sizeof(tabitem))
#define Kw (sizeof(keysitem))

/*********************************************************************/
/* sizes of btrees
/*********************************************************************/

#define Bigsize (-1)
#define Stail(r,s) ((r) > Maxint - (s) ? Bigsize : (r)+(s))
#define Ssum(r,s)  ((r) EQ Bigsize || (s) EQ Bigsize ? Bigsize : Stail(r,s))
#define Sincr(r)   ((r) EQ Bigsize ? Bigsize : Stail(r,1))
#define Sadd2(r)   ((r) EQ Bigsize ? Bigsize : Stail(r,2))
#define Sdiff(r,s) ((r) EQ Bigsize || (s) EQ Bigsize ? Bigsize : (r)-(s))
#define Sdecr(r)   ((r) EQ Bigsize ? Bigsize : (r)-(1))
value treesize(); 	/* btreeptr pnode */

/*********************************************************************/
/* (A,B)-btrees
/*********************************************************************/

/* innernodes: using A=6 B=12 */
#define Mininner 5 		/* A - 1 */
#define Maxinner 11 	        /* B - 1 */
/* bottomnodes */
#define Minbottom 11
#define Maxbottom 22
/* rangenodes */
#define Biglim		(Maxbottom+1)

typedef struct btrnode {
    HEADER; int size;
    char **g;
}
btreenode, *btreeptr;

typedef struct innernode {
    HEADER; int size;
    btreeptr ptr[Maxinner+1]; itemarray iitm;
}
innernode, *innerptr;

typedef struct itexnode {
    HEADER; int size;
    btreeptr ptr[Maxinner+1]; texitem icitm[Maxinner];
}
itexnode, *itexptr;

typedef struct ilisnode {
    HEADER; int size;
    btreeptr ptr[Maxinner+1]; lisitem ilitm[Maxinner];
}
ilisnode, *ilisptr;

typedef struct itabnode {
    HEADER; int size;
    btreeptr ptr[Maxinner+1]; tabitem ititm[Maxinner];
}
itabnode, *itabptr;

typedef struct bottomnode {
    HEADER; int size;
    itemarray bitm;
}
bottomnode, *bottomptr;

typedef struct btexnode {
    HEADER; int size;
    texitem bcitm[Maxbottom];
}
btexnode, *btexptr;

typedef struct blisnode {
    HEADER; int size;
    lisitem blitm[Maxbottom];
}
blisnode, *blisptr;

typedef struct btabnode {
    HEADER; int size;
    tabitem btitm[Maxbottom];
}
btabnode, *btabptr;

typedef struct rangenode {
    HEADER; int size;
    lisitem lwb, upb;
}
rangenode, *rangeptr;

#define Bnil ((btreeptr) 0)

#define Flag(pnode)	((pnode)->type)
#define Inner	'i'
#define Bottom	'b'
#define Irange  '.'
#define Crange  '\''

#define Lim(pnode)	((pnode)->len)
#define Minlim(pnode)	(Flag(pnode) EQ Inner ? Mininner : Minbottom)
#define Maxlim(pnode)	(Flag(pnode) EQ Inner ? Maxinner : Maxbottom)
#define SetRangeLim(pnode) (Size(pnode) EQ Bigsize || Size(pnode) > Maxbottom\
			    ? Biglim : Size(pnode))

#define Size(pnode)	((pnode)->size)

#define Ptr(pnode,l)	(((innerptr) (pnode))->ptr[l])
/* pointer to item in innernode: */
#define Piitm(pnode,l,w) ((itemptr) (((char*)&(((innerptr) (pnode))->iitm)) + ((l)*(w))))
/* pointer to item in bottomnode: */
#define Pbitm(pnode,l,w) ((itemptr) (((char*)&(((bottomptr) (pnode))->bitm)) + ((l)*(w))))
#define Ichar(pnode,l)	(((itexptr) (pnode))->icitm[l])
#define Bchar(pnode,l)	(((btexptr) (pnode))->bcitm[l])

#define Lwbval(pnode)	(((rangeptr) (pnode))->lwb)
#define Upbval(pnode)	(((rangeptr) (pnode))->upb)
#define Lwbchar(pnode)  (Bchar(Root(Lwbval(pnode)), 0))
#define Upbchar(pnode)  (Bchar(Root(Upbval(pnode)), 0))

#define Maxheight 20        /* should be some function of B */

/* Procedure merge(); */
    /* btreeptr pleft; itemptr pitm; btreeptr pright; literal it; */
bool rebalance();
    /* btreeptr *pptr1; itemptr pitm; btreeptr pptr2;
       intlet minlim, maxlim; literal it; */
/* Procedure restore_child(); */
    /* btreeptr pparent; intlet ichild, minl, maxl; literal it; */
bool inodeinsert();
    /* btreeptr pnode, *pptr; itemptr pitm; intlet at; literal it; */
bool bnodeinsert();
    /* btreeptr pnode, *pptr; itemptr pitm; intlet at; literal it; */
bool i_search();
    /* btreeptr pnode; value key; intlet *pl; width iw; */
bool b_search();
    /* btreeptr pnode; value key; intlet *pl; width iw; */

/*********************************************************************/
/* texts only (mbte.c)
/*********************************************************************/

btreeptr trimbtextnode(); /* btreeptr pnode, intlet from,to */
btreeptr trimitextnode(); /* btreeptr pnode, intlet from,to */
bool join_itm();
    /* btreeptr pnode, *pptr; itemptr pitm; bool after */

/*********************************************************************/
/* lists only (mbli.c)
/*********************************************************************/

btreeptr spawncrangenode(); /* value lwb, upb */
/* Procedure set_size_and_lim(); */ 	/* btreeptr pnode */
/* PRrocedure ir_to_bottomnode(); */ 	/* btreeptr *pptr; */
bool ins_itm();
    /* btreeptr *pptr1; itemptr pitm; btreeptr *pptr2; literal it; */
/* Procedure rem_greatest(); */
    /* btreeptr *pptr; itemptr prepl_itm; literal it; */
bool rem_itm(); 
    /* btreeptr *pptr1; itemptr pitm;
       itemptr p_insitm; btreeptr *pptr2; bool *psplit;
       literal it; */

/*********************************************************************/
/* tables only (mbla.c)
/*********************************************************************/

bool rpl_itm(); 
    /* btreeptr *pptr1, *pptr2; itemptr pitm; bool *p_added */
bool del_itm(); 
    /* btreeptr *pptr1; itemptr pitm */
value assocval(); 	/* btreeptr pnode; value key; */
bool assocloc();
    /* value **ploc; btreeptr pnode; value key; */
bool u_assoc();	/* btreeptr pnode; value key; */

/***************** Texts, lists and tables ********************/
/* Procedure move_itm(); */ 	/* itemptr pdes, psrc; literal it; */
bool get_th_item();	/* itemptr pitm; value num, v; */

#endif !INTEGRATION
