/*
	These are machine-configuration dependent
	tables.  To add a machine, be sure to update all
	these tables and to add the "ifdef" entry in "mach.h".
	This file corresponds to the following network:

	Ing70-----------CSVAX		A              B
	  |		 |		|              |
	  |		 |		|              |
	  |		 |		|              |
	  |		 |		|              |
	IngVAX		Cory------------C--------------D------------Q
			 |		|	       |\
			 |		|	       | \
			 |		|	       |  \
			 |		|	       |   \
	EECS40----------OptVAX		E	      SRC   F
	   		 |
	   		 |
	   		 |
	   		 |
			Image

	The tables must be consistent.
	To be added:

*/
/* Computer Center A Machine (A) */
char configA[] = {		/* to get to i, config[i] */
	'a','c','c','c','c',		/* a,b,c,d,e */
	'c',000,000,'c','c',		/* f,g,h,i,j */
	000,000,'c',000,'c',		/* k,l,m,n,o */
	000,'c',000,'c',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center B Machine (B) */
char configB[] = {		/* to get to i, config[i] */
	'd','b','d','d','d',		/* a,b,c,d,e */
	'd',000,000,'d','d',		/* f,g,h,i,j */
	000,000,'d',000,'d',		/* k,l,m,n,o */
	000,'d',000,'d',000,		/* p,q,r,s,t */
	000,'d',000,000,'d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Computer Center C Machine (C) */
char configC[] = {		/* to get to i, config[i] */
	'a','d','c','d','e',		/* a,b,c,d,e */
	'd',000,000,'y','y',		/* f,g,h,i,j */
	000,000,'y',000,'y',		/* k,l,m,n,o */
	000,'d',000,'d',000,		/* p,q,r,s,t */
	000,'y',000,000,'y',		/* u,v,w,x,y */
	'y',0				/* z */
	};
/* Computer Center D Machine (D) */
char configD[] = {		/* to get to i, config[i] */
	'c','b','c','d','c',		/* a,b,c,d,e */
	'f',000,000,'c','c',		/* f,g,h,i,j */
	000,000,'c',000,'c',		/* k,l,m,n,o */
	000,'q',000,'s',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center E Machine (E) */
char configE[] = {		/* to get to i, config[i] */
	'c','c','c','c','e',		/* a,b,c,d,e */
	'c',000,000,'c','c',		/* f,g,h,i,j */
	000,000,'c',000,'c',		/* k,l,m,n,o */
	000,'c',000,'c',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center F Machine (F) */
char configF[] = {		/* to get to i, config[i] */
	'd','d','d','d','d',		/* a,b,c,d,e */
	'f',000,000,'d','d',		/* f,g,h,i,j */
	000,000,'d',000,'d',		/* k,l,m,n,o */
	000,'d',000,'d',000,		/* p,q,r,s,t */
	000,'d',000,000,'d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Project INGRES 11/70 (Ing70) */
char configI[] = {		/* to get to i, config[i] */
	'v','v','v','v','v',		/* a,b,c,d,e */
	'v',000,000,'i','j',		/* f,g,h,i,j */
	000,000,'v',000,'v',		/* k,l,m,n,o */
	000,'v',000,'v',000,		/* p,q,r,s,t */
	000,'v',000,000,'v',		/* u,v,w,x,y */
	'v',0				/* z */
	};
/* Project INGRES VAX (IngVAX) */
char configJ[] = {		/* to get to i, config[i] */
	'i','i','i','i','i',		/* a,b,c,d,e */
	'i',000,000,'i','j',		/* f,g,h,i,j */
	000,000,'i',000,'i',		/* k,l,m,n,o */
	000,'i',000,'i',000,		/* p,q,r,s,t */
	000,'i',000,000,'i',		/* u,v,w,x,y */
	'i',0				/* z */
	};
/* Sakrison's Image Project 11/40 (Image) */
char configM[] = {		/* to get to i, config[i] */
	'o','o','o','o','o',		/* a,b,c,d,e */
	'o',000,000,'o','o',		/* f,g,h,i,j */
	000,000,'m',000,'o',		/* k,l,m,n,o */
	000,'o',000,'o',000,		/* p,q,r,s,t */
	000,'o',000,000,'o',		/* u,v,w,x,y */
	'o',0				/* z */
	};
/* Pfeister - Pollack - Sangiovanni Optimization VAX (OptVAX) */
char configO[] = {		/* to get to i, config[i] */
	'y','y','y','y','y',		/* a,b,c,d,e */
	'y',000,000,'y','y',		/* f,g,h,i,j */
	000,000,'m',000,'o',		/* k,l,m,n,o */
	000,'y',000,'y',000,		/* p,q,r,s,t */
	000,'y',000,000,'y',		/* u,v,w,x,y */
	'z',0				/* z */
	};
/* Computer Center Q Machine (Q) */
char configQ[] = {		/* to get to i, config[i] */
	'd','d','d','d','d',		/* a,b,c,d,e */
	'd',000,000,'d','d',		/* f,g,h,i,j */
	000,000,'d',000,'d',		/* k,l,m,n,o */
	000,'q',000,'d',000,		/* p,q,r,s,t */
	000,'d',000,000,'d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Survey Research Center 11/40 (SRC) */
char configS[] = {		/* to get to i, config[i] */
	'd','d','d','d','d',		/* a,b,c,d,e */
	'd',000,000,'d','d',		/* f,g,h,i,j */
	000,000,'d',000,'d',		/* k,l,m,n,o */
	000,'d',000,'s',000,		/* p,q,r,s,t */
	000,'d',000,000,'d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* EECS Research (Fateman - Ernie) VAX (CSVAX) */
char configV[] = {		/* to get to i, config[i] */
	'y','y','y','y','y',		/* a,b,c,d,e */
	'y',000,000,'i','i',		/* f,g,h,i,j */
	000,000,'y',000,'y',		/* k,l,m,n,o */
	000,'y',000,'y',000,		/* p,q,r,s,t */
	000,'v',000,000,'y',		/* u,v,w,x,y */
	'y',0				/* z */
	};
/* EECS Instructional 11/70 (199 Cory) (Cory) */
char configY[] = {		/* to get to i, config[i] */
	'c','c','c','c','c',		/* a,b,c,d,e */
	'c',000,000,'v','v',		/* f,g,h,i,j */
	000,000,'o',000,'o',		/* k,l,m,n,o */
	000,'c',000,'c',000,		/* p,q,r,s,t */
	000,'v',000,000,'y',		/* u,v,w,x,y */
	'o',0				/* z */
	};
/* EECS Departmental 11/40  (EECS40) */
char configZ[] = {		/* to get to i, config[i] */
	'o','o','o','o','o',		/* a,b,c,d,e */
	'o',000,000,'o','o',		/* f,g,h,i,j */
	000,000,'o',000,'o',		/* k,l,m,n,o */
	000,'o',000,'o',000,		/* p,q,r,s,t */
	000,'o',000,000,'o',		/* u,v,w,x,y */
	'z',0				/* z */
	};
/* this table is used by netlpr to do lpr w/o an acct
   and by net and netdaemon to do pre-emption */
/* sub.c uses the table in initdaemon to check machine
   type - errormsg may be ignored */
char machtype[]= {
	M_CC, M_CC, M_CC, M_CC, M_CC,	/* a,b,c,d,e */
	M_CC, 0, 0, M_INGRES, M_INGRES,	/* f,g,h,i,j */
	0, 0, M_OTHER, 0, M_VAX,		/* k,l,m,n,o */
	0, M_CC, 0, M_SRC, 0,		/* p,q,r,s,t */
	0, M_VAX, 0, 0, M_CORY,		/* u,v,w,x,y */
	M_OTHER, 0};				/* z */

/* this is basically the default machine for each local machine */
char remtable[] = {
	'c','d','a','c','c',		/* a,b,c,d,e */
	'd',000,000,'j','i',		/* f,g,h,i,j */
	000,000,'v',000,'v',		/* k,l,m,n,o */
	000,'d',000,'d',000,		/* p,q,r,s,t */
	000,'y',000,000,'v',		/* u,v,w,x,y */
	'i',0				/* z */
	};
/* bad login names */
struct bstruct btable[] = {
	"op", 'a',
	0,0 };
/* this table shows the correspondence between
   machine names like 'Cory' and their internal
   names, like 'y' */
static struct tt {
	char *bigname;
	char lname;
	} table[] = {
	"A",		'a',
	"B",		'b',
	"C",		'c',
	"D",		'd',
	"E",		'e',
	"F",		'f',
	"CCVAX", 	'f',
	"Ing70",	'i',
	"Ingres",	'i',
	"IngVAX",	'j',
	"Image",	'm',
	"OptVAX",	'o',
	"SESM",		'o',
	"Q",		'q',
	"SRC",		's',
	"CSVAX",	'v',
	"ucbvax", 	'v',
	"VAX",		'v',
	"Cory",		'y',
	"EECS40", 	'z',
	0, 		0
	};
