/* Copyright (c) 1979 Regents of the University of California */
/*
	This file corresponds to the following network:

				A
				|
				|
				|
				|
		Cory------------C--------------D
		 |		|	       |
		 |		|	       |
		 |		|	       |
		 |		|	       |
		VAX		E	      SRC

	The tables must be consistent.
*/
char configA[] = {		/* to get to i, config[i] */
	'a',000,'c','c','c',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'c',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configB[] = {		/* to get to i, config[i] */
	000,0				/* z */
	};
char configC[] = {		/* to get to i, config[i] */
	'a',000,'c','d','e',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'d',000,		/* p,q,r,s,t */
	000,'y',000,000,'y',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configD[] = {		/* to get to i, config[i] */
	'c',000,'c','d','c',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'s',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configE[] = {		/* to get to i, config[i] */
	'c',000,'c','c','e',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'c',000,		/* p,q,r,s,t */
	000,'c',000,000,'c',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configI[] = {		/* to get to i, config[i] */
	000,0				/* z */
	};
char configQ[] = {		/* to get to i, config[i] */
	000,0				/* z */
	};
char configS[] = {		/* to get to i, config[i] */
	'd',000,'d','d','d',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'s',000,		/* p,q,r,s,t */
	000,'d',000,000,'d',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configV[] = {		/* to get to i, config[i] */
	'y',000,'y','y','y',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'y',000,		/* p,q,r,s,t */
	000,'v',000,000,'y',		/* u,v,w,x,y */
	000,0				/* z */
	};
char configY[] = {		/* to get to i, config[i] */
	'c',000,'c','c','c',		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,'c',000,		/* p,q,r,s,t */
	000,'v',000,000,'y',		/* u,v,w,x,y */
	000,0				/* z */
	};
/* this table is used by netlpr to do lpr w/o an acct
   and by net and netdaemon to do pre-emption */
/* sub.c uses the table in initdaemon to check machine
   type - errormsg may be ignored */
char machtype[]= {
	M_CC, M_CC, M_CC, M_CC, M_CC,	/* a,b,c,d,e */
	0, 0, 0, M_INGRES, 0,		/* f,g,h,i,j */
	0, 0, 0, 0, 0,			/* k,l,m,n,o */
	0, M_CC, 0, M_SRC, 0,		/* p,q,r,s,t */
	0, M_VAX, 0, 0, M_CORY,		/* u,v,w,x,y */
	0};				/* z */
/* this is basically the default machine for each local machine */
char remtable[] = {
	'c','c','a','c','c',		/* a,b,c,d,e */
	000,000,000,'y',000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,'c',000,'d',000,		/* p,q,r,s,t */
	000,'y',000,000,'v',		/* u,v,w,x,y */
	000,0				/* z */
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
	"A",	'a',
	"B",	'b',
	"C",	'c',
	"D",	'd',
	"E",	'e',
	"INGRES", 'i',
	"Q",	'q',
	"SRC",	's',
	"VAX",	'v',
	"Cory",	'y',
	0, 0 };
