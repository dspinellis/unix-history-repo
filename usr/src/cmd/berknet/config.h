/*
	These are machine-configuration dependent
	tables.  To add a machine, be sure to update all
	these tables, add the "ifdef" entry in "mach.h",
	and add config? to gothru() in sub.c.
	For Berkeley, this file corresponds to the following network:

	IngVAX          ARPA
	  |		 |    Onyx
	  |		 |     /
	  |		 |    /
	  |		 |   /
	Ing70-----------CSVAX		A              B
	   		 |		|              |
	   		 |		|              |
	   		 |		|              |
	   		 |		|              |
	MathStat--------Cory------------C--------------D------------F
			 |		|	       |
			 |		|	       |
			 |		|	       |
			 |		|	       |
	EECS40----------ESVAX		E	      SRC
	   		 |	       /|
	   		 |     	      / | 
	   		 |	     /	|  
	   		 |	    /	|   
	VLSI------------Image	 Virus	Q   

	The tables must be consistent.


	For RAND, these tables are:

		VAX (C) ---------GRAPHICS (A)------- TP (B)

	For NOSC, these tables are:

		atts (a) ------- mssf (m) ---------- ccmm (c)

*/
# ifdef RAND
/* GRAPHICS = A */
char configA[] = {		/* to get to i, config[i] */
	'a','b','c',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* TP = B */
char configB[] = {		/* to get to i, config[i] */
	'a','b','a',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* VAX = C */
char configC[] = {		/* to get to i, config[i] */
	'a','a','c',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* if machtype is
	M_CC		netlpr will do lpr w/o an acct.
			Will pre-encrypt the password.
	M_INGRES	will allow higher file lengths.
	M_OTHER		will give no unusual effects.
(when in doubt, machtype should be M_OTHER)
*/
char machtype[]= {
	M_OTHER,M_OTHER,M_OTHER,0,0,	/* a,b,c,d,e */
	0, 0, 0, 0, 0, 			/* f,g,h,i,j */
	0, 0, 0, 0, 0,			/* k,l,m,n,o */
	0, 0, 0, 0, 0,			/* p,q,r,s,t */
	0, 0, 0, 0, 0,			/* u,v,w,x,y */
	0, 0};				/* z */

/* this is basically the default machine for each local machine */
char remtable[] = {
	'b','a','a',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,000,000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* bad login names */
struct bstruct btable[] = {
	0,0 };
/* this table shows the correspondence between
   machine names like 'Cory' and their internal
   names, like 'y' */
static struct tt {
	char *bigname;
	char lname;
	} table[] = {
	"Graphics",	'a',
	"TP",		'b',
	"VAX",		'c',
	0, 		0
	};
/* end of Rand definitions */

# endif

# ifdef NOSC
/* Naval Ocean Systems Center */

/* atts (a) */
char configA[] = {		/* to get to i, config[i] */
	'a',000,'m',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,'m',000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* ccmm (c) */
char configC[] = {		/* to get to i, config[i] */
	'm',000,'c',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,'m',000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* mssf (m) */
char configM[] = {		/* to get to i, config[i] */
	'a',000,'c',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,'m',000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
	000,0				/* z */
	};
/* if machtype is
	M_CC		netlpr will do lpr w/o an acct.
			Will pre-encrypt the password.
	M_INGRES	will allow higher file lengths.
	M_OTHER		will give no unusual effects.
(when in doubt, machtype should be M_OTHER)
*/
char machtype[]= {
	M_OTHER, 0, M_OTHER, 0, 0,	/* a,b,c,d,e */
	0, 0, 0, 0, 0,			/* f,g,h,i,j */
	0, 0, M_OTHER, 0, 0,		/* k,l,m,n,o */
	0, 0, 0, 0, 0,			/* p,q,r,s,t */
	0, 0, 0, 0, 0,			/* u,v,w,x,y */
	0, 0};				/* z */

/* this is basically the default machine for each local machine */
char remtable[] = {
	'm',000,'m',000,000,		/* a,b,c,d,e */
	000,000,000,000,000,		/* f,g,h,i,j */
	000,000,'a',000,000,		/* k,l,m,n,o */
	000,000,000,000,000,		/* p,q,r,s,t */
	000,000,000,000,000,		/* u,v,w,x,y */
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
	"atts",		'a',
	"ccmm",		'c',
	"mssf",		'm',
	0,		0
	};
# endif

# ifdef BERKELEY
/* Berkeley definitions */

/* Computer Center A Machine (A) */
char configA[] = {		/* to get to i, config[i] */
	'a','c','c','c','c',		/* a,b,c,d,e */
	'c',000,000,'c','c',		/* f,g,h,i,j */
	'c','c','c',000,'c',		/* k,l,m,n,o */
	000,'c','c','c','c',		/* p,q,r,s,t */
	000,'c',000,'c','c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center B Machine (B) */
char configB[] = {		/* to get to i, config[i] */
	'd','b','d','d','d',		/* a,b,c,d,e */
	'd',000,000,'d','d',		/* f,g,h,i,j */
	'd','d','d',000,'d',		/* k,l,m,n,o */
	000,'d','d','d','d',		/* p,q,r,s,t */
	000,'d',000,'d','d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Computer Center C Machine (C) */
char configC[] = {		/* to get to i, config[i] */
	'a','d','c','d','e',		/* a,b,c,d,e */
	'd',000,000,'y','y',		/* f,g,h,i,j */
	'e','y','y',000,'y',		/* k,l,m,n,o */
	000,'e','y','d','e',		/* p,q,r,s,t */
	000,'y',000,'y','y',		/* u,v,w,x,y */
	'y',0				/* z */
	};
/* Computer Center D Machine (D) */
char configD[] = {		/* to get to i, config[i] */
	'c','b','c','d','c',		/* a,b,c,d,e */
	'f',000,000,'c','c',		/* f,g,h,i,j */
	'c','c','c',000,'c',		/* k,l,m,n,o */
	000,'c','c','s','c',		/* p,q,r,s,t */
	000,'c',000,'c','c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center E Machine (E) */
char configE[] = {		/* to get to i, config[i] */
	'c','c','c','c','e',		/* a,b,c,d,e */
	'c',000,000,'c','c',		/* f,g,h,i,j */
	'k','c','c',000,'c',		/* k,l,m,n,o */
	000,'q','c','c','t',		/* p,q,r,s,t */
	000,'c',000,'c','c',		/* u,v,w,x,y */
	'c',0				/* z */
	};
/* Computer Center F Machine (F) */
char configF[] = {		/* to get to i, config[i] */
	'd','d','d','d','d',		/* a,b,c,d,e */
	'f',000,000,'d','d',		/* f,g,h,i,j */
	'd','d','d',000,'d',		/* k,l,m,n,o */
	000,'d','d','d','d',		/* p,q,r,s,t */
	000,'d',000,'d','d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Project INGRES 11/70 (Ing70) */
char configI[] = {		/* to get to i, config[i] */
	'v','v','v','v','v',		/* a,b,c,d,e */
	'v',000,000,'i','j',		/* f,g,h,i,j */
	'v','v','v',000,'v',		/* k,l,m,n,o */
	000,'v','v','v','v',		/* p,q,r,s,t */
	000,'v',000,'v','v',		/* u,v,w,x,y */
	'v',0				/* z */
	};
/* Project INGRES VAX (IngVAX) */
char configJ[] = {		/* to get to i, config[i] */
	'i','i','i','i','i',		/* a,b,c,d,e */
	'i',000,000,'i','j',		/* f,g,h,i,j */
	'j','i','i',000,'i',		/* k,l,m,n,o */
	000,'i','i','i','i',		/* p,q,r,s,t */
	000,'i',000,'i','i',		/* u,v,w,x,y */
	'i',0				/* z */
	};
/* Biochemistry (Virus) PDP-11/40 Running V7 */
char configK[] = {		/* to get to i, config[i] */
	'e','e','e','e','e',		/* a,b,c,d,e */
	'e',000,000,'e','e',		/* f,g,h,i,j */
	'k','e','e',000,'e',		/* k,l,m,n,o */
	000,'e','e','e','e',		/* p,q,r,s,t */
	000,'e',000,'e','e',		/* u,v,w,x,y */
	'e',0				/* z */
	};
/* Brodersen-Newton VLSI-CAD VAX (VLSI) */
char configL[] = {		/* to get to i, config[i] */
	'm','m','m','m','m',		/* a,b,c,d,e */
	'm',000,000,'m','m',		/* f,g,h,i,j */
	'm','l','m',000,'m',		/* k,l,m,n,o */
	000,'m','m','m','m',		/* p,q,r,s,t */
	000,'m',000,000,'m',		/* u,v,w,x,y */
	'm',0				/* z */
	};
/* Sakrison's Image Project 11/40 (Image) */
char configM[] = {		/* to get to i, config[i] */
	'o','o','o','o','o',		/* a,b,c,d,e */
	'o',000,000,'o','o',		/* f,g,h,i,j */
	'o','l','m',000,'o',		/* k,l,m,n,o */
	000,'o','o','o','o',		/* p,q,r,s,t */
	000,'o',000,'o','o',		/* u,v,w,x,y */
	'o',0				/* z */
	};
/* Pfeister - Pollack - Sangiovanni Optimization VAX (ESVAX) */
char configO[] = {		/* to get to i, config[i] */
	'y','y','y','y','y',		/* a,b,c,d,e */
	'y',000,000,'y','y',		/* f,g,h,i,j */
	'y','m','m',000,'o',		/* k,l,m,n,o */
	000,'y','y','y','y',		/* p,q,r,s,t */
	000,'y',000,'y','y',		/* u,v,w,x,y */
	'z',0				/* z */
	};
/* Computer Center Q Machine (Q) */
char configQ[] = {		/* to get to i, config[i] */
	'e','e','e','e','e',		/* a,b,c,d,e */
	'e',000,000,'e','e',		/* f,g,h,i,j */
	'e','e','e',000,'e',		/* k,l,m,n,o */
	000,'q','e','e','e',		/* p,q,r,s,t */
	000,'e',000,'e','e',		/* u,v,w,x,y */
	'e',0				/* z */
	};
/* Fabry's ARPA support VAX - ARPAVAX */
char configR[] = {		/* to get to i, config[i] */
	'v','v','v','v','v',		/* a,b,c,d,e */
	'v',000,000,'v','v',		/* f,g,h,i,j */
	'v','v','v',000,'v',		/* k,l,m,n,o */
	000,'v','r','v','v',		/* p,q,r,s,t */
	000,'v',000,'v','v',		/* u,v,w,x,y */
	'v',0				/* z */
	};
/* Survey Research Center 11/40 (SRC) */
char configS[] = {		/* to get to i, config[i] */
	'd','d','d','d','d',		/* a,b,c,d,e */
	'd',000,000,'d','d',		/* f,g,h,i,j */
	'd','d','d',000,'d',		/* k,l,m,n,o */
	000,'d','d','s','d',		/* p,q,r,s,t */
	000,'d',000,'d','d',		/* u,v,w,x,y */
	'd',0				/* z */
	};
/* Math-Stat Departement machine 11-45 (MathStat) */
char configT[] = {		/* to get to i, config[i] */
	'y','y','y','y','y',		/* a,b,c,d,e */
	'y',000,000,'y','y',		/* f,g,h,i,j */
	'y','y','y',000,'y',		/* k,l,m,n,o */
	000,'y','y','y','t',		/* p,q,r,s,t */
	000,'y',000,'y','y',		/* u,v,w,x,y */
	'y',0				/* z */
	};
/* EECS Research (Fateman - Ernie) VAX (CSVAX) */
char configV[] = {		/* to get to i, config[i] */
	'y','y','y','y','y',		/* a,b,c,d,e */
	'y',000,000,'i','i',		/* f,g,h,i,j */
	'y','y','y',000,'y',		/* k,l,m,n,o */
	000,'y','r','y','y',		/* p,q,r,s,t */
	000,'v',000,'x','y',		/* u,v,w,x,y */
	'y',0				/* z */
	};
/* CS Research Onyx Computer */
char configX[] = {		/* to get to i, config[i] */
	'v','v','v','v','v',		/* a,b,c,d,e */
	'v',000,000,'v','v',		/* f,g,h,i,j */
	'v','v','v',000,'v',		/* k,l,m,n,o */
	000,'v','v','v','v',		/* p,q,r,s,t */
	000,'v',000,'x','v',		/* u,v,w,x,y */
	'v',0				/* z */
	};
/* EECS Instructional 11/70 (199 Cory) (Cory) */
char configY[] = {		/* to get to i, config[i] */
	'c','c','c','c','c',		/* a,b,c,d,e */
	'c',000,000,'v','v',		/* f,g,h,i,j */
	'c','o','o',000,'o',		/* k,l,m,n,o */
	000,'c','v','c','c',		/* p,q,r,s,t */
	000,'v',000,'v','y',		/* u,v,w,x,y */
	'o',0				/* z */
	};
/* EECS Departmental 11/40  (EECS40) */
char configZ[] = {		/* to get to i, config[i] */
	'o','o','o','o','o',		/* a,b,c,d,e */
	'o',000,000,'o','o',		/* f,g,h,i,j */
	'o','o','o',000,'o',		/* k,l,m,n,o */
	000,'o','o','o','o',		/* p,q,r,s,t */
	000,'o',000,'o','o',		/* u,v,w,x,y */
	'z',0				/* z */
	};
/* if machtype is
	M_CC		netlpr will do lpr w/o an acct.
			Will pre-encrypt the password.
	M_INGRES	will allow higher file lengths.
	M_OTHER		will give no unusual effects.
(when in doubt, machtype should be M_OTHER)
*/
char machtype[]= {
	M_CC, M_CC, M_CC, M_CC, M_CC,		/* a,b,c,d,e */
	M_CC, 0, 0, M_INGRES, M_INGRES,		/* f,g,h,i,j */
	M_OTHER, M_OTHER, M_OTHER, 0, M_OTHER,	/* k,l,m,n,o */
	0, M_CC, M_OTHER, M_OTHER, M_OTHER,	/* p,q,r,s,t */
	0, M_OTHER, 0, M_OTHER, M_OTHER,		/* u,v,w,x,y */
	M_OTHER, 0};				/* z */

/* this is basically the default machine for each local machine */
char remtable[] = {
	'c','d','a','c','c',		/* a,b,c,d,e */
	'd',000,000,'j','i',		/* f,g,h,i,j */
	'e','m','o',000,'v',		/* k,l,m,n,o */
	000,'e','v','d','e',		/* p,q,r,s,t */
	000,'y',000,'v','v',		/* u,v,w,x,y */
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
	"Ing70",	'i',
	"Ingres",	'i',
	"IngVAX",	'j',
	"Virus",	'k',
	"VLSI",		'l',
	"Image",	'm',
	"ESVAX",	'o',
	"OptVAX",	'o',
	"Q",		'q',
	"ARPAVAX",	'r',
	"SRC",		's',
	"MathStat",	't',
	"CSVAX",	'v',
	"VAX",		'v',
	"Onyx",		'x',
	"Cory",		'y',
	"EECS40", 	'z',
	0, 		0
	};
# endif
