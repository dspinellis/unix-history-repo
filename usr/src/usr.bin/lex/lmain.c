#ifndef lint
static char sccsid[] = "@(#)lmain.c	4.3 (Berkeley) 2/21/85";
#endif

# include "ldefs.c"
# include "once.c"

	/* lex [-[drcyvntf]] [file] ... [file] */

	/* Copyright 1976, Bell Telephone Laboratories, Inc.,
	    written by Eric Schmidt, August 27, 1976   */

main(argc,argv)
  int argc;
  char **argv; {
	register int i;
# ifdef DEBUG
#include <signal.h>
	signal(SIGBUS,buserr);
	signal(SIGSEGV,segviol);
# endif
	while (argc > 1 && argv[1][0] == '-' ){
		i = 0;
		while(argv[1][++i]){
			switch (argv[1][i]){
# ifdef DEBUG
				case 'd': debug++; break;
				case 'y': yydebug = TRUE; break;
# endif
				case 'r': case 'R':
					ratfor=TRUE; break;
				case 'c': case 'C':
					ratfor=FALSE; break;
				case 't': case 'T':
					fout = stdout;
					errorf = stderr;
					break;
				case 'v': case 'V':
					report = 1;
					break;
				case 'f': case 'F':
					optim = FALSE;
					break;
				case 'n': case 'N':
					report = 0;
					break;
				default:
					warning("Unknown option %c",argv[1][i]);
				}
			}
		argc--;
		argv++;
		}
	sargc = argc;
	sargv = argv;
	if (argc > 1){
		fin = fopen(argv[++fptr], "r");		/* open argv[1] */
		sargc--;
		}
	else fin = stdin;
	if(fin == NULL)
		error ("Can't read input file %s",argc>1?argv[1]:"standard input");
	gch();
		/* may be gotten: def, subs, sname, schar, ccl, dchar */
	get1core();
		/* may be gotten: name, left, right, nullstr, parent */
	scopy("INITIAL",sp);
	sname[0] = sp;
	sp += slength("INITIAL") + 1;
	sname[1] = 0;
	if(yyparse(0)) exit(1);	/* error return code */
		/* may be disposed of: def, subs, dchar */
	free1core();
		/* may be gotten: tmpstat, foll, positions, gotof, nexts, nchar, state, atable, sfall, cpackflg */
	get2core();
	ptail();
	mkmatch();
# ifdef DEBUG
	if(debug) pccl();
# endif
	sect  = ENDSECTION;
	if(tptr>0)cfoll(tptr-1);
# ifdef DEBUG
	if(debug)pfoll();
# endif
	cgoto();
# ifdef DEBUG
	if(debug){
		printf("Print %d states:\n",stnum+1);
		for(i=0;i<=stnum;i++)stprt(i);
		}
# endif
		/* may be disposed of: positions, tmpstat, foll, state, name, left, right, parent, ccl, schar, sname */
		/* may be gotten: verify, advance, stoff */
	free2core();
	get3core();
	layout();
		/* may be disposed of: verify, advance, stoff, nexts, nchar,
			gotof, atable, ccpackflg, sfall */
# ifdef DEBUG
	free3core();
# endif
	if (ZCH>NCH) cname="/usr/lib/lex/ebcform";
	fother = fopen(ratfor?ratname:cname,"r");
	if(fother == NULL)
		error("Lex driver missing, file %s",ratfor?ratname:cname);
	while ( (i=getc(fother)) != EOF)
		putc(i,fout);

	fclose(fother);
	fclose(fout);
	if(
# ifdef DEBUG
		debug   ||
# endif
			report == 1)statistics();
	fclose(stdout);
	fclose(stderr);
	exit(0);	/* success return code */
	}
get1core(){
	register int i, val;
	register char *p;
ccptr =	ccl = myalloc(CCLSIZE,sizeof(*ccl));
pcptr = pchar = myalloc(pchlen, sizeof(*pchar));
	def = (char **) myalloc(DEFSIZE,sizeof(*def));
	subs = (char **) myalloc(DEFSIZE,sizeof(*subs));
dp =	dchar = myalloc(DEFCHAR,sizeof(*dchar));
	sname = (char **) myalloc(STARTSIZE,sizeof(*sname));
sp = 	schar = myalloc(STARTCHAR,sizeof(*schar));
	if(ccl == 0 || def == 0 || subs == 0 || dchar == 0 || sname == 0 || schar == 0)
		error("Too little core to begin");
	}
free1core(){
	cfree(def,DEFSIZE,sizeof(*def));
	cfree(subs,DEFSIZE,sizeof(*subs));
	cfree(dchar,DEFCHAR,sizeof(*dchar));
	}
get2core(){
	register int i, val;
	register char *p;
	gotof = (int *) myalloc(nstates,sizeof(*gotof));
	nexts = (int *) myalloc(ntrans,sizeof(*nexts));
	nchar = myalloc(ntrans,sizeof(*nchar));
	state = (int **) myalloc(nstates,sizeof(*state));
	atable = (int *) myalloc(nstates,sizeof(*atable));
	sfall = (int *) myalloc(nstates,sizeof(*sfall));
	cpackflg = myalloc(nstates,sizeof(*cpackflg));
	tmpstat = myalloc(tptr+1,sizeof(*tmpstat));
	foll = (int **) myalloc(tptr+1,sizeof(*foll));
nxtpos = positions = (int *) myalloc(maxpos,sizeof(*positions));
	if(tmpstat == 0 || foll == 0 || positions == 0 ||
		gotof == 0 || nexts == 0 || nchar == 0 || state == 0 || atable == 0 || sfall == 0 || cpackflg == 0 )
		error("Too little core for state generation");
	for(i=0;i<=tptr;i++)foll[i] = 0;
	}
free2core(){
	cfree(positions,maxpos,sizeof(*positions));
	cfree(tmpstat,tptr+1,sizeof(*tmpstat));
	cfree(foll,tptr+1,sizeof(*foll));
	cfree(name,treesize,sizeof(*name));
	cfree(left,treesize,sizeof(*left));
	cfree(right,treesize,sizeof(*right));
	cfree(parent,treesize,sizeof(*parent));
	cfree(nullstr,treesize,sizeof(*nullstr));
	cfree(state,nstates,sizeof(*state));
	cfree(sname,STARTSIZE,sizeof(*sname));
	cfree(schar,STARTCHAR,sizeof(*schar));
	cfree(ccl,CCLSIZE,sizeof(*ccl));
	}
get3core(){
	register int i, val;
	register char *p;
	verify = (int *) myalloc(outsize,sizeof(*verify));
	advance = (int *) myalloc(outsize,sizeof(*advance));
	stoff = (int *) myalloc(stnum+2,sizeof(*stoff));
	if(verify == 0 || advance == 0 || stoff == 0)
		error("Too little core for final packing");
	}
# ifdef DEBUG
free3core(){
	cfree(advance,outsize,sizeof(*advance));
	cfree(verify,outsize,sizeof(*verify));
	cfree(stoff,stnum+1,sizeof(*stoff));
	cfree(gotof,nstates,sizeof(*gotof));
	cfree(nexts,ntrans,sizeof(*nexts));
	cfree(nchar,ntrans,sizeof(*nchar));
	cfree(atable,nstates,sizeof(*atable));
	cfree(sfall,nstates,sizeof(*sfall));
	cfree(cpackflg,nstates,sizeof(*cpackflg));
	}
# endif
char *myalloc(a,b)
  int a,b; {
	register char *i;
	i = calloc(a, b);
	if(i==0)
		warning("OOPS - calloc returns a 0");
	else if(i == (char *)-1){
# ifdef DEBUG
		warning("calloc returns a -1");
# endif
		return(0);
		}
	return(i);
	}
# ifdef DEBUG
buserr(){
	fflush(errorf);
	fflush(fout);
	fflush(stdout);
	fprintf(errorf,"Bus error\n");
	if(report == 1)statistics();
	fflush(errorf);
	}
segviol(){
	fflush(errorf);
	fflush(fout);
	fflush(stdout);
	fprintf(errorf,"Segmentation violation\n");
	if(report == 1)statistics();
	fflush(errorf);
	}
# endif

yyerror(s)
char *s;
{
	fprintf(stderr, "\"%s\", line %d: %s\n",
		fptr > 0 ? sargv[fptr] : "<stdin>", yyline, s);
}
