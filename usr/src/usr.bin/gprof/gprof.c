#ifndef lint
    static	char *sccsid = "@(#)gprof.c	1.2 (Berkeley) %G%";
#endif lint

#include "gprof.h"

main(argc, argv)
	int argc;
	char **argv;
{

    --argc;
    argv++;
    debug = 0;
    while ( *argv != 0 && **argv == '-' ) {
	(*argv)++;
	if ( **argv == 'd' ) {
	    (*argv)++;
	    debug |= atoi( *argv );
	    debug |= ANYDEBUG;
#	    ifdef DEBUG
		printf( "[main] debug = %d\n" , debug );
#	    endif DEBUG
	} else if ( **argv == 'z' ) {
	    zflg++;
	}
	argv++;
    }
    if ( *argv != 0 ) {
	a_outname  = *argv;
	argv++;
    } else {
	a_outname  = A_OUTNAME;
    }
    if ( *argv != 0 ) {
	gmonname = *argv;
	argv++;
    } else {
	gmonname = GMONNAME;
    }
	/*
	 *	get information about a.out file.
	 */
    getnfile();
	/*
	 *	get information about mon.out file(s).
	 */
    getpfile( gmonname );
	/*
	 *	assign samples to procedures
	 */
    asgnsamples();
	/*
	 *	print the usual profile
	 */
    printprof();	
	/*
	 *	assemble and print the dynamic profile
	 */
    doarcs();
    done();
}

printprof()
{
    register nltype	*np;
    nltype		**sortednlp;
    int			index;

    actime = 0.0;
    putprofheader();
	/*
	 *	Sort the symbol table in by time
	 */
    sortednlp = (nltype **) calloc( nname , sizeof(nltype *) );
    if ( sortednlp == (nltype **) 0 ) {
	fprintf( stderr , "[printprof] ran out of memory for time sorting\n" );
    }
    for ( index = 0 ; index < nname ; index += 1 ) {
	sortednlp[ index ] = &nl[ index ];
    }
    qsort( sortednlp , nname , sizeof(nltype *) , timecmp );
    for ( index = 0 ; index < nname ; index += 1 ) {
	np = sortednlp[ index ];
	putprofline( np , 1 );
    }
    actime = 0.0;
    printf( "\ngranularity: each sample hit covers %.1f bytes" , scale );
    printf( " for %.2f%% of %.2f seconds\n" , 100.0/totime , totime / HZ );
}

putprofline( np , cumflag )
    register nltype	*np;
    int			cumflag;
{
    double	time;
    long	calls = np -> ncall + np -> selfcalls;

    if ( zflg == 0 && calls == 0 && np -> time == 0 && np -> childtime == 0 ) {
	return;
    }
    if ( cumflag ) {
	time = (np->time + np->childtime) / totime;
	actime += np->time;
	if ( np -> index != 0 ) {
	    printf( "[%d]" , np -> index );
	}
	printf( "\t%5.1f %7.1f" , 100 * time , actime / HZ );
    } else {
	printf( "\t%5.5s %7.7s" , "" , "" );
    }
    printf( " %7.1f", np -> time / HZ );
    if ( np -> childtime != 0.0 ) {
	printf( " %7.1f" , np -> childtime / HZ );
    } else {
	printf( " %7.7s" , "" );
    }
    if ( calls != 0 ) {
	printf( " %7d" , np -> ncall );
	if ( np -> selfcalls != 0 ) {
	    printf( "+%-7d  " , np -> selfcalls );
	} else {
	    printf( " %7.7s  " , "" );
	}
    } else {
	printf( " %7.7s %7.7s  " , "" , "" );
    }
    if ( ! cumflag ) {
	printf( "    " );
    }
    printname( np );
    printf( "\n" );
}

    /*
     *	header for putprofline
     */
putprofheader()
{
    
    printf( "\n\t%5.5s %7.7s %-7.7s %-7.7s %7.7s %7.7s %5.5s\n" ,
	    "%time" , "cumsecs" , "  self" , " child" , "ncall" , "" , "name" );
}

/*
 * Set up string and symbol tables from a.out.
 * On return symbol table is sorted by value.
 */
getnfile()
{
    FILE	*nfile;

    nfile = fopen( a_outname ,"r");
    if (nfile == NULL) {
	perror( a_outname );
	done();
    }
    fread(&xbuf, 1, sizeof(xbuf), nfile);
    if (N_BADMAG(xbuf)) {
	fprintf(stderr, "%s: bad format\n", a_outname );
	done();
    }
    getstrtab(nfile);
    getsymtab(nfile);
    qsort(nl, nname, sizeof(nltype), valcmp);
    fclose(nfile);
#   ifdef DEBUG
	if ( debug & AOUTDEBUG ) {
	    register int j;

	    for (j = 0; j < nname; j++){
		printf("[getnfile] 0X%08x\t%s\n", nl[j].value, nl[j].name);
	    }
	}
#   endif DEBUG
}

getstrtab(nfile)
    FILE	*nfile;
{

    fseek(nfile, (long)(N_SYMOFF(xbuf) + xbuf.a_syms), 0);
    if (fread(&ssiz, sizeof (ssiz), 1, nfile) == 0) {
	fprintf(stderr, "%s: no string table (old format?)\n", a_outname );
	done();
    }
    strtab = (char *)calloc(ssiz, 1);
    if (strtab == NULL) {
	fprintf(stderr, "%s: no room for %d bytes of string table",
		a_outname , ssiz);
	done();
    }
    if (fread(strtab+sizeof(ssiz), ssiz-sizeof(ssiz), 1, nfile) != 1) {
	fprintf(stderr, "%s: error reading string table\n", a_outname );
	done();
    }
}

    /*
     * Read in symbol table
     */
getsymtab(nfile)
    FILE	*nfile;
{
    register long	i;
    int			askfor;
    struct nlist	nbuf;

    /* pass1 - count symbols */
    fseek(nfile, (long)N_SYMOFF(xbuf), 0);
    nname = 0;
    for (i = xbuf.a_syms; i > 0; i -= sizeof(struct nlist)) {
	fread(&nbuf, sizeof(nbuf), 1, nfile);
	if ( nbuf.n_type != N_TEXT+N_EXT ) {
	    continue;
	}
	nname++;
    }
    if (nname == 0) {
	fprintf(stderr, "%s: no symbols\n", a_outname );
	done();
    }
	/*
	 *	ask also for CYCLEFRACTION extra namelist entries for 
	 *	cycle entries.  these hide out at the end of the namelist
	 *	and aren't accessed unless the whole namelist (nname+ncycles)
	 *	is sorted and searched.
	 */
    ncycles = nname * CYCLEFRACTION;
    askfor = nname + 1 + ncycles;
    nl = (nltype *) calloc( askfor , sizeof(nltype) );
    if (nl == 0) {
	fprintf(stderr, "prof: No room for %d bytes of symbol table\n",
		askfor * sizeof(nltype) );
	done();
    }

    /* pass2 - read symbols */
    fseek(nfile, (long)N_SYMOFF(xbuf), 0);
    npe = nl;
    nname = 0;
    for (i = xbuf.a_syms; i > 0; i -= sizeof(struct nlist)) {
	fread(&nbuf, sizeof(nbuf), 1, nfile);
	if ( nbuf.n_type != N_TEXT+N_EXT ) {
	    continue;
	}
	npe->value = nbuf.n_value;
	npe->name = strtab+nbuf.n_un.n_strx;
#	ifdef DEBUG
	    if ( debug & AOUTDEBUG ) {
		printf( "[getsymtab] %d %s 0x%08x\n" ,
			nname , npe -> name , npe -> value );
	    }
#	endif DEBUG
	npe++;
	nname++;
    }
    npe->value = -1;
    npe++;
}

    /*
     *	information from a gmon.out file is in two parts:
     *	an array of sampling hits within pc ranges,
     *	and the arcs.
     */
getpfile(filename)
    char *filename;
{
    FILE		*pfile;
    FILE		*openpfile();
    struct rawarc	arc;

    pfile = openpfile(filename);
    readsamples(pfile);
	/*
	 *	the rest of the file consists of
	 *	a bunch of <from,self,count> tuples.
	 */
    while ( fread( &arc , sizeof arc , 1 , pfile ) == 1 ) {
#	ifdef DEBUG
	    if ( debug & SAMPLEDEBUG ) {
		printf( "[getpfile] frompc %d selfpc %d count %d\n" ,
			arc.raw_frompc , arc.raw_selfpc , arc.raw_count );
	    }
#	endif DEBUG
	    /*
	     *	add this arc
	     */
	tally( &arc );
    }
    fclose(pfile);
}

FILE *openpfile(filename)
    char *filename;
{
    FILE	*pfile;

    if((pfile = fopen(filename, "r")) == NULL) {
	perror(filename);
	done();
    }
    fread(&h, sizeof(struct hdr), 1, pfile);
    lowpc = h.lowpc - (UNIT *)0;
    highpc = h.highpc - (UNIT *)0;
    sampbytes = h.ncnt - sizeof(struct hdr);
    nsamples = sampbytes / sizeof (unsigned UNIT);
    return(pfile);
}

tally( rawp )
    struct rawarc	*rawp;
{
    nltype		*parentp;
    nltype		*childp;
    arctype		*arcp;
    arctype		*malloc();

    parentp = nllookup( rawp -> raw_frompc );
    childp = nllookup( rawp -> raw_selfpc );
    childp -> ncall += rawp -> raw_count;
#   ifdef DEBUG
	if ( debug & TALLYDEBUG ) {
	    printf( "[tally] arc from %s to %s traversed %d times\n" ,
		    parentp -> name , childp -> name , rawp -> raw_count );
	}
#   endif DEBUG
    arcp = arclookup( parentp , childp );
    if ( arcp != 0 ) {
	    /*
	     *	a hit:  just increment the count.
	     */
#	ifdef DEBUG
	    if ( debug & TALLYDEBUG ) {
		printf( "[tally] hit %d += %d\n" ,
			arcp -> arc_count , rawp -> raw_count );
	    }
#	endif DEBUG
	arcp -> arc_count += rawp -> raw_count;
	return;
    }
    arcp = malloc( sizeof *arcp );
    arcp -> arc_parentp = parentp;
    arcp -> arc_childp = childp;
    arcp -> arc_count = rawp -> raw_count;
	/*
	 *	prepend this child to the children of this parent
	 */
    arcp -> arc_childlist = parentp -> children;
    parentp -> children = arcp;
	/*
	 *	prepend this parent to the parents of this child
	 */
    arcp -> arc_parentlist = childp -> parents;
    childp -> parents = arcp;
}

valcmp(p1, p2)
    nltype *p1, *p2;
{
    if ( p1 -> value < p2 -> value ) {
	return LESSTHAN;
    }
    if ( p1 -> value > p2 -> value ) {
	return GREATERTHAN;
    }
    return EQUALTO;
}

readsamples(pfile)
    FILE	*pfile;
{
    register i;
    unsigned UNIT	sample;
    
    if (samples == 0) {
	samples = (unsigned UNIT *) calloc(sampbytes, sizeof (unsigned UNIT));
	if (samples == 0) {
	    fprintf( stderr , "prof: No room for %d sample pc's\n", 
		sampbytes / sizeof (unsigned UNIT));
	    done();
	}
    }
    for (i = 0; i < nsamples; i++) {
	fread(&sample, sizeof (unsigned UNIT), 1, pfile);
	if (feof(pfile))
		break;
	samples[i] += sample;
    }
    if (i != nsamples) {
	fprintf(stderr,
	    "prof: unexpected EOF after reading %d/%d samples\n",
		--i, nsamples);
	done();
    }
}

/*
 * Assign samples to the procedures to which they belong.
 */
asgnsamples()
{
    register int	j;
    unsigned UNIT	ccnt;
    double		time;
    unsigned long	pcl, pch;
    register int	i;
    int			overlap;
    unsigned long	svalue0, svalue1;

    /* read samples and assign to namelist symbols */
    scale = highpc - lowpc;
    scale /= nsamples;
    for (i=0; i < nsamples; i++) {
	ccnt = samples[i];
	if (ccnt == 0)
		continue;
	pcl = lowpc + scale*i;
	pch = lowpc + scale*(i+1);
	time = ccnt;
#	ifdef DEBUG
	    if ( debug & SAMPLEDEBUG ) {
		printf( "[asgnsamples] ccnt %d time %f totime %f\n" ,
			ccnt , time , totime );
	    }
#	endif DEBUG
	totime += time;
	for (j=0; j<nname; j++) {
	    svalue0 = nl[j].value / sizeof(UNIT);
	    svalue1 = nl[j+1].value / sizeof(UNIT);
	    if (pch < svalue0)
		    break;
	    if (pcl >= svalue1)
		    continue;
	    overlap=min(pch,svalue1) - max(pcl,svalue0);
	    if (overlap>0) {
#		ifdef DEBUG
		    if ( debug & SAMPLEDEBUG ) {
			printf( "[asgnsamples] %s gets %f ticks\n" ,
				nl[j].name , overlap*time/scale );
		    }
#		endif DEBUG
		nl[j].time += overlap*time/scale;
	    }
	}
    }
#   ifdef DEBUG
	if ( debug & SAMPLEDEBUG ) {
	    printf( "[asgnsamples] totime %f\n" , totime );
	}
#   endif DEBUG
    if (totime==0.0) {
	fprintf( stderr , "No time accumulated\n" );
	totime=1.0;
    }
}


min(a, b)
    unsigned a,b;
{
    if (a<b)
	return(a);
    return(b);
}

max(a, b)
    unsigned a,b;
{
    if (a>b)
	return(a);
    return(b);
}

timecmp( npp1 , npp2 )
    nltype **npp1, **npp2;
{
    double d;

    d = (*npp2)->time - (*npp1)->time;
    if (d > 0.0)
	return(1);
    if (d < 0.0)
	return(-1);
    return(strcmp((*npp1)->name,(*npp2)->name));
}

done()
{

    exit(0);
}
