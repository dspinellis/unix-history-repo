#ifndef lint
    static	char *sccsid = "@(#)gprof.c	1.5 (Berkeley) %G%";
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
	} else if ( **argv == 'c' ) {
	    cflag++;
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

/*
 * Set up string and symbol tables from a.out.
 *	and optionally the text space.
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
    gettextspace( nfile );
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
     *	read in the text space of an a.out file
     */
gettextspace( nfile )
    FILE	*nfile;
{
    unsigned char	*malloc();
    
    if ( cflag == 0 ) {
	return;
    }
    textspace = malloc( xbuf.a_text );
    if ( textspace == 0 ) {
	fprintf( stderr , "gprof: ran out room for %d bytes of text space:  " );
	fprintf( stderr , "can't do -c\n" , xbuf.a_text );
	return;
    }
    (void) fseek( nfile , N_TXTOFF( xbuf ) , 0 );
    if ( fread( textspace , 1 , xbuf.a_text , nfile ) != xbuf.a_text ) {
	fprintf( stderr , "couldn't read text space:  " );
	fprintf( stderr , "can't do -c\n" , xbuf.a_text );
	free( textspace );
	textspace = 0;
	return;
    }
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
		printf( "[getpfile] frompc 0x%x selfpc 0x%x count %d\n" ,
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

FILE *
openpfile(filename)
    char *filename;
{
    FILE	*pfile;

    if((pfile = fopen(filename, "r")) == NULL) {
	perror(filename);
	done();
    }
    fread(&h, sizeof(struct hdr), 1, pfile);
    s_lowpc = (unsigned long) h.lowpc;
    s_highpc = (unsigned long) h.highpc;
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

    parentp = nllookup( rawp -> raw_frompc );
    childp = nllookup( rawp -> raw_selfpc );
    childp -> ncall += rawp -> raw_count;
#   ifdef DEBUG
	if ( debug & TALLYDEBUG ) {
	    printf( "[tally] arc from %s to %s traversed %d times\n" ,
		    parentp -> name , childp -> name , rawp -> raw_count );
	}
#   endif DEBUG
    addarc( parentp , childp , rawp -> raw_count );
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
    unsigned long	overlap;
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


unsigned long
min(a, b)
    unsigned long a,b;
{
    if (a<b)
	return(a);
    return(b);
}

unsigned long
max(a, b)
    unsigned long a,b;
{
    if (a>b)
	return(a);
    return(b);
}

done()
{

    exit(0);
}
