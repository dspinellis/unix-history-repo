static char Sccsid[] = "aplcvt.c @(#)aplcvt.c	1.2	10/1/82 Berkeley ";
#

/*
 *	aplcvt - convert APL workspace to/from VAX format
 */

#include <stdio.h>

#define PDPMAGIC 0100554		/* PDP-11 magic number */
#define	VAXMAGIC 0100556		/* VAX magic number */

#define	DA	1			/* data type */
#define	NF	8			/* niladic function type */
#define	MF	9			/* monadic function type */
#define	DF	10			/* dyadic function type */
#define	MRANK	8			/* maximum rank */

/*
 *	The following define the internal data structures for APL
 *	on both the PDP-11 and the VAX.  Two short integers are
 *	used instead of a long integer for the VAX definitions so
 *	that the program can be compiled and run on either machine
 *	without changes.  (Otherwise, the reversal of long integers
 *	between the two machines would cause problems.)
 */

struct pdp_thread {
	double	pt_fuzz;
	short	pt_iorg;
	short	pt_rl;
	short	pt_digits;
	short	pt_width;
} pthread;
#define	PTSIZE	14		/* its real size, not the sizeof */

struct vax_thread {
	double	vt_fuzz;
	short	vt_iorg[2];
	short	vt_rl[2];
	short	vt_digits[2];
	short	vt_width[2];
} vthread;


struct pdp_item {
	char	pi_rank;
	char	pi_type;
	short	pi_size;
	short	pi_index;
	short	pi_datap;	/* really a 16-bit pointer */
	short	pi_dim[MRANK];
} pitem;

struct vax_item {
	char	vi_rank;
	char	vi_type;
	char	vi_pad[2];
	short	vi_size[2];
	short	vi_index[2];
	short	vi_datap[2];	/* really a 32-bit pointer */
	short	vi_dim[MRANK][2];	/* array of 32-bit integers */
} vitem;

union uci {
	char	cv[4];
	unsigned short s;
};

#define	eperror(x,y)	{eprintf(x); perror(y);}
char *base(), *strcpy(), *strcmp();

#ifdef vax
int makevax = 1;		/* by default, convert to VAX format */
#else
int makevax = 0;		/* by default, convert to PDP format */
#endif

char *pname;			/* holds argv[0] */
char *ifname;			/* points to input file name */
char ofname[128];		/* contains output file name */

main(argc, argv)
char **argv;
{
	register FILE *ifp, *ofp;
	register char **ap;

	/* Parse the arguments */

	pname = *argv;
	ap = argv+1;
	if (argc > 1 && *argv[1] == '-'){
		switch(argv[1][1]){
		case 'v':
		case 'p':
			makevax = (argv[1][1] == 'v');
			break;
		default:
			eprintf("unknown flag \"%s\"\n", argv[1]);
			exit(1);
		}
		ap++;
	}


	/* If there are no filename arguments, convert standard
	 * input to standard output.  However, if one of these is
	 * a tty, just exit with a syntax error message (it is highly
	 * unlikely that the user wanted input or output from/to his
	 * tty.
	 *
	 * If there are filenames, convert each one.
	 */

	if (!*ap){
		if(isatty(0) || isatty(1)){
			fprintf(stderr, "Syntax: \"%s [-v|-p] filename ...\"\n",
			    pname);
			exit(1);
		}
		ifname = "<stdin>";
		strcpy(ofname, "<stdout>");
		if (makevax ? tovax(stdin,stdout) : topdp(stdin,stdout)){
			eprintf("don't trust the output file!\n");
			exit(1);
		}
	} else
		for(; *ap; ap++){
			ifname = *ap;
			if ((ifp=fopen(ifname, "r")) == NULL){
				eperror("can't open ", ifname);
				continue;
			}
			strcat(strcpy(ofname,base(ifname)),
			    makevax ? ".vax" : ".pdp");
			if ((ofp=fopen(ofname, "w")) == NULL){
				eperror("can't create ", ofname);
				fclose(ifp);
				continue;
			}
			if (makevax ? tovax(ifp,ofp) : topdp(ifp,ofp))
				if (unlink(ofname) < 0)
					eperror("unlink ", ofname);
			fclose(ifp);
			fclose(ofp);
		}

	exit(0);
}

char *
base(s)
register char *s;
{
	static char basename[128];
	register char *p;

	/* Strip off a trailing ".pdp" or ".vax" (depending upon the
	 * direction of conversion.
	 */

	for(p=basename; *p = *s; p++,s++)
		if (*s == '.' && !strcmp(s+1, makevax ? "pdp" : "vax")){
			*p = '\0';
			break;
		}

	return(basename);
}

topdp(ifp, ofp)
FILE *ifp, *ofp;
{
	unsigned short magic;
	short nsz;
	union uci iz;
	char name[128];
	register c;
	register j;

	/* Look for proper magic number */

	if (fread(&magic, sizeof magic, 1, ifp) != 1){
		eperror("read error on ", ifname);
		return(-1);
	}

	if ((magic|1) != (VAXMAGIC|1)){
		eprintf("%s is not a VAX APL workspace\n", ifname);
		return(-1);
	}

	if (fread(&magic, sizeof magic, 1, ifp) != 1){
		eperror("read error on ", ifname);
		return(-1);
	}

	if (magic){
		eprintf("warning: %s may be corrupted\n", ifname);
		eprintf("attempting to continue\n");
	}

	magic = (magic&1) | PDPMAGIC;
	if (fwrite(&magic, sizeof magic, 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}


	/* Convert the "thread" structure */

	if (fread(&vthread, sizeof vthread, 1, ifp) != 1){
		eperror("read error on ", ifname);
		return(-1);
	}

	pthread.pt_fuzz = vthread.vt_fuzz;
	pthread.pt_iorg = vthread.vt_iorg[0];
	pthread.pt_rl = vthread.vt_rl[0];
	pthread.pt_digits = vthread.vt_digits[0];
	pthread.pt_width = vthread.vt_width[0];

	if (fwrite(&pthread, PTSIZE, 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}


	/* Convert each data item or function */

loop:
	if ((j=fread(&iz, sizeof(long), 1, ifp)) != 1)
		if (j <= 0)
			return(0);
		else {
			eperror("read error on ", ifname);
			return(-1);
		}
	if (fwrite(&iz, sizeof(short), 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}

	if (fread(name, sizeof(char), (unsigned)iz.cv[1], ifp) != iz.cv[1]){
		eperror("read error on ", ifname);
		return(-1);
	}
	if (fwrite(name, sizeof(char), (unsigned)iz.cv[1], ofp) != iz.cv[1]){
		eperror("write error on ", ofname);
		return(-1);
	}

	switch(iz.cv[0]){
	default:
		eprintf("unknown item, type = %d\n", iz.cv[0]);
		eprintf("conversion aborted\n");
		return(-1);

	case NF:
	case MF:
	case DF:
		do {
			if ((c=getc(ifp)) == EOF){
				eperror("getc error on ", ifname);
				return(-1);
			}
			putc(c, ofp);
		} while (c);
		break;

	case DA:
		if (fread(&iz, sizeof(long), 1, ifp) != 1){
			eperror("read error on ", ifname);
			return(-1);
		}
		if (iz.cv[2] | iz.cv[3]){
			eprintf("item %s too large -- aborting\n", name);
			return(-1);
		}
		if (fread(&vitem, sizeof vitem - MRANK*sizeof(long),
		    1, ifp) != 1){
			eperror("read error on ", ifname);
			return(-1);
		}
		if (fread(vitem.vi_dim, sizeof(long), vitem.vi_rank, ifp)
		    != vitem.vi_rank){
			eperror("read error on ", ifname);
			return(-1);
		}
		pitem.pi_rank = vitem.vi_rank;
		pitem.pi_type = vitem.vi_type;
		pitem.pi_size = vitem.vi_size[0];
		for(j=0; j<vitem.vi_rank; j++)
			pitem.pi_dim[j] = vitem.vi_dim[j][0];
		nsz = sizeof pitem - (MRANK-pitem.pi_rank)*sizeof(short)
		    - sizeof vitem + (MRANK-vitem.vi_rank)*sizeof(long)
		    + iz.s;
		if (fwrite(&nsz, sizeof nsz, 1, ofp) != 1){
			eperror("write error on ", ofname);
			return(-1);
		}
		j = sizeof pitem - (MRANK-pitem.pi_rank)*sizeof(short);
		if (fwrite(&pitem, j, 1, ofp) != 1){
			eperror("write error on ", ofname);
			return(-1);
		}
		j = sizeof vitem - (MRANK-vitem.vi_rank)*sizeof(long);
		if (copy(ifp, ofp, iz.s-j))
			return(-1);
	}

	goto loop;	/* should be while(1) */
}

tovax(ifp, ofp)
FILE *ifp, *ofp;
{
	unsigned short magic;
	static short zero = 0;
	short nsz;
	union uci iz;
	char name[128];
	register c;
	register j;

	/* Look for proper magic number. */

	if (fread(&magic, sizeof magic, 1, ifp) != 1){
		eperror("read error on ", ifname);
		return(-1);
	}

	if ((magic|1) != (PDPMAGIC|1)){
		eprintf("%s is not a PDP-11 APL workspace\n", ifname);
		return(-1);
	}

	magic = (magic&1) | VAXMAGIC;
	if (fwrite(&magic, sizeof magic, 1, ofp) != 1
	    || fwrite(&zero, sizeof zero, 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}


	/* Convert the "thread" structure. */

	if (fread(&pthread, PTSIZE, 1, ifp) != 1){
		eperror("read error on ", ifname);
		return(-1);
	}

	vthread.vt_fuzz = pthread.pt_fuzz;
	vthread.vt_iorg[0] = pthread.pt_iorg;
	vthread.vt_iorg[1] = 0;
	vthread.vt_rl[0] = pthread.pt_rl;
	vthread.vt_rl[1] = 0;
	vthread.vt_digits[0] = pthread.pt_digits;
	vthread.vt_digits[1] = 0;
	vthread.vt_width[0] = pthread.pt_width;
	vthread.vt_width[1] = 0;

	if (fwrite(&vthread, sizeof vthread, 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}


	/* Convert each data item or function. */

loop:
	if ((j=fread(&iz, sizeof(short), 1, ifp)) != 1)
		if (j <= 0)
			return(0);
		else {
			eperror("read error on ", ifname);
			return(-1);
		}
	iz.cv[2] = iz.cv[3] = 0;
	if (fwrite(&iz, sizeof(long), 1, ofp) != 1){
		eperror("write error on ", ofname);
		return(-1);
	}

	if (fread(name, sizeof(char), (unsigned)iz.cv[1], ifp) != iz.cv[1]){
		eperror("read error on ", ifname);
		return(-1);
	}
	if (fwrite(name, sizeof(char), (unsigned)iz.cv[1], ofp) != iz.cv[1]){
		eperror("write error on ", ofname);
		return(-1);
	}

	switch(iz.cv[0]){
	default:
		eprintf("unknown item, type = %d\n", iz.cv[0]);
		eprintf("conversion aborted\n");
		return(-1);

	case NF:
	case MF:
	case DF:
		do {
			if ((c=getc(ifp)) == EOF){
				eperror("getc error on ", ifname);
				return(-1);
			}
			putc(c, ofp);
		} while (c);
		break;

	case DA:
		if (fread(&iz, sizeof(short), 1, ifp) != 1){
			eperror("read error on ", ifname);
			return(-1);
		}
		if (fread(&pitem, sizeof pitem - MRANK*sizeof(short),
		    1, ifp) != 1){
			eperror("read error on ", ifname);
			return(-1);
		}
		if (fread(pitem.pi_dim, sizeof(short), pitem.pi_rank, ifp)
		    != pitem.pi_rank){
			eperror("read error on ", ifname);
			return(-1);
		}
		vitem.vi_rank = pitem.pi_rank;
		vitem.vi_type = pitem.pi_type;
		vitem.vi_size[0] = pitem.pi_size;
		vitem.vi_size[1] = 0;
		for(j=0; j<pitem.pi_rank; j++){
			vitem.vi_dim[j][0] = pitem.pi_dim[j];
			vitem.vi_dim[j][1] = 0;
		}
		nsz = sizeof vitem - (MRANK-vitem.vi_rank)*sizeof(long)
		    - sizeof pitem + (MRANK-pitem.pi_rank)*sizeof(short)
		    + iz.s;
		if (fwrite(&nsz, sizeof nsz, 1, ofp) != 1
		    || fwrite(&zero, sizeof zero, 1, ofp) != 1){
			perror("write error on ", ofname);
			return(-1);
		}
		j = sizeof vitem - (MRANK-vitem.vi_rank)*sizeof(long);
		if (fwrite(&vitem, j, 1, ofp) != 1){
			eperror("write error on ", ofname);
			return(-1);
		}
		j = sizeof pitem - (MRANK-pitem.pi_rank)*sizeof(short);
		if (copy(ifp, ofp, iz.s-j))
			return(-1);
	}

	goto loop;	/* should be while(1) */
}

copy(ifp, ofp, len)
FILE *ifp, *ofp;
register len;
{
	register c;

	while(len--){
		if ((c=getc(ifp)) == EOF){
			eperror("getc error on ", ifname);
			return(-1);
		}
		putc(c, ofp);
	}
	return(0);
}

/*VARARGS 1*/
eprintf(a, b, c, d, e, f, g, h, i, j){

	fprintf(stderr, "%s: ", pname);
	fprintf(stderr, a, b, c, d, e, f, g, h, i, j);
}
