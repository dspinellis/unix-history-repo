#include <a.out.h>
#include <fcntl.h>
#include <stdio.h>

#define mkioctl(type,value) (0x20000000|('type'<<8)|value)

#define ENPIOGO		mkioctl( S,1 )		/* start the enp */
#define ENPIORESET	mkioctl( S,2 )		/* reset the enp */

#define	ENP0		0x0	/* Enp's 0 Memory		*/
#define	ENP1		0x0	/* Enp's 1 Memory		*/
#define	ENP2		0x0	/* Enp's 2 Memory		*/
#define	ENP3		0x0	/* Enp's 3 Memory		*/

#define	RELO		0x03FFFF	/* ENP->Kernel Mask		*/

#define	BUTTON		0x1000		/* Addr to "push" for startup	*/
#define	JADDR		0x1004		/* Start vector goes here	*/
#define	JUMP		0x8080		/* "jump" command		*/

/*
 *		ENP "device" table (so we can load multiple ENPs)
 *		See ENP/30 User Guide
*/

typedef struct	enp
{
	char	name[8];		/* Device's "name"		*/
	long	mstart;			/* Host's idea of enp mem org	*/
	long	button;			/* Addr of "go" button		*/
	long	jaddr;			/* Addr of "jump" command	*/
} ENP;

ENP	enp_tab[] =
{
	{ "enp0", ENP0, ENP0 + BUTTON, ENP0 + JADDR, },
	{ "enp1", ENP1, ENP1 + BUTTON, ENP1 + JADDR, },
	{ "enp2", ENP2, ENP2 + BUTTON, ENP2 + JADDR, },
	{ "enp3", ENP3, ENP3 + BUTTON, ENP3 + JADDR, },
};
short	num_enp = sizeof(enp_tab) / sizeof(enp_tab[0]);

ENP	*getenp();
short	enpfid = -1;			/* dev "enpram" fid	*/


/*
 *		For loading from a.out files ...
*/

#define	BSIZE		512		/* Size buffer we use	*/
#define	MAXFILE		4		/* Max of 4 a.outs	*/
#define MAXBIN		4		/* bin files on command line */

typedef struct	bins
{
	short	b_fid;
	char	*b_name;
} BINS;

BINS	file_tab[ MAXFILE ];		/* fid/name of a.outs	*/

char	enpdev[] = "/dev/enpXram";
char	buff[ BSIZE ];			/* I/O Buffer		*/
char	zbuf[ BSIZE ];			/* For quick clearing	*/

int	nostart;
int	noload;

main( argc,argv )
int	argc;
char	**argv;
{
	register BINS	*fp;			/* File params		*/
	register short	fcnt,i;			/* Files to load	*/
	struct exec 	hdr;			/* a.out header		*/
	int		cnt;			/* I/O count		*/
	long		f_size,bss_size;	/* text + data & bss	*/

	register ENP	*ep;			/* enp_tab pointer	*/
	unsigned long	enp_go;			/* Start up vector	*/
	short		enp_jump = JUMP;	/* "jump" command	*/
	long		lstart;			/* Start loading at	*/
	long		cstart;			/* Clear from		*/


	if( (argc < 3) || (argc > 3 + MAXBIN) )
	{
		printf( "usage: enpload dev file_1 [ file_2 ... file_4 ]\n" );
		exit( 1 );
	}
	argv++;
	if( (ep = getenp( *argv++ )) == 0 )
	{
		printf( "Bad ENP device name!\n" );
		exit( 1 );
	}

	enpdev[8] = ep->name[3];
	dup2(1,2);	/* redirect sdterr to stdout */
	if( (enpfid = open(enpdev,O_RDWR)) == -1 )
	{
		 sprintf(zbuf, "enpload: Can't open %s ram", ep->name);
	   	 perror( zbuf );
		 exit( 1 );
	}

/*	Collect file names and compute number of them		*/

	fp = file_tab; fcnt = 0;
	while( *argv )
	{
		if( argv[0][0] == '-' )
		{
			if( argv[0][1] == 'L' )		
				noload = 1;
			else
			if( argv[0][1] == 'S' )
				nostart = 1;
			argv++;
		}
		else
		{
			fp->b_name = *argv++;
			fp++; fcnt++;
		}
	}

/*	Zero buffer used to clear bss storage in ENP		*/

	for( i = 0; i < BSIZE; i++ )
	{
		zbuf[i] = 0;
	}

	if( noload )
	{
		printf("Restart %s at 0xf02000\n", ep->name);
		ioctl( enpfid,ENPIOGO,0xf02000 );
		exit( 0 );
	}
	ioctl( enpfid,ENPIORESET,0 );

/*	Open, validate, and load each file_tab[] file		*/

	for( i = 0; i < fcnt; i++ )
	{
		fp = &file_tab[i];
		if( (fp->b_fid = open( fp->b_name,O_RDONLY )) == -1 )
		{
			printf( "enpload: Can't open %s!\n",fp->b_name );
			closem( i-1 );
			exit( 1 );
		}

		if( read( fp->b_fid,&hdr,sizeof( hdr ) ) != sizeof( hdr) )
		{
			printf( "enpload: %s Bad header!\n",fp->b_name );
			closem( i );
			exit( 1 );
		}

		if( N_BADMAG( hdr ) )
		{
			printf( "enpload: %s Bad magic!\n",fp->b_name );
			closem( i );
			exit( 1 );
		}

		f_size = hdr.a_text + hdr.a_data;
		bss_size = hdr.a_bss;
		lstart = (ep->mstart + (hdr.a_entry & RELO)) - 0x1000;
		cstart = lstart + f_size;
			
		printf("Loading %s --- ", enpdev);
		printf( "with file: %s --- ",fp->b_name );
/*
		printf( "text + data: %d  bss: %d\n",f_size,bss_size );
		printf( "ENP's a_entry addr: %06X\n",hdr.a_entry );
		printf( "load start:%06X  bss start:%06X\n",lstart,cstart );

		printf( "Clearing bss ... " );
*/
		lseek( enpfid,cstart,0 );
		while( bss_size >= BSIZE )
		{
			if( write( enpfid,zbuf,BSIZE ) != BSIZE )
				printf("enpload: bss write error\n");
			bss_size -= BSIZE;
		}
		if( bss_size > 0 )
		{
			write( enpfid,zbuf,bss_size );
		}
/*
		printf( "DONE!\n" );
*/

/*
		printf( "Loading ... " );
*/
		lseek( enpfid,lstart,0 );
		while( f_size > BSIZE )
		{
			cnt = read( fp->b_fid,buff,BSIZE );
			f_size -= cnt;
			if( write( enpfid,buff,cnt ) != cnt )
				perror("enpload: write");
		}
		if( f_size > 0 )
		{
			cnt = read( fp->b_fid,buff,f_size );
			write( enpfid,buff,cnt );
		}
		printf( "DONE!\n" );
	}

/*	Last file, we exec this one	*/

	if( nostart == 0 )
	{
		enp_go = hdr.a_entry;
/*
		printf( "Starting ENP execution at %X ... ",enp_go );
*/
		ioctl( enpfid,ENPIOGO, enp_go );
/*
		printf( "DONE!\n" );
*/
	}
}


ENP *
getenp( np )
register char	*np;
{
	register ENP	*ep;

	for( ep = enp_tab; ep < &enp_tab[num_enp]; ep++ )
	if( strcmp( np,ep->name ) == 0 )
	{
		return( ep );
	}
	return( 0 );
}


closem( cnt )
register short	cnt;
{
	register BINS	*fp;

	for( fp = file_tab; fp < &file_tab[cnt]; fp++ )
	{
		close( fp->b_fid );
	}
	if( enpfid != -1 )
	{
		close( enpfid );
	}
}
