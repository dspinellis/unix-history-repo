#include <sys/types.h>
#include <sys/time.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <a.out.h>
#include <stdio.h>
#include <ctype.h>

char *malloc();
static void rewind();
long lseek();
int wflag;
int xflag;
int tflag;
int cflag;
int vflag;
int dflag;
int fflag;
int totalreadfiles = 0 ;
int totalreadblocks = 0 ;
int totalreadlines = 0 ;
int totalreadchars = 0 ;
int totalwritefiles = 0 ;
int totalwriteblocks = 0 ;
int totalwritelines = 0 ;
int totalwritechars = 0 ;

main(argc,argv)
	int argc;
	char *argv[];
{
	struct tm *tm;
	long timetemp,time();
	int year;
	int day;
	char *tapename;
	char *filename;
	char *namelist=NULL;
	char *device = "/dev/rmt12";
	int tape;
	int file;
	int filenum;
	int argnum;
	char line[1001];
	char vmsname[1000];
	char unixname[1000];
	FILE *names;
	int count;
	int tmp;
	char blockchar;
	int blocksize=2048;
	int recordsize=1;

	char *key;

	timetemp = time((long *)NULL);
	tm = localtime(&timetemp);
	year = tm->tm_year;
	day = tm->tm_yday;
	tapename = malloc(10);
	gethostname(tapename,6);
	tapename[7]='\0';

	/* parse command line */
	if (argc < 2)
		usage();

	argv++;
	argc--;
	/* loop through first argument (key) */
	argc--;
	for (key = *argv++; *key; key++) 
		switch(*key) {

		case 'f':
			if (*argv == NULL || argc <1) {
				fprintf(stderr,
			"ansitape: 'f' option requires tape name \n");
				usage();
			}
			device = *argv++;
			argc--;
			break;

		case 'n':
			if (*argv == NULL || argc <1) {
				fprintf(stderr,
			"ansitape: 'n' option requires file name\n");
				usage();
			}
			namelist = *argv++;
			argc--;
			break;

		case 'l':
			if (*argv == NULL || argc<1) {
				fprintf(stderr,
			"ansitape: 'l' option requires label\n");
				usage();
			}
			tapename = *argv++;
			argc--;
			break;

		case 'F':
			if(*argv == NULL) {
				fprintf(stderr, 
		"ansitape: 'F' options requires recordsize and blocksize specifiers.\n"
						);
				usage();
			}
			tmp = sscanf(*argv++," %d%c ",&recordsize,&blockchar);
			argc--;
			if(tmp<1) { 
				fprintf(stderr,"illegal recordsize: recordsize set to 80\n");
				recordsize=80;
			} else if(tmp>1) {
				if(blockchar == 'b') recordsize *= 512;
				if(blockchar == 'k') recordsize *= 1024;
			}

			if (*argv == NULL) {
				fprintf(stderr,
			"ansitape: 'F' option requires blocksize specifier \n");
				usage();
			}
			tmp = sscanf(*argv++," %d%c ",&blocksize,&blockchar);
			argc--;
			if(tmp<1) { 
				fprintf(stderr,"illegal blocksize: blocksize set to 2048\n");
				blocksize=2048;
			} else if(tmp>1) {
				if(blockchar == 'b') blocksize *= 512;
				if(blockchar == 'k') blocksize *= 1024;
			}
			if(blocksize <18) blocksize=18;
			if(blocksize >62*1024) blocksize=62*1024;
			fflag++;
			break;

		case 'b':
			if (*argv == NULL) {
				fprintf(stderr,
			"ansitape: 'b' option requires blocksize specifier \n");
				usage();
			}
			tmp = sscanf(*argv++," %d%c ",&blocksize,&blockchar);
			argc--;
			if(tmp<1) { 
				fprintf(stderr,"illegal blocksize: blocksize set to 2048\n");
				blocksize=2048;
			} else if(tmp>1) {
				if(blockchar == 'b') blocksize *= 512;
				if(blockchar == 'k') blocksize *= 1024;
			}
			if(blocksize <18) blocksize=18;
			if(blocksize >62*1024) blocksize=62*1024;
			break;

		case 'c':
			cflag++;
			wflag++;
			break;

		case 'r':
			/*I know, this should be rflag, but I just don't like r for write*/
			wflag++;
			break;

		case 'v':
			vflag++;
			break;

		case 'x':
			xflag++;
			break;

		case 't':
			tflag++;
			break;

		case '-':
			break;

		default:
			fprintf(stderr, "ansitape: %c: unknown option\n", *key);
			usage();
		}

	if (!wflag && !xflag && !tflag)
		usage();

	tape = open(device,wflag?O_RDWR:O_RDONLY,NULL);
	if(tape<0) {
		perror(device);
		fprintf(stderr,"tape not accessable - check if drive online and write ring present\n");
		exit(1);
	}
	rewind(tape);
	filenum=1;
	casefix(tapename);

	if(cflag) { 
		writevol(tapename,tape);
	} else {
		getvol(tapename,tape);
		while(1) {
			/* read files */
			if( readfile(tape,argc,argv) ) break;
			filenum++;
		}
		backspace(tape);
	}

	if(wflag) {
		if(namelist) {
			if(*namelist == '-') {
				names = stdin;
			} else {
				names=fopen(namelist,"r");
				if(names == NULL) {
					fprintf(stderr,"unable to open namelist file - no files added to tape\n");
				}
			}
			while(1) {
				fgets(line,1000,names);
				if(feof(names)) break;
				count = sscanf(line,"%s %s",unixname,vmsname);
				if(count<1) continue; /* blank line */
				if(count==1) strcpy(vmsname,unixname);
				casefix(vmsname);
				if(filecheck(&file,unixname)) continue;
				writefile(tape,file,vmsname,tapename,filenum,year,day,blocksize,
						recordsize);
				filenum++;
				close(file);
			}
		} else {
			for(argnum=0;argnum<argc;argnum++) {
				filename = argv[argnum];
				if(filecheck(&file,filename)) continue;
				casefix(filename);
				writefile(tape,file,filename,tapename,filenum,year,day,
						blocksize,recordsize);
				filenum++;
				close(file);
			}
		}
		writetm(tape);
		writetm(tape);
		writetm(tape);
		writetm(tape);
	}
	rewind(tape);
	close(tape);
	if(vflag && (tflag || xflag)) {
		fprintf(stdout," read  %d files in %d blocks (%d lines, %d chars)\n",
			totalreadfiles,totalreadblocks,totalreadlines,totalreadchars);
	}
	if(vflag && wflag) {
		fprintf(stdout," wrote  %d files in %d blocks (%d lines, %d chars)\n",
			totalwritefiles,totalwriteblocks,totalwritelines,totalwritechars);
	}
	return(0);
}
usage() {
	fprintf(stderr,
			"ansitape: usage: ansitape -{rxtc}[flnvb] [filename] [label] [filename] [blocksize] [files]\n");
	exit(1);
}

writefile(tape,file,filename,tapename,filenum,year,day,blocksize,recordsize)
	int tape;
	int file;
	char *filename;
	char *tapename;
	int filenum;
	int year;
	int day;
	int blocksize;
	int recordsize;

{
	int blocks;
	writehdr1(tape,filename,tapename,filenum,year,day);
	writehdr2(tape,blocksize,recordsize);
	writehdr3(tape);
	writetm(tape);
	writedata(tape,file,filename,&blocks,blocksize,recordsize);
	writetm(tape);
	writeeof1(tape,filename,tapename,filenum,year,day,blocks);
	writeeof2(tape,blocksize,recordsize);
	writeeof3(tape);
	writetm(tape);
	totalwritefiles++;
}

writedata(tape,file,filename,blocks,blocksize,recsize)
	int tape;
	int file;
	char *filename;
	int *blocks;
	int blocksize;
	int recsize;
{
char *ibuf;
char *ibufstart;
char *obuf;
char *obufstart;
char *endibuf;
char *endobuf;
int got;
int i;
char *j;
int numchar = 0 ;
int numline = 0 ;
int numblock = 0;
int success;

	ibufstart = ibuf = malloc((unsigned)(blocksize<4096?8200:(2*blocksize+10)));
	obufstart = obuf = malloc((unsigned)(blocksize+10));
	endobuf = obuf + blocksize;
	endibuf = ibuf;


	i=0;
	if (!fflag) {
		while(1) {
			if(ibuf+i>=endibuf) {	/* end of input buffer */
				strncpy(ibufstart,ibuf,endibuf-ibuf); /* copy leftover to start */
				ibuf = ibufstart+(endibuf-ibuf);	/* point to end of valid data */
				got = read(file,ibuf,blocksize<4096?4096:2*blocksize);	/* read in a chunk */
				endibuf = ibuf + got;
				ibuf = ibufstart;	/* point to beginning of data */
				if(got == 0) { /* end of input */
					if(ibuf==ibufstart){ /* no leftovers */
						break; /* done */
					} else {
						ibuf[i]='\n'; /* fake extra newline */
					}
				}
			}

			if(obuf+i+4 > endobuf) {  /* end of output buffer */
				if(i>blocksize-4) { 
					printf("record exceeds blocksize - file truncated\n");
					break;
				}
				/* filled up output record - have to fill,output,restart*/
				for(j=obuf;j<endobuf;j++) {
					*j = '^';
				}
				success = write(tape,obufstart,blocksize);
				if(success != blocksize) {
					perror("tape");
					fprintf(stderr," hard write error:  write aborted\n");
					rewind(tape);
					exit(1);
				}
				obuf=obufstart;
				numchar -= i;
				i=0;
				numblock++;
				continue;
			}

			if(ibuf[i] == '\n') { /* end of line */
				obuf[0] = ((i+4)/1000) + '0';
				obuf[1] = (((i+4)/100)%10) + '0';
				obuf[2] = (((i+4)/10)%10) + '0';
				obuf[3] = (((i+4)/1)%10) + '0';
				obuf += (4+i); /* size + strlen */
				ibuf += (1+i); /* newline + strlen */
				i=0;
				numline++;
				continue; /* back to the top */
			}

			obuf[i+4]=ibuf[i];
			numchar++;
			i++;

		}
		/* exited - write last record and go for lunch */
		if(obuf != obufstart) {
			for(j=obuf;j<endobuf;j++) {
				*j = '^';
			}
			success = write(tape,obufstart,blocksize);
			if(success != blocksize) {
				perror("tape");
				fprintf(stderr," hard write error:  write aborted\n");
				rewind(tape);
				exit(1);
			}
			numblock++;
		}
	 } else {
		fflush(stdout);
		while(1) {
			/* writing an 'F' format tape */
			got = read(file,ibuf,recsize+1);
			if(got == 0) {
				/* end of input */
				if(obuf<=obufstart) {
					break; /* done */
				} else {
					/* no more data, so force the record out */
					recsize = blocksize+1;
				}
			} else if(got != recsize+1) {
				printf("short read: filled\n");
			} else if( *(ibuf+recsize) != '\n') {
				printf("corrupted record - write aborted\b");
				rewind(tape);
				exit(1);
			}
			if(obuf+recsize >endobuf) {
				/*would overflow output buffer, so fill up old buffer */
				for(j=obuf;j<endobuf;j++) {
					*j = '^';
				}
				/* and write it */
				success = write(tape,obufstart,blocksize);
				if(success != blocksize) {
					perror("tape");
					fprintf(stderr," hard write error:   write aborted\n");
					rewind(tape);
					exit(1);
				}
				obuf=obufstart;
				numblock++;
			}
			bcopy(ibuf,obuf,recsize);
			obuf+=got-1;
			numline++;
			numchar += recsize;
		}
		numchar -= recsize;
		numline--;
	}
	free(ibufstart);
	free(obufstart);
	if(vflag) {
		fprintf(stdout,"r - %s:  %d lines (%d chars) in %d tape blocks\n",
				filename,numline,numchar,numblock);
	}
	totalwritechars += numchar;
	totalwritelines += numline;
	totalwriteblocks += numblock;
	*blocks = numblock;
}

writetm(tape)
	int tape;
{
	struct mtop mtop;
	mtop.mt_op = MTWEOF;
	mtop.mt_count = 1;
	ioctl(tape,MTIOCTOP,&mtop);
}

void
static rewind(tape)
	int tape;
{
	struct mtop mtop;
	mtop.mt_op = MTREW;
	mtop.mt_count = 1;
	ioctl(tape,MTIOCTOP,&mtop);
}

skipfile(tape)
	int tape;
{
	struct mtop mtop;
	mtop.mt_op = MTFSF;
	mtop.mt_count = 1;
	ioctl(tape,MTIOCTOP,&mtop);
}

backspace(tape)
	int tape;
{
	struct mtop mtop;
	mtop.mt_op = MTBSF;
	mtop.mt_count = 1;
	ioctl(tape,MTIOCTOP,&mtop);
}

writehdr1(tape,filename,tapename,filenum,year,day)
	int tape;
	char *filename;
	char *tapename;
	int filenum;
	int year;
	int day;
{
	char buf[81];
	sprintf(buf,
"HDR1%-17.17s%-6.6s0001%4.4d000101 %2.2d%3.3d %2.2d%3.3d 000000DECFILE11A          "
		,filename,tapename,filenum,year,day,year,day);
	write(tape,buf,80);
}

writeeof1(tape,filename,tapename,filenum,year,day,blocks)
	int tape;
	char *filename;
	char *tapename;
	int filenum;
	int year;
	int day;
	int blocks;
{
	char buf[81];
	sprintf(buf,
"EOF1%-17.17s%-6.6s0001%4.4d000101 %2.2d%3.3d %2.2d%3.3d %6.6dDECFILE11A          "
		,filename,tapename,filenum,year,day,year,day,blocks);
	write(tape,buf,80);
}

writehdr2(tape,blocksize,recordsize)
	int tape;
	int blocksize;
	int recordsize;
{
	char buf[81];
	sprintf(buf,"HDR2%c%5.5d%5.5d%35.35s00%28.28s",fflag?'F':'D',
			blocksize,recordsize," "," ");
	write(tape,buf,80);
}

writeeof2(tape,blocksize,recordsize)
	int tape;
	int blocksize;
	int recordsize;
{
	char buf[81];
	sprintf(buf,"EOF2%c%5.5d%5.5d%35.35s00%28.28s",fflag?'F':'D',
			blocksize,recordsize," "," ");
	write(tape,buf,80);
}

writehdr3(tape)
	int tape;
{
	char buf[81];
	sprintf(buf, "HDR3%76.76s"," ");
	write(tape,buf,80);
}

writeeof3(tape)
	int tape;
{
	char buf[81];
	sprintf(buf, "EOF3%76.76s"," ");
	write(tape,buf,80);
}

writevol(tapename,tape)
	int tape;
	char *tapename;
{
	char buf[81];
	sprintf(buf,"VOL1%-6.6s %26.26sD%%C%10.10s1%28.28s3",tapename," "," "," ");
	write(tape,buf,80);
	if(vflag) {
		fprintf(stdout," tape labeled %-6.6s\n",tapename);
	}
}

getvol(tapename,tape)
	int tape;
	char *tapename;
{
	char buf[81];
	read(tape,buf,80);
	sscanf(buf,"VOL1%6s",tapename);
	if(vflag) {
		fprintf(stdout," tape was labeled %-6.6s\n",tapename);
	}
}

casefix(string)
    register char *string;
{
    while(*string) {
        if(islower(*string)) {
            *string = toupper(*string);
        }
        string++;
    }
}

int
readfile(tape,argc,argv)
	int tape;
	int argc;
	char *argv[];
{
char buf[80];
char mode;
char filename[18];
FILE *file;
int extract;
char *ibuf;
char *ibufstart;
char *endibuf;
char *fixpoint;
int size;
int numblock = 0 ;
int numchar = 0 ;
int numline = 0 ;
int argnum;
int ok;
int blocksize;
int recordsize;

	if(!(read(tape,buf,80))) return(1); /* no hdr record, so second eof */
	sscanf(buf,"HDR1%17s",filename);
	read(tape,buf,80);
	sscanf(buf,"HDR2%c%5d%5d",&mode,&blocksize,&recordsize);
	blocksize = blocksize>recordsize?blocksize:recordsize;
	skipfile(tape); /* throw away rest of header(s) - not interesting */
	ibufstart=ibuf=malloc((unsigned)(blocksize+10));
	endibuf=ibufstart+blocksize;
	extract=0;
	if(tflag || xflag) {
		ok=0;
		if(!argc) {
			ok=1;
		} else for(argnum=0;argnum<argc;argnum++) {
			casefix(argv[argnum]);
			if(!strcmp(filename,argv[argnum])) {
				ok=1;
				break;
			}
		}
		if(mode == 'D') {
			if(xflag && ok) {
				file = fopen(filename,"w");
				if(file == NULL) {
					perror(filename);
				} else {
					extract = 1;
				}
			}
			while(size=read(tape,ibufstart,blocksize)) {
				if(size != blocksize) {
					/*
					 * somebody's brain damaged program leaves
					 * short blocks on the tape - fill them up to size
					 * (this is work THEY should have done before writing
					 * their undersized blocks)
					 */
					for(fixpoint=ibufstart+size;fixpoint<endibuf;fixpoint++) {
						*fixpoint='^';
					}
				}
				numblock++;
				ibuf = ibufstart;
				while(strncmp("^^^^",ibuf,4)) {
#define getsize(a) ((a[0]-'0')*1000)+((a[1]-'0')*100)+((a[2]-'0')*10)+(a[3]-'0')
#define bad(a) (!(isdigit(ibuf[a])))
					if(bad(0) || bad(1) || bad(2) || bad(3)) {
						fprintf(stderr, "error:  bad record length field - file may be corrupted, skipping\n");
						break;
					}
					size = getsize(ibuf);
					if(extract) {
						fwrite(ibuf+4,sizeof(char),size-4,file);
						fwrite("\n",1,1,file);
					}
					ibuf += (size);
					numline++;
					numchar += (size-4);
					if(ibuf > endibuf+1) {
						fprintf(stderr,"error:  bad tape records(s) - file may be corrupted\n");
						break;
					}
					if(ibuf>endibuf-4) break;
				}
			}
			if(extract) {
				fclose(file);
			}
		} else if (mode == 'F') {
			if(xflag && ok) {
				file = fopen(filename,"w");
				if(file == NULL) {
					perror(filename);
				} else {
					extract = 1;
				}
			}
			while(read(tape,ibufstart,blocksize)) {
				numblock++;
				ibuf = ibufstart;
				while(ibuf+recordsize <= endibuf) {
					if(extract) {
						fwrite(ibuf,sizeof(char),recordsize,file);
						fwrite("\n",1,1,file);
					}
					ibuf += recordsize;
					numline++;
					numchar += recordsize;
				}
			}
			if(extract) {
				fclose(file);
			}
		} else {
			fprintf(stderr,"unknown record mode (%c) - file %s skipped\n",
					mode,filename);
			skipfile(tape);	/* throw away actual file */
		}
	} else {
		/* not interested in contents of file, so move fast */
		skipfile(tape);
	}
	skipfile(tape); /* throw away eof stuff - not interesting */
	totalreadchars += numchar;
	totalreadlines += numline;
	totalreadblocks += numblock;
	totalreadfiles ++;
	if(xflag && vflag && ok) {
		fprintf(stdout,"x - %s:  %d lines (%d chars) in %d tape blocks\n",
				filename,numline,numchar,numblock);
	} else if(tflag && ok) {
		fprintf(stdout,"t - %s:  %d lines (%d chars) in %d tape blocks\n",
				filename,numline,numchar,numblock);
	}
	free(ibufstart);
	return(0);
}

filecheck(file,name)
	int *file;
	char *name;

{

	struct stat buf;
	struct exec sample;

	stat(name,&buf);
	if ((buf.st_mode & S_IFDIR)==S_IFDIR) { 
		fprintf(stderr,"%s: directory - skipped\n",name);
		return(1);
	}
	if ((buf.st_mode & S_IFCHR)==S_IFCHR) { 
		fprintf(stderr,"%s: character device - skipped\n",name);
		return(1);
	}
	if ((buf.st_mode & S_IFBLK)==S_IFBLK) {
		fprintf(stderr,"%s: block device - skipped\n",name);
		return(1);
	}
	if ((buf.st_mode & S_IFLNK)==S_IFLNK) {
		fprintf(stderr,"%s: symbolic link - skipped\n",name);
		return(1);
	}
	if ((buf.st_mode & S_IFSOCK)==S_IFSOCK) {
		fprintf(stderr,"%s: socket - skipped\n",name);
		return(1);
	}
	*file = open(name,O_RDONLY,NULL);
	if(*file <0) { 
		perror(name);
		return(1);
	}
	if(read(*file,&sample,sizeof(struct exec))>= sizeof(struct exec)) {
		if(!(N_BADMAG(sample))) {
			/* executable */
			/* the format requires either fixed blocked records,
			 * or variable format records with each record remaining
			 * entirely within a tape block - this limits the
			 * distance between \n's to 2044 bytes, something
			 * which is VERY rarely true of executables, so
			 * we don't even try with them....
			 */
			close(*file);
			fprintf(stderr,"%s: executable - skipped\n",name);
			return(1);
		}
	}
	/* either couldn't read sizeof(struct exec) or wasn't executable */
	/* so we assume it is a reasonable file until proven otherwise */
	lseek(*file,0l,0);
	return(0);
}
