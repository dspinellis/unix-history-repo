#include <signal.h>
/* Magnetic Tape Manipulation Program
** mtm [-sn] [-lm] [-bp] [unit]
**
** skip n files
** list m files
** buffer size pK bytes
** on unit 
**
** Assumes definition of raw magnetic tapes and the 
** mapping of 4-7 into physical drives 0-3
*/
char *buf;
int bcnt;
int filecnt;
int fd;
int unit;
char *file ;

main(argc,argv)
int argc;
char *argv[];
{
	char *p;
	int nskip, nlist, skip, list;
	int finis();

	signal(SIGINT,finis);
	signal(SIGQUIT,finis);
	unit = nskip = nlist = 0;
	skip = 0;
	list = 1;
	bcnt = 2;
	file = "/dev/rmt0 ";
	filecnt=1;
	argv++;

	while(--argc) {
		p = *argv++;
		if(*p == '-'){
			++p;
			switch(*p){

			case 's':
				skip++;
				list--;
				nskip = atoi(++p);
				break;
	
			case 'l':
				list++;
				nlist = atoi(++p);
				break;
	
			case 'b':
				bcnt = atoi(++p);
				if(bcnt < 30) break;
				printf("Requested buffer too big %dK\n",bcnt);
				exit(8);
			default:
				printf("Bad option\n");
				exit(8);
			}
		}
		else{

			if((*p < '0') || (*p> '9')){
				printf("Syntax Error\n");
				exit(8);
			}
			unit = atoi(p);
		}
	}

	bcnt = bcnt<<10;
	if (((int)(buf = sbrk(bcnt))) < 0) {
		printf("requested buffer too big - %dK\n",bcnt);
		exit(8);
	}

	unit = unit | 04;	/* force to no rewind unit number */
	sprintf(&file[8],"%d",unit);
	if((fd = open(file, 0)) < 0) {
		printf("Tape Open Error %s\n",file);
		exit(8);
	}

	if(skip)
		files(nskip,0);
	if(list){
		if(nlist == 0)
			nlist = -1;
		files(nlist,1);
		finis();
	}
}

files(nfiles,lflag)
int nfiles, lflag;
{
	int cnt, prevcnt, reccnt, eof;
	if(nfiles == 0)return;
	eof=0;
	for(; nfiles--; filecnt++){
		cnt = reccnt = prevcnt = 0;
		do{
			prevcnt = cnt;
			if((cnt = read(fd,buf,bcnt)) == 0){	/* EOF */
				if(eof){	/* double eof */
					printf("Double EOF after file %d\n",
						--filecnt);
					finis();
				}
				else{
					eof=1;
					if(lflag)
						printf("      %d Records\n",reccnt);
				}
			}
			else{
				eof = 0;
				if((++reccnt == 1) && lflag) printf("File %d:\n",filecnt);
				if(cnt<0){	/* error */
					if(lflag) printf("      Record %d - ERROR\n",reccnt);
				}
				else{	/*data read*/
					if(lflag && cnt != prevcnt)
						printf("      Record %d - %d bytes\n",
							reccnt,cnt);
				}
			}
		}while(eof==0);
	}
}


finis()
{
	unit -= 4;
	sprintf(&file[8],"%d",unit);
	close(fd);
	fd = open(file, 0);
	close(fd);
	printf("DONE\n");
	exit(0);
}
