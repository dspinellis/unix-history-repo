/* tail command 
 *
 *	tail where [file]
 *	where is +_n[type]
 *	- means n lines before end
 *	+ means nth line from beginning
 *	type 'b' means tail n blocks, not lines
 *	type 'c' means tail n characters
 *	Type 'r' means in lines in reverse order from end
 *	 (for -r, default is entire buffer )
*/
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<errno.h>
#define LBIN 4097
struct	stat	statb;
char bin[LBIN];
int errno;

main(argc,argv)
char **argv;
{
	long n,di;
	register i,j,k;
	char *p;
	int partial,piped,bylines,bkwds,fromend,lastnl;
	char *arg;
	lseek(0,(long)0,1);
	piped = errno==ESPIPE;
	arg = argv[1];
	if(argc<=1 || *arg!='-'&&*arg!='+') {
		arg = "-10l";
		argc++;
		argv--;
	}
	fromend = *arg=='-';
	arg++;
	n = 0;
	while(digit(*arg))
		n = n*10 + *arg++ - '0';
	if(!fromend&&n>0)
		n--;
	if(argc>2) {
		close(0);
		if(open(argv[2],0)!=0) {
			write(2,"tail: can't open ",17);
			write(2,argv[2],strlen(argv[2]));
			write(2,"\n",1);
			exit(1);
		}
	}
	bylines = 0; bkwds = 0;
	switch(*arg) {
	case 'b':
		n <<= 9;
		break;
	case 'c':
		break;
	case 'r':
		if(n==0) n = LBIN;
		bkwds = 1; fromend = 1; bylines = 1;
		break;
	case '\0':
	case 'l':
		bylines = 1;
		break;
	default:
		goto errcom;
	}
	if(fromend)
		goto keep;

			/*seek from beginning */

	if(bylines) {
		j = 0;
		while(n-->0) {
			do {
				if(j--<=0) {
					p = bin;
					j = read(0,p,512);
					if(j--<=0) exit(0);
				}
			} while(*p++ != '\n');
		}
		write(1,p,j);
	} else  if(n>0) {
		if(!piped)
			fstat(0,&statb);
		if(piped||(statb.st_mode&S_IFMT)==S_IFCHR)
			while(n>0) {
				i = n>512?512:n;
				i = read(0,bin,i);
				if(i<=0) exit(0);
				n -= i;
			}
		else
			lseek(0,n,0);
	}
copy:
	while((i=read(0,bin,512))>0)
		write(1,bin,i);
	exit(0);

			/*seek from end*/

keep:
	if(n<=0) exit(0);
	if(!piped) {
		fstat(0,&statb);
		di = !bylines? n: LBIN-1;
		if(statb.st_size > di)
			lseek(0,-di,2);
		if(!bylines)
			goto copy;
	}
	partial = 1;
	for(;;) {
		i = 0;
		do {
			j = read(0,&bin[i],LBIN-i);
			if(j<=0)
				goto brka;
			i += j;
		} while(i<LBIN);
		partial = 0;
	}
brka:
	if(!bylines) {
		k =
		    n<=i ? i-n:
		    partial ? 0:
		    n>=LBIN ? i+1:
		    i-n+LBIN;
		k--;
	} else {
		if(bkwds && bin[i==0?LBIN-1:i-1]!='\n'){	/* force trailing newline */
			bin[i]='\n';
			if(++i>=LBIN) {i = 0; partial = 0;}
		}
		k = i;
		j = 0;
		do {
			lastnl = k;
			do {
				if(--k<0) {
					if(partial) {
						if(bkwds) write(1,bin,lastnl+1);
						goto brkb;
					}
					k = LBIN -1;
				}
			} while(bin[k]!='\n'&&k!=i);
			if(bkwds && j>0){
				if(k<lastnl) write(1,&bin[k+1],lastnl-k);
				else {
					write(1,&bin[k+1],LBIN-k-1);
					write(1,bin,lastnl+1);
				}
			}
		} while(j++<n&&k!=i);
brkb:
		if(bkwds) exit(0);
		if(k==i) do {
			if(++k>=LBIN)
				k = 0;
		} while(bin[k]!='\n'&&k!=i);
	}
	if(k<i)
		write(1,&bin[k+1],i-k-1);
	else {
		write(1,&bin[k+1],LBIN-k-1);
		write(1,bin,i);
	}
	exit(0);
errcom:
	write(2,"usage: tail +_n[lbcr] [file]\n",30);
	exit(1);
}

digit(c)
{
	return(c>='0'&&c<='9');
}
