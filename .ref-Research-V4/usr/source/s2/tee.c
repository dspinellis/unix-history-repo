int open[9] { 1 };
int n 1;
int t 0;

char in[512];

char out[512];

main(argc,argv)
char **argv;
{
	int register r,w,p;
	struct { int x1[2], type, x2[15]; } buf;
	fstat(1,&buf);
	t = (buf.type&060000)==020000;
	while(argc-->1) {
		open[n++] = creat(argv[1],0666);
		if(stat(argv[1],&buf)>=0)
			if((buf.type&060000)==020000)
				t++;
		argv++;
	}
	r = w = 0;
	for(;;) {
		for(p=0;p<512;) {
			if(r>=w) {
				if(t>0&&p>0) break;
				w = read(0,in,512);
				r = 0;
				if(w<=0) {
					stash(p);
					return;
				}
			}
			out[p++] = in[r++];
		}
		stash(p);
	}
}

stash(p)
{
	int k;
	int i;
	int d;
	d = t ? 10 : p;
	for(i=0; i<p; i=+d)
		for(k=0;k<n;k++)
			write(open[k], out+i, d<p-i?d:p-i);
}
