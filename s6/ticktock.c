/*
 * ticktock - a nice big clock on a soft copy terminal
 *
 * Author: Ivan Maltz June, 1977
 */
main()
{
char    *buf;
int    tvec[2],f;
while (1) {
    time(tvec);
    buf = ctime(tvec);
    buf =+ 9;
    buf[0] = buf[1] = ' ';
    buf[7] = 0;
    fix(&buf[2]);
    printf("");
    f = fork();
    if (f == 0)
        execl("/usr/bin/block","block","3","@"," ",buf,0);
    wait(&f);
    printf("\n\n\n\n\n\n\n");
    time(tvec);
    buf = ctime(tvec);
    buf =+ 11;
    buf[8] = 0;
    fix(&buf[0]);
    printf("%s",buf);
    while (buf[6] <= '5')
    {
        printf("%s",buf);
        if (buf[7]++ == '9')
        {
            buf[7] = '0';
            buf[6]++;
        }
        sleep(1);
    }
}
}

fix(c)
char    *c;
{
switch (c[0]) {
    case '0':
	if (c[1] == '0') {
	    c[0] = '1';
	    c[1] = '2';
	} else {
	    c[0] = ' ';
	}
	break;
    case '1':
	if (c[1] > '2') {
	    c[0] = ' ';
	    c[1] =- 2;
	}
	break;
    case '2':
	if (c[1] <= '1') {
	    c[0] = ' ';
	    c[1] =+ 8;
	} else {
	    c[0] = '1';
	    c[1] =- 2;
	}
	break;
}
}
