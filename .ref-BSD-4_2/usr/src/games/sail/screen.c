#ifndef lint
static	char *sccsid = "@(#)screen.c	1.2 83/05/20";
#endif

#include "externs.h"
#include <sys/types.h>
#include <sys/stat.h>

extern int choke();

readpos()
{
	register int n;
	register postype *p = pos;
	register ships *s = scene[game].ship;

	for (n = scene[game].vessels; n > 0; n--, p++, s++){
		p -> row = s -> shiprow;
		p -> col = s -> shipcol;
		p -> dir = s -> shipdir;
		if (s -> file -> explode == 2 || s -> file -> sink == 2)
			p -> dir = 0;
	}
}

makesignal(fmt, shipnum, owner)
int shipnum, owner;
char *fmt;
{
	int junk;
	char message[60];

	sprintf(message, fmt, scene[game].ship[shipnum].shipname, colours(shipnum), sterncolor(shipnum, &junk, &junk));
	Write(FILES + owner, 1, 164, message);
}

	
#define PI 3.1415926535

float contable[8] = {1.5708, 0.7854, 0.0, -0.7854, -1.5708, -2.3562, -PI, 2.3562};

angle(dr, dc)
register dr, dc;
{
	register i;

	if (dc >= 0 && dr > 0)
		i = 0;
	else if (dr <= 0 && dc > 0)
		i = 2;
	else if (dc <= 0 && dr < 0)
		i = 4;
	else
		i = 6;
	dr = abs(dr);
	dc = abs(dc);
	if ((i == 0 || i == 4) && dc * 2.4 > dr) {
		i++;
		if (dc > dr * 2.4)
			i++;
	} else if ((i == 2 || i == 6) && dr * 2.4 > dc) {
		i++;
		if (dr > dc * 2.4)
			i++;
	}
	return i % 8 + 1;
}

char gunsbear(fromship, toship)		/* checks for target bow or stern */
{
	int dr, dc, i;
	register ang;
	register postype *from = &pos[fromship];
	register postype *to = &pos[toship];

	dr = from->row - to->row;
	dc = to->col - from->col;
	for (i = 2; i; i--) {
		if ((ang = angle(dr, dc) - from->dir + 1) < 1)
			ang += 8;
		if (ang >= 2 && ang <= 4)
			return 'r';
		if (ang >= 6 && ang <= 7)
			return 'l';
		drdc(&dr, &dc, to->dir);
	}
	return 0;
}

portside(fromship, onship, quick)
register fromship, onship;
int quick;			/* returns true if fromship is */
{				/* shooting at onship's starboard side */
	register ang;
	int dr, dc;

	dr = pos[fromship].row - pos[onship].row;
	dc = pos[onship].col - pos[fromship].col;
	if (quick == -1)
		drdc(&dr, &dc, pos[onship].dir);
	ang = angle(dr, dc);
	if (quick != 0)
		return ang;
	ang = (ang + 4 - pos[onship].dir - 1) % 8 + 1;
	return ang < 5;
}

int tantable[40] = {0,100,197,291,381,464,540,610,675,733,785,833,876,915,951,983,1012,1039,1064,1086,1107,1126,1144,1161,1176,1190,1204,1216,1227,1239,1249,1259,1268,1277,1285,1293,1300,1307,1313,1470}; 

double arctan(y,x)
int y,x;
{
	int sx, sy;
	register int index;

	sy = y < 0 ? -1 : 1;
	sx = x < 0 ? -1 : 1;
	y *= sy;
	x *= sx;
	if (!x) return((double) PI/2 * sy);
	index = (10*y)/x + 0.5;
	if (index > 39) index = 39;
	return((double) (sy * (sx < 0 ? PI : 0 + sx*(tantable[index]/1000))));
}

boarders(from, key)
int from, key;
{
	register int n;
	struct BP *ptr;

	ptr = key ? scene[game].ship[from].file -> DBP : scene[game].ship[from].file -> OBP ;
	for (n=0; n < 3; n++){
		if (ptr[n].turnsent)
			return(1);
	}
	return(0);
}

lengthof(string)
char *string;
{
	register int n;

	for (n=0; string[n]; n++);
	return(n);
}


Write(address, string, offset, value)
int string, address, offset, value;
{
	int fd;
	char file[25];
	int *ptr;
	char *cptr;
	struct BP *bptr;
	struct snag *sptr;

	if (string){
		sprintf(Outbuf + buffercount, "%d %d %d %s\n", address, string, offset, value);
	}
	else {
		sprintf(Outbuf + buffercount, "%d %d %d %d\n", address, string, offset, value);
	}
	buffercount += lengthof(Outbuf + buffercount);
	if (address < 32){
		ptr = &scene[game].ship[address].shipnum;
		*(ptr + (offset - 2)/2) = value;
	} else if (address < 64){
		address -= SPECS;
		ptr = &specs[scene[game].ship[address].shipnum].bs;
		*(ptr + offset/2) = value;
	} else if (address < 128){
		address -= FILES;
		ptr = &scene[game].ship[address].file -> points;
		if (offset >= 30 && offset < 48){
			bptr = scene[game].ship[address].file -> OBP;
			switch(((offset - 30) % 6) / 2){
				case 0:
					bptr[(offset - 30)/6].turnsent = value;
					break;
				case 1:
					bptr[(offset - 30)/6].toship = value;
					break;
				case 2:
					bptr[(offset - 30)/6].mensent = value;
					break;
			}
		}
		if (offset >= 48 && offset < 66){
			bptr = scene[game].ship[address].file -> DBP;
			switch(((offset - 48) % 6) / 2){
				case 0:
					bptr[(offset - 48)/6].turnsent = value;
					break;
				case 1:
					bptr[(offset - 48)/6].toship = value;
					break;
				case 2:
					bptr[(offset - 48)/6].mensent = value;
					break;
			}
		}
		if (offset >= 84 && offset < 124){
			sptr = scene[game].ship[address].file -> fouls;
			switch(((offset - 84) % 4) / 2){
				case 0:
					sptr[(offset - 84) / 4].turnfoul = value;
					break;
				case 1:
					sptr[(offset - 84) / 4].toship = value;
					break;
			}
		}
		if (offset >= 124 && offset < 164){
			sptr = scene[game].ship[address].file -> grapples;
			switch(((offset - 124) % 4) / 2){
				case 0:
					sptr[(offset - 124) / 4].turnfoul = value;
					break;
				case 1:
					sptr[(offset - 124) / 4].toship = value;
					break;
			}
		}
		if (offset > 72) ptr = &scene[game].ship[address].file -> drift;
		if (offset > 164) ptr = &scene[game].ship[address].file -> RH;
		cptr = scene[game].ship[address].file -> captain;
		if (offset == 72) cptr = scene[game].ship[address].file -> last;
		if (offset == 164) cptr = scene[game].ship[address].file -> signal;
		if (!string)
			*(ptr + (offset - (offset > 72 ? (offset < 164 ? 82 : 224) : 20))/2 ) = value;
		else
			strcpy(cptr, (char *) value);
	} else {
		ptr = &scene[game].winddir;
		*(ptr + offset/2) = value;
	}
}

sync()
{
	char file[25];
	char source[25];
	int address, string;
	int offset, value[40];
	int fd, *ptr;
	char *cptr;
	struct BP *bptr;
	struct snag *sptr;
	register int n;
	int (*sig1)(), (*sig2)();

	sig1 = signal(SIGHUP, SIG_IGN);
	sig2 = signal(SIGINT, SIG_IGN);
	sprintf(file, "/tmp/sync.%d", game);
	sprintf(source, "/tmp/.%d", game);
	for(n = 0; link(source, file) == -1 && n < 30; n++)
		sleep(2);
	fseek(syncfile, lastsync, 0);
	while(fscanf(syncfile, "%d%d%d", &address, &string, &offset) != EOF){
		if (string){
			fgets((char *) value, 60, syncfile);
			rmend((char *) value);
			while(*(char *)value == ' ')
				strcpy((char *) value, (char *) value + 1);
		}
		else
			fscanf(syncfile, "%d", value);
		if (address < 32){
			ptr = &scene[game].ship[address].shipnum;
			*(ptr + (offset - 2)/2) = value[0];
			continue;
		}
		if (address < 64){
			address -= SPECS;
			ptr = &specs[scene[game].ship[address].shipnum].bs;
			*(ptr + offset/2) = value[0];
			continue;
		}
		if (address < 128){
			address -= FILES;
			ptr = &scene[game].ship[address].file -> points;
			if (offset >= 30 && offset < 48){
				bptr = scene[game].ship[address].file -> OBP;
				switch(((offset - 30) % 6) / 2){
					case 0:
						bptr[(offset - 30)/6].turnsent = value[0];
						break;
					case 1:
						bptr[(offset - 30)/6].toship = value[0];
						break;
					case 2:
						bptr[(offset - 30)/6].mensent = value[0];
						break;
				}
			}
			if (offset >= 48 && offset < 66){
				bptr = scene[game].ship[address].file -> DBP;
				switch(((offset - 48) % 6) / 2){
					case 0:
						bptr[(offset - 48)/6].turnsent = value[0];
						break;
					case 1:
						bptr[(offset - 48)/6].toship = value[0];
						break;
					case 2:
						bptr[(offset - 48)/6].mensent = value[0];
						break;
				}
			}
			if (offset >= 84 && offset < 124){
				sptr = scene[game].ship[address].file -> fouls;
				switch(((offset - 84) % 4) / 2){
					case 0:
						sptr[(offset - 84) / 4].turnfoul = value[0];
						break;
					case 1:
						sptr[(offset - 84) / 4].toship = value[0];
						break;
				}
			}
			if (offset >= 124 && offset < 164){
				sptr = scene[game].ship[address].file -> grapples;
				switch(((offset - 124) % 4) / 2){
					case 0:
						sptr[(offset - 124) / 4].turnfoul = value[0];
						break;
					case 1:
						sptr[(offset - 124) / 4].toship = value[0];
						break;
				}
			}
			if (offset > 72) ptr = &scene[game].ship[address].file -> drift;
			if (offset > 164) ptr = &scene[game].ship[address].file -> RH;
			cptr = scene[game].ship[address].file -> captain;
			if (offset == 72) cptr = scene[game].ship[address].file -> last;
			if (offset == 164) cptr = scene[game].ship[address].file -> signal;
			if (!string)
				*(ptr + (offset - (offset > 72 ? (offset < 164 ? 82 : 224) : 20))/2 ) = value[0];
			else
				strcpy(cptr, (char *) value);
			continue;
		}
		ptr = &scene[game].winddir;
		*(ptr + offset/2) = value[0];
	}
	if (buffercount){
		fseek(syncfile, 0L, 2);
		fprintf(syncfile, "%s", Outbuf);
		fflush(syncfile);
		buffercount = 0;
	}
	lastsync = ftell(syncfile);
	unlink(file);
	signal(SIGHUP, sig1);
	signal(SIGINT, sig2);
}

rmend(str)
char *str;
{
	register int n;

	for (n=0; *(str + n); n++);
	if (n)
		*(str + n - 1) = '\0';
}

char
colours(ship)
int ship;
{
	char flag;
	int index;
	struct File *ptr;

	ptr = scene[game].ship[ship].file;
	if (ptr -> struck)
		flag = '!';
	if (ptr -> explode)
		flag = '#';
	if (ptr -> sink)
		flag = '~';
	if (ptr -> struck)
		return(flag);
	if (ptr -> captured > -1)
		index = scene[game].ship[ptr -> captured].nationality;
	else
		index = scene[game].ship[ship].nationality;
	switch(index){
		
		case 0:
			flag = 'a';
			break;
		case 1:
			flag = 'b';
			break;
		case 2:
			flag = 's';
			break;
		case 3:
			flag = 'f';
			break;
	}
	return(ptr -> FS ? flag - 32 : flag);
}

char
sterncolor(ship, sternr, sternc)
int ship, *sternr, *sternc;
{
	int sr, sc;
	int cap;
	cap = scene[game].ship[ship].file -> captured;
	sr = pos[ship].row;
	sc = pos[ship].col;
	drdc(&sr,&sc,pos[ship].dir);
	*sternr = sr;
	*sternc = sc;
	ship -= nation[scene[game].ship[ship].nationality];
	return(ship + '0' - 10*(cap > -1));
}
