#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

#include "gedit.h"

#ifndef lint
static char *rcsid_gedit_c = "$Header: gedit.c,v 10.6 86/02/01 16:18:53 tony Rel $";
#endif	lint

char *malloc(), *gentry(), *strcpy(), *strcat();

struct state cur_state = {	/* everything about what we are doing */
  NULL, 1, 4, 0, 0, 0, 0, 0, 0, 16, 0, NULL, 0, 0, 0
};

/*extern char Aborted;*/

struct prototype *directory;

short incol;		/* current text prompt column */
char *prompt;		/* what the current user prompt is */
char typein[100];	/* user input buffer */

char ocomp[8][8] =	/* orientation composition matrix */
  {	NORTH,	EAST,	SOUTH,	WEST,	RNORTH,	REAST,	RSOUTH,	RWEST,
	EAST,	SOUTH,	WEST,	NORTH,	RWEST,	RNORTH,	REAST,	RSOUTH,
	SOUTH,	WEST,	NORTH,	EAST,	RSOUTH,	RWEST,	RNORTH,	REAST,
	WEST,	NORTH,	EAST,	SOUTH,	REAST,	RSOUTH,	RWEST,	RNORTH,
	RNORTH,	REAST,	RSOUTH,	RWEST,	NORTH,	EAST,	SOUTH,	WEST,
	REAST,	RSOUTH,	RWEST,	RNORTH,	WEST,	NORTH,	EAST,	SOUTH,
	RSOUTH,	RWEST,	RNORTH,	REAST,	SOUTH,	WEST,	NORTH,	EAST,
	RWEST,	RNORTH,	REAST,	RSOUTH,	EAST,	SOUTH,	WEST,	NORTH
  };

char lcomp[8][9] =	/* orient & label composition matrix */
  {	CC,  TC,  BC,  CL,  TL,  BL,  CR,  TR,  BR,	/* north */
	CC,  CR,  CL,  TC,  TR,  TL,  BC,  BR,  BL,	/* east */
	CC,  BC,  TC,  CR,  BR,  TR,  CL,  BL,  TL,	/* south */
	CC,  CL,  CR,  BC,  BL,  BR,  TC,  TL,  TR, 	/* west */
	CC,  TC,  BC,  CR,  TR,  BR,  CL,  TL,  BL,	/* rnorth */
	CC,  CL,  CR,  TC,  TL,  TR,  BC,  BL,  BR,	/* reast */
	CC,  BC,  TC,  CL,  BL,  TL,  CR,  BR,  TR,	/* rsouth */
	CC,  CR,  CL,  BC,  BR,  BL,  TC,  TR,  TL	/* rwest */
};

char *lorient[] = { "cc", "tc", "bc", "cl", "tl", "bl", "cr", "tr", "br" };
char *oorient[] = { "n", "e", "s", "w", "rn", "re", "rs", "rw" };

/* read a coordinate from the input file */
short read_coord(f)
  FILE *f;
  {	register int ch;
	register short coord = 0;
	char sign = 0;

	/* skip over leading blanks */
	while ((ch = getc(f))<=' ' && ch!=EOF);

	/* look for negative coord */
	if (ch == '~') { sign = 1; ch = getc(f); }

	/* read in the number itself */
	while (ch>='0' && ch<='9') {
	  coord *= 10;
	  coord += ch - '0';
	  ch = getc(f);
	}

	return(sign ? -coord : coord);
}	

/* read a token from the input file */
read_token(f,t)
  FILE *f;
  register char *t;
  {	register int ch;

	/* skip over leading blanks */
	while ((ch = getc(f))<=' ' && ch!=EOF);

	/* read in the number itself */
	while (ch > ' ' && ch!=EOF) {
	  if (ch == '~') *t++ = ' ';		/* embedded blank */
	  else *t++ = ch;
	  ch = getc(f);
	}
	*t = 0;
}	

/* read a .def file and return pointer to linked list of objects.  For now,
 * not much error checking is done...
 */
struct prototype *read_def(name)
  char *name;
  {	register int ch;
	register gptr p;
	struct prototype *d;
	FILE *in;
	char token[200],iname[100],buf[100];
	short x1,y1,x2,y2;

	/* see if we've already got a copy in core */
	for (d = directory; d != NULL; d = d->next)
	  if (strcmp(name,d->name) == 0) return(d);

	/* new widget, set up directory entry */
	d = (struct prototype *)malloc(sizeof(struct prototype));

	d->recent = cur_state;		/* Copy current parameters. */
	d->recent.curobj = d;
	d->recent.editee = NULL;
	d->recent.curx = d->recent.cury = 0;
	d->recent.oldx = d->recent.oldy = 0;
	d->recent.lxoff = d->recent.lyoff = 0;
	new_window(&d->recent,0,0);

	d->next = directory;
	directory = d;
	d->name = malloc((unsigned) (strlen(name) + 1));
	strcpy(d->name,name);
	d->body = NULL;
	d->modified = 0;

	strcpy(iname,name);
	strcat(iname,".def");
	if ((in = fopen(iname,"r")) == NULL) return(d);
	sprintf(buf,"reading in %s",iname);
	msg(buf);

	/* read through file processing commands */
	while ((ch = getc(in)) != EOF) switch (ch) {
	  case ' ':
	  case '\n':
	  case '\r':
	  case '\t':	continue;

	  case '|':	while ((ch = getc(in))!='\n' && ch!=EOF);
			continue;

	  case 'd':	read_token(in,token);
			continue;

	  case 'e':	goto done;

	  case 'm':	x1 = read_coord(in);
			y1 = read_coord(in);
			continue;

	  case 'A':
	  case 'l':	x2 = read_coord(in);
			y2 = read_coord(in);
			if (ch == 'A') read_token(in,token);	/* angle */
			if ((p = (gptr)malloc(sizeof(struct segment))) == NULL) {
			  msg("out of room!");
			  continue;
			}
			p->s.type = SEGMENT;
			p->s.selink = NULL;
			p->s.next = d->body;
			d->body = p;
			p->s.parent = d;
			p->s.x1 = x1;
			p->s.y1 = y1;
			p->s.x2 = x2;
			p->s.y2 = y2;
			if (ch == 'A') p->s.angle = atoi(token);
			else p->s.angle = 0;
			p->s.cache = NULL;
			newalist(&p->s,p->s.x1,p->s.y1,p->s.x2,p->s.y2);
			x1 = x2;
			y1 = y2;
			continue;

	  case 'c':	read_token(in,token);	/* label orientation */
			x2 = token[0];
			y2 = token[1];
			read_token(in,token);	/* the label itself */
			if ((p = (gptr)malloc(sizeof(struct label))) == NULL) {
			  msg("out of room!");
			  continue;
			}
			p->l.type = LABEL;
			p->l.selink = NULL;
			p->l.next = d->body;
			d->body = p;
			p->l.parent = d;
			p->l.x = x1;
			p->l.y = y1;
			switch (x2) {
			  default:
			  case 'c':	x2 = CC; break;
			  case 't':	x2 = TC; break;
			  case 'b':	x2 = BC; break;
			}
			switch (y2) {
			  default:
			  case 'c':	p->l.orient = CC + x2; break;
			  case 'l':	p->l.orient = CL + x2; break;
			  case 'r':	p->l.orient = CR + x2; break;
			}
			if ((p->l.string = malloc((unsigned) (strlen(token)+1))) == NULL) {
			  msg("out of room!");
			  continue;
			}
			strcpy(p->l.string,token);
			continue;

	  case 'i':	read_token(in,iname);	/* name of file */

			/* next is "___" or scale factor */
			read_token(in,token);
			if (token[0]>='0' && token[0]<='9')
			  sscanf(token,"%hd:%hd",&x2,&y2);
			else x2 = y2 = 1;

			read_token(in,token);	/* orientation */
			if ((p = (gptr)malloc(sizeof(struct object))) == NULL) {
			  msg("out of room!");
			  continue;
			}
			p->o.type = OBJECT;
			p->o.selink = NULL;
			p->o.next = d->body;
			d->body = p;
			p->o.parent = d;
			p->o.x = x1;
			p->o.y = y1;
			if (token[0] == 'r') {
			  p->o.orient = RNORTH;
			  token[0] = token[1];
			} else p->o.orient = NORTH;
			switch (token[0]) {
			  default:
			  case 'n':	ch = NORTH; break;
			  case 'e':	ch = EAST; break;
			  case 's':	ch = SOUTH; break;
			  case 'w':	ch = WEST; break;
			}
			p->o.orient = ocomp[p->o.orient][ch];
			p->o.proto = read_def(iname);
			p->o.mscale = x2;
			p->o.dscale = y2;
			continue;

	  default:	sprintf(buf,"%s: unrecognized .def command: 0%o, ESC continues...",d->name,ch);
			userinput("",buf);
			continue;
	}

  done:	fclose(in);
	clearprompt();
	return(d);
}

/* print .def file coordinate */
pcoord(f,n)
  FILE *f;
  {	if (n < 0) fprintf(f," ~%d",-n);
	else fprintf(f," %d",n);
}

/* write out a object list */
write_defn(p)
  register struct prototype *p;
  {	register gptr o;
	char *s,temp[100],buf[100];
	FILE *out;

	strcpy(temp,p->name);
	strcat(temp,".def");
	if ((out = fopen(temp,"w")) == NULL) {
	  sprintf(buf,"cannot open %s for output, ESC continues...",temp);
	  userinput("",buf);
	  return(1);
	}

	sprintf(buf,"writing out %s",temp);
	msg(buf);
	fprintf(out,"d main\n");

	for (o = p->body; o != NULL; o = o->s.next) {
	  putc('m',out);
	  pcoord(out,o->s.x1);
	  pcoord(out,o->s.y1);
	  putc(' ',out);
	  switch (o->s.type) {
	    case SEGMENT:
		if (o->s.angle == 0) {
		  putc('l',out);
		  pcoord(out,o->s.x2);
		  pcoord(out,o->s.y2);
		} else {
		  putc('A',out);
		  pcoord(out,o->s.x2);
		  pcoord(out,o->s.y2);
		  fprintf(out," %d",o->s.angle);
		}
		break;

	    case LABEL:
		fprintf(out,"c %s ",lorient[o->l.orient]);
		for (s = o->l.string; *s; s += 1)
		  putc(*s==' ' ? '~' : *s,out);
		break;
	
	    case OBJECT:
		fprintf(out,"i %s %d:%d %s",
			o->o.proto->name,
			o->o.mscale,o->o.dscale,
			oorient[o->o.orient]);
		break;
	  }
	  putc('\n',out);
	}

	fprintf(out,"e\n");
	fclose(out);
	clearprompt();
	p->modified = 0;	/* we've saved away changes */
	return(0);
}

/* remove an object from its prototype and reclaim its storage */
rmalist(q)
  register gptr q;
  {	register gptr r;

	for (; q != NULL; q = r) {
	  r = q->s.next;
	  free((char *)q);
	}
}

remove(p)
  register gptr p;
  {	register gptr q;

	p->s.parent->modified = 1;

	if ((q = p->s.parent->body) == p) p->s.parent->body = p->s.next;
	else {
	  while (q->s.next!=p && q!=NULL) q = q->s.next;
	  if (q != NULL) q->s.next = p->s.next;
	}

	if (p->s.type == LABEL) free(p->l.string);
	else if (p->s.type == SEGMENT) rmalist(p->s.cache);
	free((char *)p);
}

main(argc,argv)
  char **argv;
  {	register struct prototype *d;
	char *fname;
	char buf[100];

	fname = gentry(argc,argv); /* initialize display and keyboard */

	prompt = NULL;		/* not reading anything from user */
	typein[0] = 0;
	directory = NULL;

	cur_state.mscale = 1;	/* initial state settings */
	cur_state.dscale = 4;
	cur_state.csize = 16;
	cur_state.grid = 0;

	redisplay();		/* start with a fresh slate */

	if (fname == NULL) {
again:	  do if(userinput("","name of definition to edit: ")) goto done;
	  while (typein[0] == 0);
	  d = read_def(typein);
	} else d = read_def(fname);

	cur_state = d->recent;	/* Restore state at last edit */
	redisplay();

	while (1) {
	  extern char mousechanged;
	  if (mousechanged || UserReady()) {
	    if (command()) break;
	  } else {
	    DpyUp(0); }
	}
	d->recent = cur_state;

	/* check to see if we should write anything out */
	for (d = directory; d != NULL; d = d->next)
	  if (d->modified) {
	    sprintf(buf,"%s has not been written out, should it be? (y,n) ",d->name);
	    if (userinput("",buf))
	      goto again;
	    if (typein[0] == 'y')
	      if (write_defn(d)) goto again;
	  }

	/* return to system */
done:	gexit();
}
