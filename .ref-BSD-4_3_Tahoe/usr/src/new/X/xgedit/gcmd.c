#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

#ifndef lint
static char *rcsid_gcmd_c = "$Header: gcmd.c,v 10.6 86/02/01 16:18:44 tony Rel $";
#endif	lint

char *malloc(), *strcpy();

#include "gedit.h"

int mult = 1;		/* command multiplier */
int dx,dy,ends;		/* used during select operation */
int redo;		/* set to one if arc cache should be recalculated */
struct prototype *previous = NULL;	/* last proto to be edited */

/* process command input from user -- return 1 if it's time to exit, 0 otherwise */
command()
  {	register int ch;
	fptr cmd;
	extern char mousechanged;
	char buf[100];

	while (mousechanged) {
	  ch = track();
	  if (ch & (RECENTER+REDISPLAY)) {
	    if (ch & RECENTER) {
	      new_window(&cur_state,cur_state.curx,cur_state.cury);
	      remouse(cur_state.curx, cur_state.cury, 1);
	    }
	    redisplay();
	  }
	  if (!UserReady()) return(0);
	}

	ch = UserChar() & 0xFF;
	if (ch == 0377) return(0);

	cmd = dispatch[ch];
	if (cmd == NULL) {
	  Beep();
	  sprintf(buf, "unrecognized command: 0%o", ch);
	  msg(buf);
	} else {
	  ch = (*cmd)();
   	  if (!(ch & MULTIPLIER)) mult = 1;
	  if (ch & (RECENTER+REDISPLAY)) {
	    if (ch & RECENTER) {
	      new_window(&cur_state,cur_state.curx,cur_state.cury);
	      remouse(cur_state.curx, cur_state.cury, 1);
	    }
	    redisplay();
	  }
	  if (ch & DONE) return(1);
	}

	return(0);
}

/* go back to editing last thing we were working on */
editlast()
  {	struct prototype *temp;

	if (previous == NULL) return(0);

	if (temp = cur_state.curobj) temp->recent = cur_state;
	cur_state = previous->recent;
	previous = temp;
	return(REDISPLAY);
}

/* accept angle for SEGMENT */
angle()
  {	register gptr p;
	char buf[100];

	if ((p = cur_state.editee) == NULL) goto done;
	if (p->s.type != SEGMENT) goto done;
	sprintf(buf,"angle = %d, new value (2048 units in a circle): ",p->s.angle);
	if (userinput("",buf))
	  goto done;
	if (typein[0] == 0) goto done;
	p->s.angle = atoi(typein) & 2047;
	redo = 1;

  done:	return(0);
}

/* resize window and redisplay */
fixwindow()
  {	new_window(&cur_state,
		   (cur_state.worgx+cur_state.wmaxx)>>1,
		   (cur_state.worgy+cur_state.wmaxy)>>1);
	remouse(cur_state.curx, cur_state.cury, 1);
	return(REDISPLAY);
}

/* recenter window at cursor position */
setwindow()
  {	return(RECENTER);
}

/* frob orient parameter of selected object */
rotateobj()
  {	register gptr p;
	short temp;

	if ((p = cur_state.editee) != NULL) switch (p->s.type) {
	  case SEGMENT:	if (p->s.angle == 0) break;
			temp = p->s.x1; p->s.x1 = p->s.x2; p->s.x2 = temp;
			temp = p->s.y1; p->s.y1 = p->s.y2; p->s.y2 = temp;
			cur_state.whichend = 3 - cur_state.whichend;
			redo = 1;
			break;

	  case LABEL:	p->l.orient += 1;
			if (p->l.orient > BR) p->l.orient = CC;
			break;

	  case OBJECT:	p->o.orient += 1;
			if (p->o.orient > RWEST) p->o.orient = NORTH;
			break;
	}

	return(0);
}

/* edit selected object */
editobj()
  {	register gptr p;

	if ((p = cur_state.editee) != NULL) switch (p->s.type) {
	  case LABEL:	if (userinput(p->l.string,"edit label: ")) break;
			if (strlen(typein) <= strlen(p->l.string))
			  strcpy(p->l.string,typein);
			else {
			  free(p->l.string);
			  if ((p->l.string = malloc((unsigned)(strlen(typein)+1))) == NULL) {
			    msg("out of room!");
			    break;
			  }
			  strcpy(p->l.string,typein);
			}
			break;

	  case OBJECT:	cur_state.curobj->recent = cur_state;
			previous = cur_state.curobj;
			cur_state = p->o.proto->recent;
			return(REDISPLAY);
	}

	return(0);
}

/* rescale subpicture */
rescale()
  {	register gptr p;
	register int ch;
	char buf[100];
	int in1,in2;

	if ((p = cur_state.editee)==NULL || p->o.type!=OBJECT) return(0);
	sprintf(buf,"subpicture scale = %d:%d, new scale: ",p->o.mscale,p->o.dscale);
	if (userinput("",buf)) return(0);
	ch = sscanf(typein,"%d:%d",&in1,&in2);
	if (ch == 1) {
	  p->o.mscale = in1;
	  p->o.dscale = 1;
	} else if (ch == 2) {
	  p->o.mscale = in1;
	  p->o.dscale = in2;
	} else return(0);
	remouse(cur_state.curx, cur_state.cury, 1);
	return(REDISPLAY);
}

/* delete object */
delobj()
  {	register gptr p;

	if ((p = cur_state.editee) == NULL) return(0);
	deselect(0);
	remove(p);
	return(0);
}

/* abort current selection */
quit()
  {	deselect(REDISPLAY);
	return(0);
}

/* drop current selection where cursor is */
letgo()
  {	deselect(UPDATE+REDISPLAY);
	return(0);
}

/* drop current selection with same offset as last time */
lastgo()
  {	deselect(UPDATE+USEOFFSET+REDISPLAY);
	return(0);
}

/* select nearby object */
selobj()
  {	register gptr p;
	register int in1 = 0;

	if ((p = cur_state.editee) == NULL) {
	  if ((p = cur_state.curobj->body) == NULL) return(0);
	  in1 = 1;
	} else p = p->s.next;

	while (1) {
	  if (p == NULL) {
	    if (!in1) {
	      in1 = 1;
	      p = cur_state.curobj->body;
	      continue;
	    } else break;
          } else if (p == cur_state.editee) break;

	  if (nearby(p)) { selectobj(p,0); break; }
	  else p = p->s.next;
	}

	return(0);
}

/* create a new line segment */
newline()
  {	register gptr p;

	if ((p = (gptr)malloc(sizeof(struct segment))) == NULL)
	  msg("out of room!");
	else {
	  p->s.type = SEGMENT;
	  p->s.x2 = cur_state.curx;
	  p->s.y2 = cur_state.cury;
	  p->s.angle = 0;
	  p->s.cache = NULL;
	  newobj(p);
	}
	return(0);
}

/* finish creation of new object */
newobj(p)
  register gptr p;
  {	p->s.next = cur_state.curobj->body;
	cur_state.curobj->body = p;
	cur_state.curobj->modified = 1;
	p->s.parent = cur_state.curobj;
	p->s.x1 = cur_state.curx;
	p->s.y1 = cur_state.cury;
	dx = dy = 0;
	ends = 2;
	selectobj(p,1);
}

/* instantiate an object */
instantiate()
  {	register gptr p;
	register struct prototype *new;
	char buf[100];

	if (userinput("","name of definition to instantiate: ")) return(0);
	new = read_def(typein);
	if (new->body == NULL) {
	  sprintf(buf,"definition for %s not found",new->name);
	  msg(buf);
retdef:	  free(new->name);
	  directory = new->next;  /* remove dir entry */
	  free((char *)new);
	  return(0);
	};
	if ((p = (gptr)malloc(sizeof(struct object))) == NULL) {
	  msg("out of room!");
	  goto retdef;
	}
	p->o.type = OBJECT;
	p->o.orient = NORTH;
	p->o.proto = new;
	p->o.mscale = p->o.dscale = 1;
	newobj(p);
	return(0);
}

/* new label */
newlabel()
  {	register gptr p;

	if (userinput("","label: ")) return(0);
	if ((p = (gptr)malloc(sizeof(struct label))) == NULL) {
	  msg("out of room!");
	  return(0);
	}
	p->l.type = LABEL;
	p->l.orient = CC;
	if ((p->l.string = malloc((unsigned) (strlen(typein)+1))) == NULL) {
	  msg("out of room!");
	  free((char *) p);
	  return(0);
	}
	strcpy(p->l.string,typein);
	newobj(p);
	return(0);
}

/* start editing new .def file */
newin()
  {	if (userinput("","name of definition to edit: ") || typein[0]==0)
	  return(0);

	previous = cur_state.curobj;
	cur_state.curobj->recent = cur_state;
	cur_state = read_def(typein)->recent;
	return(REDISPLAY);
}

/* write out current .def file */
newout()
  {	if (userinput(cur_state.curobj->name,"name of output file: ")) return(0);
	if (typein[0] == 0) return(0);
	if (strcmp(typein,cur_state.curobj->name) != 0) {
	  if (strlen(typein) <= strlen(cur_state.curobj->name))
	    strcpy(cur_state.curobj->name,typein);
	  else {
	    free(cur_state.curobj->name);
	    if ((cur_state.curobj->name = malloc((unsigned) (strlen(typein)+1))) == NULL) {
	      msg("out of room!");
	      return(0);
	    }
	    strcpy(cur_state.curobj->name,typein);
	  }
	}
	write_defn(cur_state.curobj);
	banner();
	return(0);
}

/* snap cursor to grid */
snap()
  {	return(remouse(snap_coord(cur_state.curx),
		       snap_coord(cur_state.cury), 0));
}	

snap_coord(coord)
  register int coord;
  {	register int mask = ~(cur_state.csize - 1);

	coord += cur_state.csize >> 1;
	coord &= mask;
	return(coord);
}

/* move cursor to the right */
mright()
  {	return(remouse(cur_state.curx + mult*cur_state.csize,
		       cur_state.cury, 0));
}

/* move cursor to the left */
mleft()
  {	return(remouse(cur_state.curx - mult*cur_state.csize,
	       cur_state.cury, 0));
}

/* move cursor up */
mup()
  {	return(remouse(cur_state.curx,
		       cur_state.cury + mult*cur_state.csize, 0));
}

/* move cursor down */
mdown()
  {	return(remouse(cur_state.curx,
		       cur_state.cury - mult*cur_state.csize, 0));
}

/* make cursor larger */
curup()
  {	if (cur_state.csize < 128) cur_state.csize <<= 1;
	return(0);
}

/* make cursor smaller */
curdown()
  {	if (cur_state.csize > 1) cur_state.csize >>= 1;
	return(0);
}

/* multiply multiplier */
multiplier()
  {	mult <<= 2;
	return(MULTIPLIER);
}

/* rescale picture */
scale()
  {	register int ch;
	int in1,in2;

	if (userinput("","new scale (screen:defn): ")) return(0);
	ch = sscanf(typein,"%d:%d",&in1,&in2);
	if (ch == 1) {
	  cur_state.mscale = in1;
	  cur_state.dscale = 1;
	} else if (ch == 2) {
	  cur_state.mscale = in1;
	  cur_state.dscale = in2;
	} else return(1);

	return(RECENTER);
}

/* Scale picture up/down by factor of +n/-n:
 */

magnify(n)
 {	if (n<0)		/* Make it smaller?		*/
	 { n = -n;
	   if ((cur_state.mscale % n) == 0) cur_state.mscale /= n;
	   else cur_state.dscale *= n;
	 }
	else if (n > 0)
	 { if ((cur_state.dscale % n) == 0) cur_state.dscale /= n;
	   else cur_state.mscale *= n;
	 }
	return RECENTER;
 }

scaleup()
 {	return magnify(4);
 }
scaledn()
 {	return magnify(-4);
 }

/* move origin */
neworg()
 {	adj_org(cur_state.curobj,cur_state.curx,cur_state.cury);
	cur_state.curobj->modified = 1;
	remouse(0, 0, 0);
	return(RECENTER);
 }

/* move cursor to origin */
home()
  {	remouse(0, 0, 0);
	return(RECENTER);
}

/* finished editing current picture */
stop()
  {	deselect(UPDATE+REDISPLAY);
	return(DONE);
}

/* toggle grid flag */
toggle()
  {	cur_state.grid ^= 1;
	return(REDISPLAY);
}

/* get some input from the user, leave in typein array.  Return 1
 * if user aborted, 0 if he thinks there's something worth reading.
 */
userinput(seed,cue)
  char *seed,*cue;
  {	register int ch;
	register char *p = typein;
	char temp[100],save[100];
	int curcol,times,mch;

	strcpy(typein,seed);
	sprintf(temp,cue);
	prompt = temp;
	curcol = strlen(prompt);
	times = 1;
	mch = mousechanged;
	mousechanged = 0;
	goto redraw;

	while (1) {
	  if (mousechanged || UserReady()) {
	    if (mousechanged) {
		mch = 1;
		mousechanged = 0;
		continue;
	    }
	    switch (ch = UserChar() & 0xFF) {
	      default:	times = 1;
			if (ch < ' ' || ch > 0177) {
			  Beep();
			  break;
			}
			if (*p == 0) {		/* char at end */
			  *p++ = ch;
			  *p = 0;
			  disp_char(ch,curcol * chrwid,0,0,NORMAL,1);
			  curcol += 1;
			  break;
			}
			strcpy(save,p);		/* insert char */
			*p++ = ch;
			curcol += 1;
			strcpy(p,save);
			goto redraw;

	      case 'C'-0100:
	      case 'Q'-0100:
	      case 'G'-0100:
			prompt = NULL;
			clearprompt();
			mousechanged = mch;
			return(1);

	      case 'A'-0100:
			p = typein;
			curcol = strlen(prompt);
	      ctldone:	times = 1;
			break;

	      case 'B'-0100:
	      case 'H'-0100:
			while (p!=typein && times>0) {
			  times -= 1;
			  p -= 1;
			  curcol -= 1;
			}
			goto ctldone;

	      case 'D'-0100:
			while (*p && times>0) {
			  strcpy(p,p+1);
			  times -= 1;
			}
			goto redraw;

	      case 'E'-0100:
			for (p=prompt, curcol=0; *p; p+=1, curcol+=1);
			for (p = typein; *p; p += 1) curcol += 1;
			goto ctldone;

	      case 'F'-0100:
			while (*p && times>0) {
			  times -= 1;
			  p += 1;
			  curcol += 1;
			}
			goto ctldone;

	      case 'K'-0100:
			*p = 0;
			goto redraw;

	      case 'Y'-0100:
			new_window(&cur_state,cur_state.curx,cur_state.cury);
	      case 'L'-0100:
			redisplay();
			goto redraw;

	      case 'U'-0100:
			times <<= 2;
			break;

	      case '\r':
	      case '\n':
	      case 033:	prompt = NULL;
			clearprompt();
			mousechanged = mch;
			return(0);

	      case 0177:
			while (p!=typein && times>0) {
			  strcpy(p-1,p);
			  p -= 1;
			  curcol -= 1;
			  times -= 1;
			}
			goto redraw;

	      redraw:	msg(prompt);
			incol += disp_str(typein, incol * chrwid, 0, 0, NORMAL,1);
			times = 1;
			break;
	    }
	  } else {
	    incol = curcol;
	    DpyUp(1);
	  }
	}
}

/* given center, calculate various window coords */
new_window(s,cx,cy)
  register struct state *s;
  {	int wx,wy;

	/* half-width of window in defn coords */
	wx = (wmaxx - wminx) >> 1;
	wx *= s->dscale;
	wx /= s->mscale;

	/* half-height of window in defn coords */
	wy = (wmaxy - wminy) >> 1;
	wy *= s->dscale;
	wy /= s->mscale;

	s->worgx = cx - wx;
	s->worgy = cy - wy;
	s->wmaxx = cx + wx;
	s->wmaxy = cy + wy;
}

#define abs(x) ((x) < 0 ? -(x) : (x))

/* see if an object is "near enough" to the cursor, sets dx, dy, ends */
nearby(p)
  register gptr p;
  {	switch (p->s.type) {
	  case SEGMENT:	dx = p->s.x2 - cur_state.curx;
			dy = p->s.y2 - cur_state.cury;
	  		if (abs(dx) <= cur_state.csize &&
			    abs(dy) <= cur_state.csize)  {
			  ends = 2;
			  return(1);
			}
	  case OBJECT:
	  case LABEL:	dx = p->s.x1 - cur_state.curx;
			dy = p->s.y1 - cur_state.cury;
			if (abs(dx) <= cur_state.csize &&
			    abs(dy) <= cur_state.csize)  {
			  ends = 1;
			  return(1);
			}
			break;
	}

	return(0);
}

/* unselect currently selected object:
 *	update & UPDATE		update object's info
 *				+ USEOFFSET -- use lxoff, lyoff
 *	update & REDISPLAY	redisplay when done
 */
deselect(update)
  {	register gptr p = cur_state.editee;
	register int temp;
	gptr save;

	if (p == NULL) return;		/* nothing to do */

	if (update & UPDATE) {
	  switch (p->s.type) {
	    case SEGMENT:	if (cur_state.whichend == 2) {
				  if (update & USEOFFSET) {
				    p->s.x2 += cur_state.lxoff;
				    p->s.y2 += cur_state.lyoff;
				  } else {
				    temp = cur_state.curx + cur_state.xoff;
				    cur_state.lxoff = temp - p->s.x2;
				    p->s.x2 = temp;
				    temp = cur_state.cury + cur_state.yoff;
				    cur_state.lyoff = temp - p->s.y2;
				    p->s.y2 = temp;
				  }
			  	  break;
				};
	    case LABEL:
	    case OBJECT:	if (update & USEOFFSET) {
				  p->s.x1 += cur_state.lxoff;
				  p->s.y1 += cur_state.lyoff;
				} else {
				  temp = cur_state.curx + cur_state.xoff;
				  cur_state.lxoff = temp - p->s.x1;
				  p->s.x1 = temp;
				  temp = cur_state.cury + cur_state.yoff;
				  cur_state.lyoff = temp - p->s.y1;
				  p->s.y1 = temp;
				}
			  	break;
	  }
	  cur_state.curobj->modified = 1;
	}

	if (p->s.type == SEGMENT) newalist(&p->s,p->s.x1,p->s.y1,p->s.x2,p->s.y2);

	cur_state.editee = NULL;

	/* to update display we only have to redraw selected object */
	if (update & REDISPLAY) {
	  save = p->s.next;
	  p->s.next = NULL;
	  display(p,0,0,NORTH,1,1,NORMAL);
	  p->s.next = save;
	}
}

/* update database to reflect newly selected object */
selectobj(p,new)
  register gptr p;
  {	deselect(UPDATE+REDISPLAY);
	cur_state.editee = p;
	cur_state.xoff = 0;
	cur_state.yoff = 0;
	cur_state.whichend = ends;
	if (!new) {
	      if (remouse(cur_state.curx+dx, cur_state.cury+dy,1)) {
		new_window(&cur_state,cur_state.curx+dx,cur_state.cury+dy);
		remouse(cur_state.curx, cur_state.cury, 1);
	      }
	      redisplay();
	}
}

/* redisplay entire screen */
redisplay()
 {
	clearscreen();
	if (cur_state.curobj != NULL) {
	  display(cur_state.curobj->body,0,0,NORTH,1,1,NORMAL);
	  ctext("*",0,0,CC,NORMAL);
	}

	/* grid points fall every csize points */
	if (cur_state.grid) disp_grid();

	return(0);	/* return code when used as a command */
}

/* return appropriately oriented x and y coords */
xorient(x,y,orient)
  {	switch (orient) {
	  case NORTH:	return(x);
	  case EAST:	return(y);
	  case SOUTH:	return(-x);
	  case WEST:	return(-y);
	  case RNORTH:	return(-x);
	  case REAST:	return(-y);
	  case RSOUTH:	return(x);
	  case RWEST:	return(y);
	}
	/*NOTREACHED*/
}

yorient(x,y,orient)
  {	switch (orient) {
	  case NORTH:	return(y);
	  case EAST:	return(-x);
	  case SOUTH:	return(-y);
	  case WEST:	return(x);
	  case RNORTH:	return(y);
	  case REAST:	return(-x);
	  case RSOUTH:	return(-y);
	  case RWEST:	return(x);
	}
	/*NOTREACHED*/
}

/* display objects with specified translation and rotation */
display(o,x,y,orient,mscale,dscale,dflag)
  register gptr o;
  {	short tx,ty;
	short ex,ey;

	while (o != NULL) {
	  tx = xorient(o->l.x, o->l.y, orient);
	  tx *= mscale; tx /= dscale;
	  tx += x;
	  ty = yorient(o->l.x, o->l.y, orient);
	  ty *= mscale; ty /= dscale;
	  ty += y;

	  if (cur_state.editee != o) switch (o->s.type) {
	    case LABEL:
		ctext(o->l.string,tx,ty,lcomp[orient][o->l.orient],dflag);
		break;

	    case OBJECT:
		display(o->o.proto->body,tx,ty,
			ocomp[orient][o->o.orient],
			mscale * o->o.mscale, dscale * o->o.dscale,
			dflag);
		break;

	    case SEGMENT:
		if (o->s.cache != NULL) {
		  display(o->s.cache,x,y,orient,mscale,dscale,dflag);
		  break;
		}
		ex = xorient(o->s.x2, o->s.y2, orient);
		ex *= mscale; ex /= dscale;
		ex += x;
		ey = yorient(o->s.x2, o->s.y2, orient);
		ey *= mscale; ey /= dscale;
		ey += y;
		cline(tx,ty,ex,ey,dflag);
		break;
	  }
	  o = o->s.next;
	}
}

/* output a message on the bottom-most line */
msg(string)
  char *string;
  {
	clearprompt();
	incol += disp_str(string, incol * chrwid, 0, 0, NORMAL,1);
}

/* display text string with proper orientation clipped by current window */
ctext(string,x,y,orient,dflag)
  register char *string;
  {	register int i = (strlen(string) * chrwid) >> 1;

	/* first translate and scale to the window coord system */
	y -= cur_state.worgy;
	x -= cur_state.worgx;

	y *= cur_state.mscale; y /= cur_state.dscale;
	x *= cur_state.mscale; x /= cur_state.dscale;

	/* adjust for character orientation */
	switch (orient) {
	  case CC:	x -= i; y -= (chrhgt >> 1); break;
	  case TC:	x -= i; y -= chrhgt; break;
	  case BC:	x -= i; break;
	  case CL:	y -= (chrhgt >> 1); break;
	  case TL:	y -= chrhgt; break;
	  case BL:	break;
	  case CR:	x -= i+i; y -= (chrhgt >> 1); break;
	  case TR:	x -= i+i; y -= chrhgt; break;
	  case BR:	x -= i+i;
	}

	/* only display chars that lie within current window */
	x += wminx;
	y += wminy;
	if (y <= wmaxy && y+chrhgt >= wminy &&
	    x <= wmaxx && x+(strlen(string)*chrwid) >= wminx)
	  disp_str(string,x,y,0,dflag,0);
}

/* draw a clipped vector */
#define code(x,y) \
 ((x<wminx ? 1 : x>wmaxx ? 2 : 0) + (y<wminy ? 4 : y>wmaxy ? 8 : 0))

cline(fx,fy,tx,ty,dflag)
  {

	/* first translate and scale to the window coord system */
	fx -= cur_state.worgx;
	fy -= cur_state.worgy;
	tx -= cur_state.worgx;
	ty -= cur_state.worgy;

	fy *= cur_state.mscale; fy /= cur_state.dscale;
	fx *= cur_state.mscale; fx /= cur_state.dscale;
	ty *= cur_state.mscale; ty /= cur_state.dscale;
	tx *= cur_state.mscale; tx /= cur_state.dscale;

	fx += wminx; fy += wminy;
	tx += wminx; ty += wminy;

	if ((code(fx,fy) & code(tx,ty)) == 0)
	    line(fx,fy,tx,ty,dflag);
}

/* display grid points at csize intervals */
disp_grid()
  {	int x,y,incrx,incry;

	/* find window coord of lower left grid point */
	x = cur_state.worgx + cur_state.csize - 1;
	x &= ~(cur_state.csize - 1);
	x -= cur_state.worgx;
	x *= cur_state.mscale; x /= cur_state.dscale;

	y = cur_state.worgy + cur_state.csize - 1;
	y &= ~(cur_state.csize - 1);
	y -= cur_state.worgy;
	y *= cur_state.mscale; y /= cur_state.dscale;

	x += wminx;
	y = wmaxy - y - wminy;

	incrx = (cur_state.csize * cur_state.mscale) / cur_state.dscale;
	if (incrx <= 0) incrx = 8;
	else while (incrx < 8) incrx <<= 1;
	incry = incrx;

	drawgrid(x,y,incrx,incry);
}

/* Xor drawing cursor and selected object */
dcurxor()
  {	register gptr p;
	int x,y;

	if (cur_state.curobj == NULL) return;	/* no object on screen */

	if ((p = cur_state.editee) != NULL) {
	  if (p->s.type == SEGMENT && (redo || (p->s.angle != 0 &&
		(cur_state.curx!=cur_state.oldx ||
		 cur_state.cury!=cur_state.oldy)))) {
	    cur_state.oldx = cur_state.curx;
	    cur_state.oldy = cur_state.cury;
	    redo = 0;
	    if (cur_state.whichend == 2)
	      newalist(&p->s,p->s.x1,p->s.y1,
			 cur_state.curx + cur_state.xoff,
			 cur_state.cury + cur_state.yoff);
	    else
	      newalist(&p->s,cur_state.curx + cur_state.xoff,
			 cur_state.cury + cur_state.yoff,
			 p->s.x2,p->s.y2);
	  }
	  switch(p->s.type) {
	    case SEGMENT:
		if (p->s.cache != NULL) {
		  for (p = p->s.cache; p != NULL; p = p->s.next)
		    cline(p->s.x1,p->s.y1,p->s.x2,p->s.y2,HIGHLIGHT);
		  break;
		}
		if (cur_state.whichend == 2) { x = p->s.x1; y = p->s.y1; }
		else { x = p->s.x2; y = p->s.y2; }
		cline(x,y,
		      cur_state.curx + cur_state.xoff,
		      cur_state.cury + cur_state.yoff,HIGHLIGHT);
		break;

	    case LABEL:
		ctext(p->l.string,
		      cur_state.curx + cur_state.xoff,
		      cur_state.cury + cur_state.yoff,
		      p->l.orient,
		      HIGHLIGHT);
		break;

	    case OBJECT:
		display(p->o.proto->body,
		        cur_state.curx + cur_state.xoff,
		        cur_state.cury + cur_state.yoff,
		        p->o.orient,p->o.mscale,p->o.dscale,
		        HIGHLIGHT);
		return;	/* no cursor needed here */
	  }
	}

	/* draw x-shaped graphic cursor */
	cline(cur_state.curx - cur_state.csize,
	      cur_state.cury - cur_state.csize,
	      cur_state.curx + cur_state.csize,
	      cur_state.cury + cur_state.csize,
	      HIGHLIGHT);
	cline(cur_state.curx - cur_state.csize,
	      cur_state.cury + cur_state.csize,
	      cur_state.curx + cur_state.csize,
	      cur_state.cury - cur_state.csize,
	      HIGHLIGHT);
}

/* Help stuff */
Help()
 {	struct prototype *help_proto;
	char buf[100];

	help_proto = read_def(HELPFILE);	/* Load the file, if any. */

	if (help_proto->body == NULL) {
	   sprintf(buf, "Can't read '%s.DEF' -- NO HELP", HELPFILE);
	   msg(buf);
	   return 0;
	}
	previous = cur_state.curobj;
	if (cur_state.curobj) cur_state.curobj->recent = cur_state;
	cur_state = help_proto->recent;
	return REDISPLAY;
 }

/* Adjust coordinates in body of an object by the origin of that object.
 * This routine is called after an object is edited, so that an adjusted
 * subpicture origin is reflected immediately in pictures which reference
 * it.
 */
adj_org(p,adjx,adjy)
  register struct prototype *p;
  {	register gptr o;
	struct prototype temp;

	for (o = p->body; o != NULL; o = o->s.next) switch (o->s.type) {
	  case SEGMENT:	o->s.x1 -= adjx;
			o->s.y1 -= adjy;
			o->s.x2 -= adjx;
			o->s.y2 -= adjy;
			if (o->s.cache) {
			  temp.body = o->s.cache;
			  adj_org(&temp,adjx,adjy);
			}
			break;

	  case LABEL:	o->l.x -= adjx;
			o->l.y -= adjy;
			break;

	  case OBJECT:	o->o.x -= adjx;
			o->o.y -= adjy;
			break;
	}
}
