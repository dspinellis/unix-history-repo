/* Copyright 1985, Massachusetts Institute of Technology */

/*
 * X Unix performance monitor.
 */

#ifndef lint
static char *rcsid_xperfmon_c = "$Header: xperfmon.c,v 10.13 86/11/25 18:31:37 jg Rel $";
#endif	lint

/*
 * Simple graphical performance monitor for system-wide data.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/vm.h>
#include <sys/dkstat.h>
#include <nlist.h>
#include <sys/buf.h>
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>
#endif vax
#ifdef sun
#include <sundev/mbvar.h>
#endif sun
#ifdef ibm032	/* IBM RT/PC */
#include <caio/ioccvar.h>
#endif ibm032
#ifdef tahoe
#include <tahoevba/vbavar.h>
#endif
#include <X/Xlib.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/file.h>
#include <sys/time.h>

#include <strings.h>

#define	USEC_INC	50000
#define	SEC_INC		1

struct packet {
	int	input, output, collisions;
};
static	struct packet packets, old_packets;

#define NUM_VALS_PER	1000
struct statistic {
	int	min_val, max_val;
	int	value[NUM_VALS_PER];
	char	*label, *label2;
};

#define SECS_PER_TIME_TICK	10
static	char do_time[NUM_VALS_PER];
static	struct timeval current_time, saved_time;
static	struct timezone dummy_zone;

short gray_bits[16] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555};

/*
 * The array stats always has valid info for stats[i], 0 <= i < num_stats.
 * For each valid stats[i], stats[i].value[j] is valid for 0 <= j < num_of_val.
 * The info for the k-th possible statistic of interest is recorded, if it is
 *   recorded at all, in stats[possible_stats[k]].
 */

#define	NO_STAT			-1
#define USER_CPU_PERCENTAGE	0
#define SYSTEM_CPU_PERCENTAGE	1
#define IDLE_CPU_PERCENTAGE	2
#define FREE_MEM		3
#define DISK_TRANSFERS		4
#define INTERRUPTS		5
#define INPUT_PACKETS		6
#define OUTPUT_PACKETS		7
#define COLLISION_PACKETS	8
#define NUM_POSSIBLE_STATS	9
static	int			possible_stats[NUM_POSSIBLE_STATS];
#define WANT_STAT(x)		(possible_stats[(x)] != NO_STAT)

#define	MAX_STATS		10

#define DEFAULT_BORDER_WIDTH	3
#define DEFAULT_POSITION	"=%dx%d-0+0"

static	struct statistic	stats[MAX_STATS];

static	struct timeval timeout = {
	SEC_INC,USEC_INC};
static	int num_stats, num_of_val = 0;
static	int graph_x_offset = 0;
WindowInfo	WInfo;

Window Win;
char Host[40];
char *font_name = "6x10";
int background;			    	    /* color of background */
int foreground;			    	    /* color of graph */
int highlight;				    /* color of text, scale */
FontInfo *finfo;			    /* font information needed */
int debug = 0;
#define max(a,b) (a>b ? a:b)

OpaqueFrame win;

#define FORALLPOSSIBLESTATS(stat)\
	for (stat = 0; stat < NUM_POSSIBLE_STATS; stat++)
#define FORALLSTATS(stat) for (stat = 0; stat < num_stats; stat++)

struct nlist nl[] = {
#define X_CPTIME	0
	{ "_cp_time" },
#define X_RATE	  1
	{ "_rate" },
#define X_TOTAL	 2
	{ "_total" },
#define X_DEFICIT       3
	{ "_deficit" },
#define X_FORKSTAT      4
	{ "_forkstat" },
#define X_SUM	   5
	{ "_sum" },
#define X_FIRSTFREE     6
	{ "_firstfree" },
#define X_MAXFREE       7
	{ "_maxfree" },
#define X_BOOTTIME      8
	{ "_boottime" },
#define X_DKXFER	9
	{ "_dk_xfer" },
#define X_REC	   10
	{ "_rectime" },
#define X_PGIN	  11
	{ "_pgintime" },
#define X_HZ	    12
	{ "_hz" },
#define X_MBDINIT       13
	{ "_mbdinit" },
#define N_IFNET       14
	{ "_ifnet" },
#define X_UBDINIT	15
	{ "_ubdinit" },
#define X_IOCINIT	16
	{ "_ioccdinit" },
#define	X_VBDINIT	17
	{ "_vbdinit" },
	{ "" },
};

char dr_name[DK_NDRIVE][10];
char dr_unit[DK_NDRIVE];
double  stat1();
int     maxfree;
int     hz;
struct
{
	int     busy;
	long    time[CPUSTATES];
	long    xfer[DK_NDRIVE];
	struct  vmmeter Rate;
	struct  vmtotal Total;
	struct  vmmeter Sum;
	struct  forkstat Forkstat;
	unsigned rectime;
	unsigned pgintime;
} 
s, s1;
#define rate	    s.Rate
#define total	   s.Total
#define sum	     s.Sum
#define forkstat	s.Forkstat

struct  vmmeter osum;
int     deficit;
double  etime;
int     mf;
int     swflag;

int nintv;
long t;

#define steal(where, var) lseek(mf, where, 0); read(mf, &var, sizeof var);
#define pgtok(a) ((a)*NBPG/1024)

char	*options[NUM_POSSIBLE_STATS+1] = {
	"user", "system", "idle", "free", "disk", "interrupts",
	"input", "output", "collision",
	0  /* Terminator! */  };

short  arrow []=  {0x0000, 0x0020, 0x0070, 0x00f8, 0x01fc, 0x03fe, 0x0070,
		   0x0070, 0x0070, 0x0070, 0x0070, 0x0070, 0x0070, 0x0000};
short  mask []=   {0x0020, 0x0070, 0x00f8, 0x01fc, 0x03fe, 0x07ff, 0x07ff,
		   0x00f8, 0x00f8, 0x00f8, 0x00f8, 0x00f8, 0x00f8, 0x00f8};


main(argc, argv)
int argc;
char **argv;
{
	int	stat;
	int	have_disk;
	struct 	timeval timeleft;
	char 	display[40];
	char 	*strind;
	int 	Select_mask, select_mask = 0;
	int 	maxplus1, n;
	Cursor 	cursor;
	char 	*geometry = NULL;		/* location of window */
	char 	def[32];
	int 	reverse = 0;
	double 	update = -1.;
	double	atof();
	char 	*border_color;
	char 	*fore_color;
	char 	*back_color;
	char 	*high_color;
	Pixmap 	border_pixmap;
	char 	*option;
	int 	opt;
	int	i;
	int	minheight, minwidth;
	
	Color cdef;
	int border_width = DEFAULT_BORDER_WIDTH;

	display[0] = '\0';

	if ((option = XGetDefault(argv[0],"ReverseVideo")) != NULL )
		if (strcmp (option, "on") == 0)
			reverse = 1;
	if ((option = XGetDefault(argv[0],"BorderWidth")) != NULL)
		border_width = atoi(option);
	if ((option = XGetDefault(argv[0],"BodyFont")) != NULL)
		font_name = option;
	if ((border_color = XGetDefault(argv[0],"Border")) == NULL)
		border_color = XGetDefault(argv[0],"BorderColor");
	back_color = XGetDefault(argv[0],"Background");
	fore_color = XGetDefault(argv[0],"Foreground");
	high_color = XGetDefault(argv[0],"Highlight");
	if ((option = XGetDefault(argv[0],"Update")) != NULL)
		update = atof(option);

	nintv = get_namelist("/vmunix", "/dev/kmem");
	collect_stats();
	etime = 1.0;
	have_disk = (total_disk_transfers() ? 1 : 0);

	/* Initialize stats */
	FORALLPOSSIBLESTATS(stat)
		possible_stats[stat] = NO_STAT;
	num_stats = 0;
	for (i = 1; i < argc; i++) {  	                /* Parse line */
		if (argv[i][0] == '=') {
			geometry = argv[i];
			continue;
		}
		if (index(argv[i], ':') != NULL) {	/* host:display */
			strncpy(display, argv[i], sizeof(display));
			continue;
		}
		if (strcmp(argv[i], "-rv") == 0 ||
		strcmp(argv[i], "-reverse") == 0) {	/* black on white */
			reverse = 1;
			continue;
		}
		if (strcmp(argv[i], "-fw") == 0 ||
		strcmp(argv[i], "-forward") == 0) {	/* white on black */
			reverse = 0;
			continue;
		}
		if (strcmp(argv[i], "-bw") == 0 ||
		strcmp(argv[i], "-border") == 0) {	/* border width */
			if (++i >= argc) usage();
			border_width = atoi(argv[i]);
			continue;
		}
		if (strcmp(argv[i], "-fn") == 0 ||
		strcmp(argv[i], "-font") == 0) {	/* host name font */
			if (++i >= argc) usage();
			font_name = argv[i];
			continue;
		}
		if (strcmp(argv[i], "-bd") == 0 ||
		strcmp(argv[i], "-color") == 0) {	/* border color */
			if (++i >= argc) usage();
			border_color = argv[i];
			continue;
		}
		if (strcmp(argv[i], "-fg") == 0 ||
		strcmp(argv[i], "-foreground") == 0) {  /* foreground color */
			if (++i >= argc) usage();
			fore_color = argv[i];
			continue;
		}
		if (strcmp(argv[i], "-bg") == 0 ||
		strcmp(argv[i], "-background") == 0) {  /* background color */
			if (++i >= argc) usage();
			back_color = argv[i];
			continue;
		}
		if (strcmp(argv[i], "-hl") == 0 ||
		strcmp(argv[i], "-highlight") == 0) {   /* highlight color */
			if (++i >= argc) usage();
			high_color = argv[i];
			continue;
		}
		if (strcmp(argv[i], "-u") == 0 ||
		strcmp(argv[i], "-update") == 0) {	/* update interval */
			if (++i >= argc) usage();
			update = atof(argv[i]);
			continue;
		}
		opt = getcmd(argv[i], options);
		if (opt >= 0 && opt < NUM_POSSIBLE_STATS) {
			if (num_stats == MAX_STATS) {
				fprintf(stderr,
				    "MAX_STATS exceeded, please recompile!\n");
			}
			else possible_stats[opt] = num_stats++;
			continue;
		}
		usage();
	}
	
	if (num_stats == 0)
		FORALLPOSSIBLESTATS(stat) {
			if ((stat == DISK_TRANSFERS) && (have_disk == 0)) continue;
			possible_stats[stat] = num_stats++;
			if (num_stats == MAX_STATS) break;
		}
	have_disk = 0;	/* so max # of packets = 40 */
	init_stat(USER_CPU_PERCENTAGE, 100, "User", " CPU");
	init_stat(SYSTEM_CPU_PERCENTAGE, 100, "System", " CPU");
	init_stat(IDLE_CPU_PERCENTAGE, 100, "Idle", " CPU");
	init_stat(FREE_MEM, pgtok(maxfree), "Free", " memory");
	init_stat(DISK_TRANSFERS, 40, "Disk", " transfers");
	init_stat(INTERRUPTS, 60, "Interrupts", "");
	init_stat(INPUT_PACKETS, (have_disk ? 20 : 40), "Input", " packets");
	init_stat(OUTPUT_PACKETS, (have_disk ? 20 : 40), "Output", " packets");
	init_stat(COLLISION_PACKETS, 10, "Collision", " packets");
	if (border_width < 0) border_width = DEFAULT_BORDER_WIDTH;
	if (update > .09) {
		timeout.tv_sec = update;
		timeout.tv_usec = (update - (double)((int)update)) * 1000000.;
	}
	if (!XOpenDisplay (display)){
 	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    argv[0], XDisplayName(display));
	    exit(1);
	}
	if ((finfo = XOpenFont(font_name)) == NULL) {
		fprintf(stderr, "Can't load font %s!\n", font_name);
		exit(1);
	}
	gethostname(Host, sizeof (Host));
	strcat(Host, ":");
	FORALLSTATS(stat) {
		int s_width;
		s_width = XStringWidth (stats[stat].label, finfo, 0, 0);
		graph_x_offset = max(graph_x_offset, s_width);
		s_width = XStringWidth (stats[stat].label2, finfo, 0, 0);
		graph_x_offset = max(graph_x_offset, s_width);
	}
	graph_x_offset += 15;
	if(debug) fprintf(stderr, "graph_x_offset=%d\n", graph_x_offset);
	gettimeofday(&saved_time, &dummy_zone);

	if (border_color && DisplayCells() > 2 &&
    		XParseColor(border_color, &cdef) && XGetHardwareColor(&cdef))
		border_pixmap = XMakeTile(cdef.pixel);
	else if (border_color && strcmp(border_color, "black") == 0)
		border_pixmap = BlackPixmap;
	else if (border_color && strcmp(border_color, "white") == 0)
		border_pixmap = WhitePixmap;
	else	
    		border_pixmap = XMakePixmap (
			(Bitmap) XStoreBitmap (16, 16, gray_bits),
					BlackPixel, WhitePixel);



	if (back_color && DisplayCells() > 2 &&
	    XParseColor(back_color, &cdef) && XGetHardwareColor(&cdef)) {
		background = cdef.pixel;
	} else if (back_color && (strcmp(back_color, "white") == 0)) {
		background = WhitePixel;
		reverse = 0;
	} else if (back_color && (strcmp(back_color, "black") == 0)) {
		background = BlackPixel;
		reverse = 0;
	} else
	    background = BlackPixel;

	if (fore_color && DisplayCells() > 2 &&
	    XParseColor(fore_color, &cdef) && XGetHardwareColor(&cdef)) {
		foreground = cdef.pixel;
	} else if (fore_color && (strcmp(fore_color, "black") == 0)) {
		foreground = BlackPixel;
		reverse = 0;
	} else if (fore_color && (strcmp(fore_color, "white") == 0)) {
		foreground = WhitePixel;
		reverse = 0;
	} else
	    foreground = WhitePixel;

	if (high_color && DisplayCells() > 2 &&
	    XParseColor(high_color, &cdef) && XGetHardwareColor(&cdef)) {
		highlight = cdef.pixel;
	} else
	    highlight = foreground;

	if (reverse) {
		highlight = background;
		background = foreground;
		foreground = highlight;
	}
	win.bdrwidth = border_width;
	win.border = border_pixmap;
	win.background = XMakeTile(background);

	minheight = (finfo->height * 2 + 2) * num_stats;
	minwidth  = graph_x_offset + 100;
	sprintf(def, DEFAULT_POSITION, minwidth+100, 
		(finfo->height * 3 + 3) * num_stats);

	Win = XCreate ("Performance Monitor", argv[0], geometry, def, &win,
		minwidth, minheight);

	win.height -= 10;
	XMapWindow (Win);
	cursor = XCreateCursor (11, 14, arrow, mask, 5, 1, 1, 0, GXcopyInverted);
	XDefineCursor (Win, cursor);

	redisplay (Win);
	timeleft = timeout;
	Select_mask = 1<<dpyno();
	maxplus1 = 1+dpyno();
	XSelectInput(Win, KeyPressed | ExposeWindow | ExposeCopy);
	while(1) {
		select_mask = Select_mask;
		if(debug) fprintf(stderr, "time=[%d,%d]\n",
		timeleft.tv_sec, timeleft.tv_usec);
		XFlush();
		if ((n = select(maxplus1, &select_mask, NULL, NULL, &timeleft))
		    < 0) exit(46);
		if(debug)
			fprintf(stderr,"selected n=%d mask=0x%x, time=[%d,%d]\n",
			n, select_mask, timeleft.tv_sec, timeleft.tv_usec);
		if (perf_mon_selected (Win, n, select_mask, &timeleft)
		    < 0) break;
	}
}

getcmd(to_match, table)			/* Modified from ucb/lpr/lpc.c */
register char *to_match;
register char **table;
{
	register char *p, *q;
	int found, index, nmatches, longest;

	longest = nmatches = 0;
	found = index = -1;
	for (p = *table; p; p = *(++table)) {
		index++;
		for (q = to_match; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return(index);
		if (!*q) {			/* the to_match was a prefix */
			if (q - to_match > longest) {
				longest = q - to_match;
				nmatches = 1;
				found = index;
			} 
			else if (q - to_match == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return(-1);
	return(found);
}

init_stat(index, maxval, label_1, label_2)
int index, maxval;
char *label_1, *label_2;
{
	if WANT_STAT(index) {
		index = possible_stats[index];
		stats[index].max_val = maxval;
		stats[index].label = label_1;
		stats[index].label2 = label_2;
	}
}

#define	TIMER_EXPIRED(timer)						\
(*timer && ((*timer)->tv_sec == 0) && ((*timer)->tv_usec == 0))

int perf_mon_selected(w, number, mask, timer)
int mask, number;
Window w;
struct	timeval *timer;
{
	if(number == 0) {	/*timer expired */
		int *target[CPUSTATES-1], trash;
		collect_stats();
		for (trash = 0; trash < CPUSTATES-1; trash++)
			target[trash] = &trash;
		if WANT_STAT(USER_CPU_PERCENTAGE)
			target[0] =
			    &stats[possible_stats[USER_CPU_PERCENTAGE]].value[num_of_val];
		if WANT_STAT(SYSTEM_CPU_PERCENTAGE)
			target[1] =
			    &stats[possible_stats[SYSTEM_CPU_PERCENTAGE]].value[num_of_val];
		if WANT_STAT(IDLE_CPU_PERCENTAGE)
			target[2] =
			    &stats[possible_stats[IDLE_CPU_PERCENTAGE]].value[num_of_val];
		copy_cpu_stats(target);
		if WANT_STAT(FREE_MEM)
			stats[possible_stats[FREE_MEM]].value[num_of_val] =
			    pgtok(total.t_free);
		if WANT_STAT(DISK_TRANSFERS)
			stats[possible_stats[DISK_TRANSFERS]].value[num_of_val] =
			    total_disk_transfers();
		if WANT_STAT(INTERRUPTS)
			stats[possible_stats[INTERRUPTS]].value[num_of_val] =
			    (rate.v_intr/nintv) - hz;
		if WANT_STAT(INPUT_PACKETS)
			stats[possible_stats[INPUT_PACKETS]].value[num_of_val] =
			    packets.input - old_packets.input;
		if WANT_STAT(OUTPUT_PACKETS)
			stats[possible_stats[OUTPUT_PACKETS]].value[num_of_val] =
			    packets.output - old_packets.output;
		if WANT_STAT(COLLISION_PACKETS)
			stats[possible_stats[COLLISION_PACKETS]].value[num_of_val] =
			    packets.collisions - old_packets.collisions;
		gettimeofday(&current_time, &dummy_zone);
		if (current_time.tv_sec < saved_time.tv_sec) {
			/* Super-user must have set the clock back */
			saved_time = current_time;
			saved_time.tv_sec -= SECS_PER_TIME_TICK;
		}
		if (saved_time.tv_sec+SECS_PER_TIME_TICK <= current_time.tv_sec) {
			saved_time = current_time;
			do_time[num_of_val] = 1;
		} 
		else
			do_time[num_of_val] = 0;
		next_display(w);
	}
	if (mask & (1 << dpyno())){
		XEvent event;
		XEvent pevent;
		XExposeWindowEvent *exp_event;
		int key;
		if(!XPending()) return (-1); /* end of file on connection */
		while (XPending())
		  {
			XNextEvent (&event);
			switch (event.type) {
			case KeyPressed:
				if ((key = mapkey(((XKeyPressedEvent *)&event)->detail)) > 0)
					switch(key){
					case 'f': /* faster usec timeout */
						if (timeout.tv_usec >= USEC_INC)
							timeout.tv_usec -= USEC_INC;
						else {
							if (timeout.tv_sec >= SEC_INC) {
								timeout.tv_sec -= SEC_INC;
								timeout.tv_usec = 1000000-USEC_INC;
							}
						}
						break;
					case 's': /* slower usec timeout */
						if (timeout.tv_usec < 1000000-USEC_INC)
							timeout.tv_usec += USEC_INC;
						else {
							timeout.tv_usec = 0;
							timeout.tv_sec += 1;
						}
						break;
					case 'F': /* faster sec timeout */
						if (timeout.tv_sec >= SEC_INC)
							timeout.tv_sec -= SEC_INC;
						break;
					case 'S': /* slower sec timeout */
						timeout.tv_sec += SEC_INC;
						break;
					case 'R': /* reset */
						timeout.tv_sec = SEC_INC;
						timeout.tv_usec = USEC_INC;
						num_of_val = 0;
						redisplay(w);
						break;
					case 'h':
					case 'H':
					case '?': /* Help */
						printf("%s\n%s\n%s\n%s\n%s\n%s\n",
						"'s' slower usec timeout",
						"'f' faster usec timeout",
						"'S' slower sec timeout",
						"'F' faster sec timeout",
						"'R' reset timeout and display",
						"'q' or 'Q' quit");
						/*
						 * Don't reset timeout
						 */
						return(0);
					case 'q':
					case 'Q':
						return(-1);
					}	/* switch(key) */
				break;
			case ExposeWindow:
				XSync(0);
				while (XPending() != 0) {
					XPeekEvent (&pevent);
					if (pevent.type != ExposeWindow) break;
					XNextEvent(&event);
				}

				exp_event = (XExposeWindowEvent *) &event;
				win.x = exp_event->x;
				win.y = exp_event->y;
				win.width = exp_event->width;
				win.height = exp_event->height - 10;
				redisplay(w);
				break;
			default:
				break;
			}
		}
	}
	*timer = timeout;
	return(0);
}

int total_disk_transfers()
{
	register int i, total_xfers = 0;

	for(i=0; i < DK_NDRIVE; i++)
		total_xfers += s.xfer[i];
	return(total_xfers/etime);
}

copy_cpu_stats(stat)
int *stat[CPUSTATES-1];
{
	register int i;

	for(i=0; i<CPUSTATES; i++) {
		float f = stat1(i);
		if (i == 0) {           /* US+NI */
			i++;
			f += stat1(i);
		}
		if (stat[i-1] != 0)
			*stat[i-1] = f;
	}
}

collect_stats()
{

	off_t ifnetaddr = (long)nl[N_IFNET].n_value;

	register int i;

	lseek(mf, (long)nl[X_CPTIME].n_value, 0);
	read(mf, s.time, sizeof s.time);
	lseek(mf, (long)nl[X_DKXFER].n_value, 0);
	read(mf, s.xfer, sizeof s.xfer);
	if (nintv != 1) {
		steal((long)nl[X_SUM].n_value, rate);
	} 
	else {
		steal((long)nl[X_RATE].n_value, rate);
	}
	steal((long)nl[X_TOTAL].n_value, total);
	osum = sum;
	steal((long)nl[X_SUM].n_value, sum);
	steal((long)nl[X_DEFICIT].n_value, deficit);
	etime = 0;
	for (i=0; i < DK_NDRIVE; i++) {
		t = s.xfer[i];
		s.xfer[i] -= s1.xfer[i];
		s1.xfer[i] = t;
	}
	for (i=0; i < CPUSTATES; i++) {
		t = s.time[i];
		s.time[i] -= s1.time[i];
		s1.time[i] = t;
		etime += s.time[i];
	}
	if(etime == 0.)
		etime = 1.;
	etime /= 60.;
	nintv = 1;

	if (nl[N_IFNET].n_value != 0) {
		struct ifnet ifnet;
		steal((long)nl[N_IFNET].n_value, ifnetaddr);
		old_packets = packets;
		packets.input = packets.output = packets.collisions = 0;
		while (ifnetaddr) {
			steal(ifnetaddr, ifnet);
			packets.input += ifnet.if_ipackets;
			packets.output += ifnet.if_opackets;
			packets.collisions += ifnet.if_collisions;
			ifnetaddr = (off_t) ifnet.if_next;
		}
	}

}

min (a, b)
	int a,b;
{
	return(a<b ? a:b);
}

#define	YORIGIN_FOR_STAT(num)	((((num)*win.height)/num_stats)+3)
#define	YMIDPOINT_FOR_STAT(num)	((((num)*win.height+win.height/2)/num_stats) + 5)
#define Y_FOR_STAT_VAL(stat, num_of_val)				\
	y_base - min(height_of_stat, (	\
		height_of_stat*(					\
		  stats[stat].value[num_of_val]-stats[stat].min_val)/(	\
		  stats[stat].max_val-stats[stat].min_val)))
#define First_Point(v, xv, yv) {v->x = xv; v->y = yv;\
		v++->flags = VertexDontDraw; }
#define Next_Point(v, xv, yv) {v->x = xv; v->y = yv;\
		v++->flags = VertexRelative | VertexDrawLastPoint; }

display_dividers(w, clear_first)
int clear_first;
Window w;
{
	register int	i, stat;
	register int lwidth = win.width - graph_x_offset;
	Vertex v[NUM_VALS_PER];
	register Vertex *vp;

	if(debug) fprintf(stderr, "num_of_val=%d\n", num_of_val);
	FORALLSTATS(stat) {
		register int y_org = YORIGIN_FOR_STAT(stat+1);
		vp = v;
		if (clear_first)
			XPixSet(w, graph_x_offset, y_org-2, lwidth, 5, background);
		/* Draw the horizontal line and then add the tick marks */
		XLine(w, graph_x_offset, y_org, win.width, y_org, 1, 1, 
		    foreground, GXcopy, ~0);
		for (i = 0; i < num_of_val; i++) {
			if (do_time[i]){
				First_Point(vp, graph_x_offset + i, y_org - 2);
				Next_Point(vp, 0, 4);
			}
		}
		if (vp != v)
			XDraw(w, v, vp-v, 1, 1, foreground, GXcopy, ~0);
	}
}

redisplay(w)
Window w;
{
	register int height_of_stat, stat;

	XClear (w);
	display_dividers(w, 0);
	height_of_stat = YORIGIN_FOR_STAT(1) - YORIGIN_FOR_STAT(0) - 10;
	XTextMask (w, 0, 0, Host, strlen (Host), finfo->id, highlight);
	FORALLSTATS(stat) {
		register int y_origin_of_stat = YORIGIN_FOR_STAT(stat);
		int text_size;
		char temp[20];
		XTextMask (w, 0, YMIDPOINT_FOR_STAT(stat),
		    stats[stat].label, strlen (stats[stat].label), finfo->id,
		    highlight);
		XTextMask (w, 0, YMIDPOINT_FOR_STAT(stat)+10,
		    stats[stat].label2, strlen (stats[stat].label2), finfo->id, 
		    highlight);
		sprintf(temp, "%d", stats[stat].max_val);
		text_size = XStringWidth (temp, finfo, 0, 0);
		XTextMask (w, graph_x_offset-5-text_size, y_origin_of_stat+5,
			temp, strlen (temp), finfo->id, highlight);
		sprintf(temp, "%d", stats[stat].min_val);
		text_size = XStringWidth (temp, finfo, 0, 0);
		XTextMask (w, graph_x_offset-5-text_size,
		    y_origin_of_stat-1+height_of_stat, temp, strlen (temp),
		    finfo->id, highlight);
	}
	if (num_of_val > 0) FORALLSTATS(stat) 
		redisplay_stat_values(w, height_of_stat, stat, num_of_val);

}

redisplay_stat_values(w, height_of_stat, stat, stop_plus_one)
Window w;
int height_of_stat, stat, stop_plus_one;
{
	register int j, newY;
	Vertex v[NUM_VALS_PER];
	register Vertex *vp = v;
	int y_base = YORIGIN_FOR_STAT(stat+1)-5;
	newY = Y_FOR_STAT_VAL(stat, 0);
	First_Point(vp, graph_x_offset, newY);
	for (j = 1; j < stop_plus_one; ) {
		register int npts = 0, oldY = newY;
		do {
			newY = Y_FOR_STAT_VAL(stat, j);
			j++; 
			npts++;
		} 
		while ((oldY == newY) && (j < stop_plus_one));
		if (--npts)
			Next_Point(vp, npts, 0);
		Next_Point(vp, 1, newY - oldY);
	}
	if (vp != v)
		XDraw(w, v, vp-v, 1, 1, foreground, GXcopy, ~0);
}

next_display(w)
Window w;
{
	int stat, height_of_stat, redisp = 0;

	height_of_stat = YORIGIN_FOR_STAT(1) - YORIGIN_FOR_STAT(0) - 10;
	FORALLSTATS(stat) {
		int newY, oldY;
		int y_base = YORIGIN_FOR_STAT(stat+1)-5;
		newY = Y_FOR_STAT_VAL(stat, num_of_val);
		if (num_of_val == 0)
			oldY = newY;
		else
			oldY = Y_FOR_STAT_VAL(stat, num_of_val-1);
		XLine(w, graph_x_offset+num_of_val, oldY,
		    graph_x_offset+num_of_val+1, newY, 1, 1, foreground, 
		    GXcopy, ~0);
		if (do_time[num_of_val]) {
			y_base += 5;
			XLine(w, graph_x_offset+num_of_val, y_base-2,
			    graph_x_offset+num_of_val, y_base+2,
			    1, 1, foreground, GXcopy, ~0);
		}
	}
	if (++num_of_val >= NUM_VALS_PER ||
	    num_of_val >= win.width-graph_x_offset) {
		int num_shift_left = (win.width-graph_x_offset)/2;
		int width = (win.width-graph_x_offset) - num_shift_left;
		register int j;
		for (j = num_shift_left; j < num_of_val; j++)
			do_time[j-num_shift_left] = do_time[j];
		FORALLSTATS(stat) {
			register int ys = YORIGIN_FOR_STAT(stat)+5, nmax = 1, t;
			for (j = num_shift_left; j < num_of_val; j++) {
				t = stats[stat].value[j-num_shift_left] =
				    stats[stat].value[j];
				nmax = nmax > t ? nmax : t;
			}
			if (stat >= FREE_MEM && stat < COLLISION_PACKETS  && nmax != stats[stat].max_val) {
				stats[stat].max_val = nmax;
				redisp = 1;
			}
			if (!redisp) {
				XMoveArea(w, graph_x_offset+num_shift_left,
				ys, graph_x_offset, ys, width, height_of_stat+2);
				XPixSet(w, graph_x_offset+num_shift_left,
          			  ys, width, height_of_stat+2, background);

			}
		}
		num_of_val -= num_shift_left+1;
		if (redisp)
			redisplay(w);
		else
			display_dividers(w, 1);
	}
}

int get_namelist(kernel_name, memory_name)
char *kernel_name, *memory_name;
{
	time_t now;
	time_t boottime;
	register int i;
	int nintv;

	nlist(kernel_name, nl);
	if(nl[0].n_type == 0) {
		fprintf(stderr, "no %s namelist\n", kernel_name);
		exit(1);
	}
	mf = open(memory_name, 0);
	if (mf < 0) {
		fprintf(stderr, "cannot open %s\n", memory_name);
		exit(1);
	}
	steal((long)nl[X_MAXFREE].n_value, maxfree);
	steal((long)nl[X_BOOTTIME].n_value, boottime);
	steal((long)nl[X_HZ].n_value, hz);
	for (i = 0; i < DK_NDRIVE; i++) {
		strcpy(dr_name[i], "xx");
		dr_unit[i] = i;
	}
	read_names();
	time(&now);
	nintv = now - boottime;
	if (nintv <= 0 || nintv > 60*60*24*365*10) {
		fprintf(stderr,
		"Time makes no sense... namelist must be wrong.\n");
		exit(1);
	}
	return(nintv);
}

double
stat1(row)
{
	double t;
	register i;

	t = 0;
	for(i=0; i<CPUSTATES; i++)
		t += s.time[i];
	if(t == 0.)
		t = 1.;
	return(s.time[row]*100./t);
}

#ifdef vax
read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nl[X_MBDINIT].n_value;
	up = (struct uba_device *) nl[X_UBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "perfmon: Disk init info not in namelist\n");
		exit(1);
	}
	if(mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c", cp[0], cp[1]);
		dr_unit[mdev.mi_dk] = mdev.mi_unit;
	}
	if(up) for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c", cp[0], cp[1]);
		dr_unit[udev.ui_dk] = udev.ui_unit;
	}
}
#endif vax

#ifdef sun
read_names()
{
	struct mb_device mdev;
	register struct mb_device *mp;
	struct mb_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;

	mp = (struct mb_device *) nl[X_MBDINIT].n_value;
	if (mp == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(mp++, mdev);
		if (mdev.md_driver == 0)
			break;
		if (mdev.md_dk < 0 || mdev.md_alive == 0)
			continue;
		steal(mdev.md_driver, mdrv);
		steal(mdrv.mdr_dname, two_char);
		sprintf(dr_name[mdev.md_dk], "%c%c", cp[0], cp[1]);
		dr_unit[mdev.md_dk] = mdev.md_unit;
	}
}
#endif sun

#ifdef ibm032
read_names()
{
	struct iocc_device mdev;
	register struct iocc_device *mp;
	struct iocc_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;

	mp = (struct iocc_device *) nl[X_IOCINIT].n_value;
	if (mp == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(mp++, mdev);
		if (mdev.iod_driver == 0)
			break;
		if (mdev.iod_dk < 0 || mdev.iod_alive == 0)
			continue;
		steal(mdev.iod_driver, mdrv);
		steal(mdrv.idr_dname, two_char);
		sprintf(dr_name[mdev.iod_dk], "%c%c", cp[0], cp[1]);
		dr_unit[mdev.iod_dk] = mdev.iod_unit;
	}
}
#endif ibm032

#ifdef tahoe
read_names()
{
	struct vba_device udev, *up;
	struct vba_driver udrv;
	short two_char;
	char *cp = (char *)&two_char;

	up = (struct vba_device *) nl[X_VBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		     cp[0], cp[1], udev.ui_unit);
	}
}
#endif

usage()
{
	fprintf(stderr,
		"Usage: xperfmon [host:display] option option .....\n");
	exit(1);
}
