/*	mpgraph.c 1.5 10/12/90 15:57:57	*/
/*	Copyright (c) 1987, Benjamin G. Zorn */

#include	<stdio.h>
#include	"mprof.h"

typedef struct entry {
    char *key;			/* assume keys are character strings */
    char *data;
} ENTRY;

typedef enum {
    FIND,
    ENTER
} ACTION;

void	henter();
ENTRY	*hsearch();

#define	min(i1, i2)		(((i1) < (i2)) ? (i1) : (i2))

#define	UNDEFINED	-1
#define	UNUSED		-1
#define	USED		0

FILE	*stout = stdout;
FILE	*sterr = stderr;

extern	mpcell	hmem[];
extern	char	*strdup();
char	*percent_string();
char	*new_cycle_name();
mpdata	add_data_over();

int	sum_vertex_incoming_calls();
int	sum_vertex_outgoing_calls();

void	print_dynamic_graph_header();
void	print_separator();
void	print_cycle_information();
void	print_vertex_in_cycle();
void	print_normal_vertex();
void	print_self_line();

int	vdata_compar();
int	vbytes_compar();

void	print_vertex();
void	print_edge();
void	print_vecons();

int	v_count;
int	e_count;

struct vecons_struct;

typedef struct vertex_struct {
    char	*name;
    int		number;
    int		srefs;
    mpdata	data;
    struct vecons_struct	*edges;
    struct vecons_struct	*backedges;
    int		*other;
    int		*save;
    int		*scratch;
    struct vertex_struct 	*save_copy;
    /*
     * For SCC detection
     */
    struct vertex_struct 	*father;
    int		k, L;
    bool	on_S;
    /*
     * For SCC representation.
     */
    struct vecons_struct	*scc_members;
    struct vertex_struct 	*in_cycle;
    /*
     * For printing out.
     */
    int		index;
} *vertex, vertex_item;

vertex
make_vertex(name, number, data, scratch)
char	*name;
int	number;
mpdata	data;
int	*scratch;
{
    vertex result = (vertex) malloc(sizeof(vertex_item));
    result->name = name;
    result->number = number;
    result->srefs = 0;
    result->data = data;
    result->backedges = NULL;
    result->edges = NULL;
    result->scratch = scratch;
    result->save_copy = NULL;
    result->father = (vertex) UNDEFINED;
    result->k = 0;
    result->L = 0;
    result->on_S = FALSE;
    result->scc_members = NULL;
    result->in_cycle = NULL;
    result->index = 0;
    henter(name, result);
    return result;
}
    

typedef struct edge_struct {
    vertex	from, to;
    mpdata	data;
    int		mark;
    struct edge_struct *save
} *edge, edge_item;

edge
make_edge(vfrom, vto, data)
vertex	vfrom, vto;
mpdata		data;
{
    edge result = (edge) malloc(sizeof(edge_item));
    result->from = vfrom;
    result->to = vto;
    result->data = data;
    result->mark = UNUSED;
    return result;
}
    

typedef enum { VERTEX, EDGE, VECONS } vetype;

typedef struct	vecons_struct {
    vetype		headtype;
    int			 *vehead;
    struct vecons_struct *vetail;
} vecell_item, *vecell;

#define	VECELL_SIZE	(sizeof(vecell_item))

#define	vehdtype(i)	((i)->headtype)
#define	vehd(i)		((i)->vehead)
#define	vetl(i)		((i)->vetail)
#define	venull(i)	(((char *) i) == NULL)

vecell
vecons(headtype, head, tail)
vetype	headtype;
int	*head;
vecell	tail;
{
    vecell result = (vecell) malloc(VECELL_SIZE);
    vehdtype(result) = headtype;
    vehd(result) = head;
    vetl(result) = tail;
    return result;
}

#define	ccons(h, t)	vecons(VECONS, (int *) (h), (t))
#define	econs(h, t)	vecons(EDGE, (int *) (h), (t))
#define	vcons(h, t)	vecons(VERTEX, (int *) (h), (t))
#define	epush(item, listvar) \
  (listvar = (vecons(EDGE, (int *) (item), listvar)))
#define	vpush(item, listvar) \
  (listvar = (vecons(VERTEX, (int *) (item), listvar)))
#define	cpush(item, listvar) \
  (listvar = (vecons(VECONS, (int *) (item), listvar)))

#define DO_VLIST(var, list) \
	{vertex var; vecell TMPlisttail; \
	   for(TMPlisttail = (list); \
	       !venull(TMPlisttail); \
	       TMPlisttail = vetl(TMPlisttail)) { \
	     var = (vertex) vehd(TMPlisttail); {
#define DO_ELIST(var, list) \
	{edge var; vecell TMPlisttail; \
	   for(TMPlisttail = (list); \
	       !venull(TMPlisttail); \
	       TMPlisttail = vetl(TMPlisttail)) { \
	     var = (edge) vehd(TMPlisttail); {
#define DO_LIST(var, list) \
	{vecell var; vecell TMPlisttail; \
	   for(TMPlisttail = (list); \
	       !venull(TMPlisttail); \
	       TMPlisttail = vetl(TMPlisttail)) { \
	     var = (vecell) vehd(TMPlisttail); {
#define	END_DO }}}

vecell	vsort();	

void
print_vecons(f, cell)
FILE	*f;
vecell	cell;
{
    vecell	rest = cell;

    if (venull(cell)) {
	fprintf(f, "[]");
	return;
    }
	
    fprintf(f, "[");
    do {
	/*
	 * Print the car
	 */
	if (vehdtype(cell) == EDGE) {
	    print_edge(f, (edge) vehd(rest));
	} else if (vehdtype(cell) == VERTEX) {
	    print_vertex(f, (vertex) vehd(rest));
	} else if (vehdtype(cell) == VECONS) {
	    print_vecons(f, (vecell) vehd(rest));
	}
	/*
	 * Check the cdr.
	 */
	if (venull(vetl(rest))) {
	    fprintf(f, "]");
	} else {
	    fprintf(f, " ");
	}
	rest = vetl(rest);
    } while (!venull(rest));
}

int
velength(vel)
vecell	vel;
{
    int		count = 0;
    while (!venull(vel)) {
	count += 1;
	vel = (vecell) vetl(vel);
    }
    return count;
}

vecell
vereverse(vel)
vecell	vel;
{
    vecell	newl = NULL;
    while (!venull(vel)) {
	vpush((vertex) vehd(vel), newl);
	vel = (vecell) vetl(vel);
    }
    return newl;
}

void
print_vertex(f, v)
FILE	*f;
vertex	v;
{
    fprintf(f, "#v(%s :out (", v->name);
    DO_ELIST(e, v->edges)
	fprintf(f, "%s ", e->to->name);
    END_DO
    fprintf(f, ") :in (");
    DO_ELIST(e, v->backedges)
	fprintf(f, "%s ", e->from->name);
    END_DO
    fprintf(f, ")\n");
    
/*    mp_sprint_data(v->data) */
/*  fprintf(f, "edges:");
    print_vecons(f, v->edges);
    fprintf(f, "backedges:");
    print_vecons(f, v->backedges);
*/
    fflush(f);
}

void
print_edge(f, e)
FILE	*f;
edge	e;
{
    fprintf(f, "#e(%s %s)\n", e->from->name, e->to->name);
    fprintf(f, "%s\n", mp_sprint_data(e->data));
    fflush(f);
}



typedef struct graph_struct {
    int		v_count;
    vecell	vset;
    int		e_count;
    vecell	eset;
    struct graph_struct	*derived;
} *graph, graph_item;

graph	read_graph();
graph	merge_scc();

graph
make_graph(v_count, vset, e_count, eset)
int	v_count;
vecell	vset;
int	e_count;
vecell	eset;
{
    graph result = (graph) malloc(sizeof(graph_item));
    result->v_count = v_count;
    result->vset = vset;
    result->e_count = e_count;
    result->eset = eset;
    result->derived = NULL;
    return result;
}

void
print_graph(f, g)
FILE	*f;
graph	g;
{
    fprintf(f, "#g(vc:%d ", g->v_count);
    print_vecons(f, g->vset);
    fprintf(f, "\nec:%d ", g->e_count);
    print_vecons(f, g->eset);
    fprintf(f, ")\n");
    fflush(f);
}


/*
 * copy_graph -- basically, we have to copy all the vertices and edges
 * from the old graph structure.
 */

void
free_graph(g)
graph	g;
{
    vecell	rest, tmp;

    for (rest = g->vset; !venull(rest);) {
	tmp = rest;
	rest = vetl(rest);
	free((char *) vehd(tmp));
	free(tmp);
    }
    for (rest = g->eset; !venull(rest);) {
	tmp = rest;
	rest = vetl(rest);
	free((char *) vehd(tmp));
	free(tmp);
    }
    free(g);
}

/*
 * copy_graph -- basically, we have to copy all the vertices and edges
 * from the old graph structure.
 */

graph
copy_graph(g)
graph	g;
{
    vecell	new_vset = NULL, new_eset = NULL;
    vertex	new_v;
    edge	new_e;
    graph	newg = make_graph(g->v_count, g->vset, g->e_count, g->eset);

    DO_VLIST (v, g->vset)
        new_v = make_vertex(v->name, v->number, v->data, v->scratch);
        v->save_copy = new_v;
	vpush(new_v, new_vset);
    END_DO

    DO_ELIST (e, g->eset)
        mpdata	newdata = mp_new_data();
	vertex	old_vfrom, old_vto;
    
        newdata = mp_add_data(newdata, e->data);
	old_vfrom = e->from;
	old_vto = e->to;
	new_e = make_edge(old_vfrom->save_copy, old_vto->save_copy, newdata);
	epush(new_e, ((new_e->from)->edges));
	epush(new_e, ((new_e->to)->backedges));
	epush(new_e, new_eset);
    END_DO
      
    DO_VLIST (v, g->vset)
        v->save_copy = NULL;
    END_DO
      
    newg->vset = new_vset;
    newg->eset = new_eset;
    newg->derived = g;
    return newg;
}
        
    
vecell	scc();


/*
 * Hash routines -- specialized from on routines available in SunOS
 * and Ultrix libraries, but not in Berkeley Unix.
 */

ENTRY	*htable;
int	htableSize = 0;

int
hcreate(tableSize)
unsigned tableSize;
{
    htableSize = tableSize;
    htable = (ENTRY *) calloc(tableSize, sizeof(ENTRY));
    if (htable == NULL) return 0;
    return 1;
}

void
hdestroy()
{
    free(htable);
}

int
hash_fn(str, tableSize)
char	*str;
int	tableSize;
{
    int		i;
    int		result = 12345;
    
    for (i = 0; i < strlen(str); i++) {
	result =  ((result + ((int) str[i]) * 1013) & 0x7ffffff) >> 3;
    }
    return (result % tableSize);
}

ENTRY *
hsearch(item, action)
ENTRY	item;
ACTION	action;
{
    int		i, hindex;
    int		hvalue = hash_fn(item.key, htableSize);
    
    for (i = 0; i < htableSize; i++) {
	hindex = ((i + hvalue) % htableSize);
	if (htable[hindex].key == 0) {
	    /*
	     * Won't find the item.
	     */
	    if (action == ENTER) {
		htable[hindex] = item;
		return &(htable[hindex]);
	    } else {
		return NULL;
	    }
	} else if (strcmp(htable[hindex].key, item.key) == 0) {
	    /*
	     * Found the item in the table.
	     */
	    if (action == ENTER) {
		htable[hindex] = item;
	    }
	    return &(htable[hindex]);
	}
    }
    /*
     * We won't find the item.
     */
    return NULL;
}    

void
henter(key, vp)
char	*key;
vertex	vp;
{
    ENTRY	e;
    e.key = key;
    e.data = (char *) vp;
    (void) hsearch(e, ENTER);
}

vertex
hlookup(key)
char	*key;
{
    ENTRY	e, *result;
    e.key = key;
    
    result = hsearch(e, FIND);
    return (vertex) result->data;
}


#define template0 \
  "\n---------------------------s--m--l--x-------------------s--m--l--x---------------\n\n"
#define template1 \
  "  %8s %12s |%-12s | %12s |%-12s | %12s   %-s\n"
#define template1n \
  "  %8s %12d |%-12s | %12d |%-12s | %12d   %-s\n"
#define template1f \
  "  %8.1f %12d |%-12s | %12d |%-12s | %12d   %-s\n"

#define template2 \
  "%-5s%7s%9s %-5s |%-12s |%-12s |%8s%-9s%-s\n"
  
#define template2p1 \
  "%-5s%7s%9d %-5s |%-12s |%-12s |%8s%-9s%-s\n"
#define template2p2 \
  "%-5s%7s%9d %-5s |%-12s |%-12s |%8d%-9s%-s\n"

#define template2c1 \
  "%-5s%7s%9s %-5s |%-12s |%-12s |%8d%-9s%-s\n"

double
dpercent(x, y)
int	x, y;
{
    if (y == 0) {
	return 0;
    } else {
	return (100.0 * x) / y;
    }
}

char *
truncate_or_blank(d)
double	d;
{
    char	cbuf[80];
    int		intpart = d;
    double	fraction = d - intpart;
    if ((intpart == 0) && (fraction < 0.00001)) {
	return "";
    } else if (intpart == 0) {
	return ".";
    } else if (intpart == 100) {
	return "**";
    } else {
	sprintf(cbuf, "%2d", intpart);
	return strdup(cbuf);
    }
}
	

char *
data_type_string(d, divisor)
mpdata	d;
int	divisor;
{
    char	cbuf[100];
    
    if (divisor == 0) {
	return "";
    } else {
	sprintf(cbuf, " %2s %2s %2s %2s",
		truncate_or_blank(dpercent(dt_b_small(d), divisor)),
		truncate_or_blank(dpercent(dt_b_med(d), divisor)),
		truncate_or_blank(dpercent(dt_b_large(d), divisor)),
		truncate_or_blank(dpercent(dt_b_xlarge(d), divisor)));
	return strdup(cbuf);
    }
}

char *
data_kept_string(d, divisor)
mpdata	d;
int	divisor;
{
    char	cbuf[100];
    
    if (divisor == 0) {
	return "";
    } else {
	sprintf(cbuf, " %2s %2s %2s %2s",
		truncate_or_blank(dpercent(dt_d_small(d), divisor)),
		truncate_or_blank(dpercent(dt_d_med(d), divisor)),
		truncate_or_blank(dpercent(dt_d_large(d), divisor)),
		truncate_or_blank(dpercent(dt_d_xlarge(d), divisor)));
	return strdup(cbuf);
    }
}

char *
relative_type_string(num, den)
mpdata	num, den;
{
    return data_type_string(num, mp_sum_data(den));
}

char *
relative_kept_string(num, den)
mpdata	num, den;
{
    return data_kept_string(num, mp_sum_kept(den));
}

char *
type_fraction_string(d)
mpdata	d;
{
    return data_type_string(d, mp_sum_data(d));
}
		
char
 *
kept_fraction_string(d)
mpdata	d;
{
    return data_kept_string(d, mp_sum_kept(d));
}

int
sum_data_over_edges(elist)
vecell	elist;
{
    int		sum = 0;
    
    DO_ELIST(e, elist)
	if (!((e->to->in_cycle != NULL) ||
	      (e->from->in_cycle != NULL))) {
	    sum += mp_sum_data(e->data);
	}
    END_DO
    return sum;
}

int
sum_calls_over_edges(elist)
vecell	elist;
{
    int		sum = 0;
    
    DO_ELIST(e, elist)
	if (!((e->to->in_cycle != NULL) ||
	      (e->from->in_cycle != NULL))) {
	    sum += mp_sum_calls(e->data);
	}
    END_DO
    return sum;
}

int
sum_vertex_bytes(v)
vertex	v;
{
    if (v->in_cycle == NULL) {
	return mp_sum_data(v->data) + sum_data_over_edges(v->edges);
    } else {
	return sum_vertex_bytes(v->in_cycle);
    }
}

int
sum_vertex_bytes1(v)
vertex	v;
{
    if (v->in_cycle == NULL) {
	return mp_sum_data(v->data) + sum_data_over_edges(v->edges);
    } else {
	return mp_sum_data(v->data);
    }
}

int
sum_vertex_incoming_calls(v)
vertex	v;
{
    if (v->in_cycle == NULL) {
	return sum_calls_over_edges(v->backedges);
    } else {
	return sum_vertex_incoming_calls(v->in_cycle);
    }
}

int
sum_vertex_outgoing_calls(v)
vertex	v;
{
    if (v->in_cycle == NULL) {
	return sum_calls_over_edges(v->edges);
    } else {
	return sum_vertex_outgoing_calls(v->in_cycle);
    }
}

void
mprof_graph_ops(f)
FILE	*f;
{
    graph	g = read_graph();
    graph	newg = merge_scc(g);
    int		cumul_bytes;
    int		total_bytes;
    int		total_calls;
    int		total_kept;
    mpdata	total_data = mp_new_data();
    vecell	old_vset = g->vset;
    vecell	vset = newg->vset;
    vecell	ordered_vset = NULL;
    vecell	old_ordered_vset;
    int		i;

    old_ordered_vset = vsort(old_vset, vdata_compar);
    
    DO_VLIST (v, old_vset)
	total_data = mp_add_data(total_data, v->data);
    END_DO

    total_bytes = mp_sum_data(total_data);
    total_calls = mp_sum_calls(total_data);
    total_kept = mp_sum_kept(total_data);

    /*
     * Print out the allocation recorded in the leaf nodes.
     */
    fprintf(f, "---------  Direct Allocation Table ------------\n\n");
    fprintf(f, template1,
	    " % mem",
	    "bytes",
	    " % mem(size)",
	    "bytes kept",
	    "  % all kept",
	    "calls",
	    "name");
    fprintf(f, template0);
    fprintf(f, template1n,
	    "-----",
	    total_bytes,
	    type_fraction_string(total_data),
	    total_kept,
	    kept_fraction_string(total_data),
	    total_calls,
	    "<TOTAL>");
    fprintf(f, "\n");
    DO_VLIST(v, old_ordered_vset)
	mpdata	vdata = v->data;
	int	nbytes = mp_sum_data(vdata);
	if (nbytes != 0) {
	    cumul_bytes += nbytes;
	    fprintf(f, template1f,
		    dpercent(nbytes, total_bytes),
		    nbytes,
		    relative_type_string(vdata, total_data),
		    mp_sum_kept(vdata),
		    data_kept_string(vdata, total_kept),
		    mp_sum_calls(vdata),
		    v->name);
	}
    END_DO

    /*
     * Check allocation consistency from children to parents.
     */

    DO_VLIST(v, vereverse(vset))
	/*
	 * Because the vertex list is sorted, we always encounter a
	 * node before any of the callers of that node and after all
	 * the nodes it calls.
	 */
        vecell	clist = v->edges;
        vecell	plist = v->backedges;
        int	csum = sum_data_over_edges(clist);
        int	psum = sum_data_over_edges(plist);

        if (!(v->in_cycle) &&
	    (v->backedges != NULL) &&
	    (psum != (csum + mp_sum_data(v->data)))) {
	    fprintf(f, "Parent and child disagree on data allocation\n");
	    fprintf(f, "%s --> %d != %d + %d (%d)\n",
		    v->name, psum, csum, csum + mp_sum_data(v->data));
	}
    END_DO

    ordered_vset = vsort(vset, vbytes_compar);
    i = 0;
    DO_VLIST(v, ordered_vset)
	v->index = i;
	i += 1;
    END_DO

    /*
     * Print the header for the dynamic allocation graph.
     */
    print_dynamic_graph_header(f);
    
    DO_VLIST(v, ordered_vset)
	if (v->scc_members != NULL) {
	    print_cycle_information(v, f, total_bytes);
	    print_separator(f);
	    print_normal_vertex(v, f, total_bytes);
	} else if (v->in_cycle != NULL) {
	    print_vertex_in_cycle(v, f, total_bytes);
	} else {
	    print_normal_vertex(v, f, total_bytes);
	}
	print_separator(f);
    END_DO
      
    fflush(f);
}    

graph
read_graph()
{
    mpsym	s;
    int		i, count;
    mpcell	chain;
    mpcell	rest;
    mpcell	p;
    int		v_count, e_count;
    vecell	vset, eset;

    /*
     * First count the nodes and allocate space for them.
     */
    v_count = mp_count_nodes();
    e_count = 0;
    vset = NULL;
    eset = NULL;
    hcreate(2 * v_count);

    /*
     * Read in each vertex and store associated information.
     */
    count = 0;
    for (i = 0; i < MP_HASH_SIZE; i++) {
	chain = hmem[i];
	while (!mp_null(chain)) {
	    vertex	v;
	    s = (mpsym) mp_car(chain);
	    v = make_vertex(fn_name(s), count, fn_lcount(s), fn_parents(s));
	    vpush(v, vset);
	    count += 1;
	    chain = (mpcell) mp_cdr(chain);
	}
    }

    /*
     * Create the backedges
     */
    DO_VLIST (vto, vset)
	vecell	elist = NULL;
 	mpcell	rest;

        /*
	 * The parent list is stored in the scratch slot of the
         * vertex.
	 */
        rest = (mpcell) vto->scratch;
	while (!mp_null(rest)) {
	    char	*parent_name;
	    mpdata	parent_data;
	    mpcell	parent = (mpcell) mp_car(rest);
	    vertex	vfrom;
	    edge	e;
	    
	    parent_name = fn_name((mpsym) mp_car(parent));
	    parent_data = (mpdata) mp_cdr(parent);
	    vfrom = hlookup(parent_name);

	    if (vfrom == vto) {
		vto->srefs += 1;
	    } else {
		e = make_edge(vfrom, vto, parent_data);
		e_count +=  1;
		epush(e, eset);
		epush(e, elist);
	    }
   	    rest = mp_cdr(rest);
	}
        vto->backedges = elist;
    END_DO

    DO_VLIST (v, vset)
	DO_ELIST (e, v->backedges)
	    vertex	vfrom = e->from;
	    epush(e, vfrom->edges);
        END_DO
    END_DO
    return make_graph(v_count, vset, e_count, eset);
}


edge
same_edge(from_v, to_v, elist)
vertex	from_v, to_v;
vecell	elist;
{
    DO_ELIST (e, elist)
	if ((e->to == to_v) && (e->from == from_v)) {
	    return e;
	}
    END_DO
    return NULL;
}

int	
edge_less_compar(e1, e2)
edge	*e1, *e2;
{
    char	*e1_from, *e1_to, *e2_from, *e2_to;
    e1_from = (*e1)->from->name;
    e1_to = (*e1)->to->name;
    e2_from = (*e2)->from->name;
    e2_to = (*e2)->to->name;
    if (strcmp(e1_from, e2_from) == 0) {
	return strcmp(e1_to, e2_to);
    } else {
	return strcmp(e1_from, e2_from);
    }
}

int
vdata_compar(v1p, v2p)
vertex	*v1p, *v2p;
{
    int		d1 = mp_sum_data((*v1p)->data);
    int		d2 = mp_sum_data((*v2p)->data);
    if (d1 < d2) {
	return 1;
    } else if (d1 > d2) {
	return -1;
    } else {
	return 0;
    }
}

int
vbytes_compar(v1p, v2p)
vertex	*v1p, *v2p;
{
    int		d1 = sum_vertex_bytes1(*v1p);
    int		d2 = sum_vertex_bytes1(*v2p);
    if (d1 < d2) {
	return 1;
    } else if (d1 > d2) {
	return -1;
    } else {
	return 0;
    }
}

vecell
vsort(vlist, compar)
vecell	vlist;
int 	(*compar)();
{
    int	llength = velength(vlist);
    vertex	*vvec = (vertex *) calloc(llength, sizeof(vertex));
    int		i;
    vecell	newvlist = NULL;

    i = 0;
    DO_VLIST (v, vlist)
        vvec[i] = v;
        i += 1;
    END_DO

    qsort((char *) vvec, llength, sizeof(vertex), compar);

    for (i = (llength - 1); i >= 0; i--) {
	epush(vvec[i], newvlist);
    }
    return newvlist;
}

vecell
esort(elist, compar)
vecell	elist;
int 	(*compar)();
{
    int	llength = velength(elist);
    edge	*evec = (edge *) calloc(llength, sizeof(edge));
    int		i;
    vecell	newelist = NULL;

    i = 0;
    DO_ELIST (e, elist)
        evec[i] = e;
        i += 1;
    END_DO

    qsort((char *) evec, llength, sizeof(edge), compar);

    for (i = (llength - 1); i >= 0; i--) {
	epush(evec[i], newelist);
    }
    return newelist;
}

graph
merge_scc(g)
graph	g;
{
    graph	newg = copy_graph(g);
    vecell	vset = NULL, eset = NULL, added_eset = NULL;
    int	cycle_count = 0;

    vecell	cc_list = scc(newg);
/*    print_graph(stdout, newg);
*/

    /*
     * Create vertices corresponding to each of the cycles.
     */

    DO_LIST (cc, cc_list)
	if (velength(cc) != 1) {
	    vertex newv;
	    /*
	     * Create a vertex to represent the SCC.
	     */
	    cycle_count += 1;
	    newv = make_vertex(new_cycle_name(cycle_count),
			       cycle_count + newg->v_count - 1,
			       mp_new_data(),
			       NULL);
	    vpush(newv, vset);
	    newv->scc_members = cc;

	    DO_VLIST(v, cc)
		vpush(v, vset);
		v->in_cycle = newv;
		newv->data = mp_add_data(newv->data, v->data);
	    END_DO
	} else {
	    vpush((vertex) vehd(cc), vset);
	}
    END_DO

    /*
     * Look at all the edges and create a new edge set.
     */
    DO_ELIST (e, newg->eset) 
	/*
	 * Cases --
	 * 1. Either vertex is part of a cycle.
	 * 2. Neither vertex is in a cycle.
	 */
	vertex	from_v = (e->from)->in_cycle;
	vertex	to_v = (e->to)->in_cycle;
	
	if ((from_v != NULL) || (to_v != NULL)) {
	    if (from_v == NULL) {
		from_v = e->from;
	    }
	    if (to_v == NULL) {
		to_v = e->to;
	    }
	    if (from_v != to_v) {
		edge newe = same_edge(from_v, to_v, added_eset);
		if (newe != NULL) {
		    newe->data = mp_add_data(newe->data, e->data);
		} else {
		    mpdata	newdata = mp_new_data();
		    
		    newdata = mp_add_data(newdata, e->data);
		    newe = make_edge(from_v, to_v, newdata);
		    epush(newe, added_eset);
		}
	    } else {
		from_v->srefs += 1;
	    }
	}
	epush(e, eset);
    END_DO

    DO_ELIST (e, added_eset)
	epush(e, eset);
    END_DO
    eset = esort(eset, edge_less_compar);

    /*
     * Remove the old edges and add the new ones.
     */
    DO_VLIST(v, vset) 
	v->edges = NULL;
	v->backedges = NULL;
    END_DO

    DO_ELIST(e, eset)
	vertex	vfrom = e->from;
	vertex	vto = e->to;

	epush(e, vfrom->edges);
	epush(e, vto->backedges);
    END_DO

    newg->v_count = velength(vset);
    newg->vset = vereverse(vset);
    newg->e_count = velength(eset);
    newg->eset = eset;
    return newg;
}

bool
no_unused_incident_edges_from(v)
vertex	v;
{
    vecell	elist = v->edges;
    DO_ELIST (e, elist)
	if (e->mark == UNUSED) {
	    return FALSE;
	}
    END_DO
    return TRUE;
}

edge
first_unused_edge(elist)
vecell	elist;
{
    DO_ELIST (e, elist)
	if (e->mark == UNUSED) {
	    return e;
	}
    END_DO
    fprintf(stderr, "first_unused_edge -- no unused edges present:\n");
    print_vecons(stderr, elist);
    return NULL;
}

vertex
find_vertex_with_k_equal_zero(vlist)
vecell	vlist;
{
    DO_VLIST (v, vlist)
	if (v->k == 0) {
	    return v;
	}
    END_DO;
    return NULL;
}
  
/*
 * Find the strongly connected components in the graph.
 */
vecell
scc(g)
graph	g;
{
    vecell	vset = g->vset;
    vecell	eset = g->eset;
    vecell	scc_vset = NULL;
    vecell	S = NULL;
    int		i = 0;
    vertex	v = (vertex) vehd(vset);
    vertex	u, new_u;
    edge	e;

    DO_ELIST (e, eset)
	e->mark = UNUSED;
    END_DO

    DO_VLIST (v, vset)
        v->father = (vertex) UNDEFINED;
        v->k = 0;
    END_DO

 step2:
    i += 1;
    v->k = i;
    v->L = i;
    vpush(v, S);
    v->on_S = TRUE;
 step3:
    if (no_unused_incident_edges_from(v)) {
	goto step7;
    }
 step4:
    e = first_unused_edge(v->edges);
    u = e->to;
    e->mark = USED;
    if (u->k == 0) {
	u->father = v;
	v = u;
	goto step2;
    }
 step5:
    if (u->k > v->k) {
	goto step3;
    }
    if (!(u->on_S)) {
	goto step3;
    }
 step6:
    if (!((u->k < v->k) && u->on_S)) {
	fprintf(stderr, "assertion at step6 failed\n");
    }
    v->L = min(v->L, u->k);
    goto step3;
 step7:
    if (v->L == v->k) {
	vecell	comp = NULL;
	vertex	topv;

	do {
	    topv = (vertex) vehd(S);
	    S = vetl(S);
	    vpush(topv, comp);
	    topv->on_S = FALSE;
	} while (topv != v);
	cpush(comp, scc_vset);
    }
 step8:
    if (v->father != (vertex) UNDEFINED) {
	(v->father)->L = min(v->L, (v->father)->L);
	v = v->father;
	goto step3;
    }
 step9:
    if (v->father != (vertex) UNDEFINED) {
	fprintf(stderr, "assertion at step9 failed\n");
    }
    new_u = find_vertex_with_k_equal_zero(vset);
    if (new_u != NULL) {
	v = new_u;
	goto step2;
    }
    if (S != NULL) {
	cpush(S, scc_vset);
    }
    return scc_vset;
}

int
mp_count_nodes()
{
    int		i, count;
    mpcell	chain;

    count = 0;
    for (i = 0; i < MP_HASH_SIZE; i++) {
	chain = hmem[i];
	while (!mp_null(chain)) {
	    count += 1;
	    chain = (mpcell) mp_cdr(chain);
	}
    }
    return count;
}


char *
new_cycle_name(n)
int	n;
{
    char	chars[255];
    sprintf(chars, "<cycle %d>", n);
    return strdup(chars);
}

char *
int1_sprintf(fmt, n)
char	*fmt;
int	n;

{
    char	chars[255];
    sprintf(chars, fmt, n);
    return strdup(chars);
}

char *
s1_sprintf(fmt, s)
char	*fmt;
char	*s;
{
    char	chars[255];
    sprintf(chars, fmt, s);
    return strdup(chars);
}

char *
f1_sprintf(fmt, d)
char	*fmt;
double	d;
{
    char	chars[255];
    sprintf(chars, fmt, d);
    return strdup(chars);
}

char *
vertex_name_string(v)
vertex	v;
{
    char	chars[255];
    if (v->in_cycle == NULL) {
	sprintf(chars, "%s [%d]", v->name, v->index);
    } else {
	sprintf(chars, "%s [%d] in %s", v->name, v->index, v->in_cycle->name);
    }
    return strdup(chars);
}
	
void
print_dynamic_graph_header(f)
FILE	*f;
{
    fprintf(f, "\f\n\n");
    fprintf(f, template2,
	    "",
	    "self",
	    "",
	    "",
	    "     /ances",
	    "     /ances",
	    "called",
	    "/total ",
	    "    ancestors");
    fprintf(f, template2,
	    "index",
	    "+  ",
	    "self",
	    "(%)",
	    " size-func",
	    " frac",
	    "called",
	    "/recur",
	    "name [index]");
    fprintf(f, template2,
	    "",
	    "desc",
	    "",
	    "",
	    "     \\desc",
	    "     \\desc",
	    "called",
	    "/total",
	    "    descendents");
    print_separator(f);
}

#define template3 \
"\n-------------------------------s--m--l--x----s--m--l--x----------\n\n"
  
void
print_separator(f)
FILE	*f;
{
    fprintf(f, template3, "", "");
}


vecell
filter_cycle_edges(elist)
vecell	elist;
{
    vecell	result = NULL;
    
    DO_ELIST(e, elist)
	if (!((e->to->scc_members != NULL) ||
	      (e->from->scc_members != NULL))) {
	    epush(e, result);
	}
    END_DO
    return vereverse(result);
}

mpdata    
add_data_over(elist)
vecell	elist;
{
    mpdata	result = mp_new_data();
    
    DO_ELIST(e, elist)
	result = mp_add_data(result, e->data);
    END_DO
    return result;
}

void
print_normal_vertex(v, f, nbytes)
vertex	v;
FILE	*f;
int	nbytes;
{
    vecell	plist = v->backedges;
    vecell	clist = v->edges;
    vecell	filt_plist, filt_clist;
    int		pbytesum = sum_data_over_edges(plist);
    int		cbytesum = sum_data_over_edges(clist);
    mpdata	all_pdata;
    mpdata	all_cdata;

    if (v->scc_members != NULL) {
	filt_plist = plist;
	filt_clist = clist;
    } else {
	filt_plist = filter_cycle_edges(plist);
	filt_clist = filter_cycle_edges(clist);
    }
    all_pdata = add_data_over(filt_plist);
    all_cdata = add_data_over(filt_clist);

    /*
     * Parent listings.
     */
    if (velength(filt_plist) > 1) {
	fprintf(f, template2p1,
		"", "all",
		mp_sum_data(all_pdata),
		"",
		type_fraction_string(all_pdata),
		"", "", "", "");
    }
    
    DO_ELIST(e, filt_plist)
	mpdata	edata = e->data;
	int	pcallsum = sum_vertex_outgoing_calls(e->from);
	int	ecallsum = mp_sum_calls(edata);
	int	ebytesum = mp_sum_data(edata);
	fprintf(f, template2p2,
		"", "",
		ebytesum,
		percent_string(ebytesum, pbytesum),
		type_fraction_string(edata),
		relative_type_string(edata, all_pdata),
		ecallsum,
		int1_sprintf("/%d", pcallsum),
		s1_sprintf("    %s", vertex_name_string(e->from)));
    END_DO

    /*
     * Self listing.
     */
    print_self_line(f, v, nbytes, FALSE);

    /*
     * Children listings.
     */
	
    DO_ELIST(e, filt_clist)
	mpdata	edata = e->data;
	int	ccallsum = sum_vertex_incoming_calls(e->to);
	int	ecallsum = mp_sum_calls(edata);
	int	ebytesum = mp_sum_data(edata);
	fprintf(f, template2p2,
		"", "",
		ebytesum,
		percent_string(ebytesum, cbytesum),
		type_fraction_string(edata),
		relative_type_string(edata, all_cdata),
		ecallsum,
		int1_sprintf("/%d", ccallsum),
		s1_sprintf("    %s", vertex_name_string(e->to)));
    END_DO
      
    if (velength(filt_clist) > 1) {
	fprintf(f, template2p1,
		"", "all",
		mp_sum_data(all_cdata),
		"",
		type_fraction_string(all_cdata),
		"", "", "", "");
    }
}


void
print_self_line(f, v, nbytes, cyclep)
FILE	*f;
vertex	v;
int	nbytes;
bool	cyclep;
{
    vecell	plist = v->backedges;
    vecell	clist = v->edges;
    int		pcallsum = sum_calls_over_edges(plist);
    int		cbytesum = sum_data_over_edges(clist);
    int		self_bytesum = mp_sum_data(v->data);
    int		fbytesum = self_bytesum + cbytesum;

    fprintf(f, template2p2,
	    int1_sprintf("[%d]", v->index),
	    f1_sprintf("%7.1f", dpercent(fbytesum, nbytes)),
	    self_bytesum,
	    percent_string(self_bytesum, fbytesum),
	    type_fraction_string(v->data),
	    " -----------",
	    pcallsum,
	    ((v->srefs == 0) ? "" : int1_sprintf("+%d", v->srefs)),
	    ((cyclep != 0) ? s1_sprintf("%s as a whole", vertex_name_string(v))
	     : vertex_name_string(v)));
}
    
void
print_vertex_in_cycle(v, f, nbytes)
vertex	v;
FILE	*f;
int	nbytes;
{
    vecell	plist = filter_cycle_edges(v->backedges);
    vecell	clist = filter_cycle_edges(v->edges);
    int		pcallsum = 0;
    int		self_bytesum = mp_sum_data(v->data);

    /*
     * Parent listings.
     */
    
    DO_ELIST(e, plist)
	mpdata	edata = e->data;
	int	ecallsum = mp_sum_calls(edata);
    
        pcallsum += ecallsum;
	fprintf(f, template2c1,
		"", "", "", "", "", "", 
		ecallsum,
		"",
		s1_sprintf("    %s", vertex_name_string(e->from)));
    END_DO

    /*
     * Self listing.
     */
    fprintf(f, template2p2,
	    int1_sprintf("[%d]", v->index),
	    f1_sprintf("%7.1f", dpercent(self_bytesum, nbytes)),
	    self_bytesum,
	    "",
	    type_fraction_string(v->data),
	    " -----------",
	    pcallsum,
	    ((v->srefs == 0) ? "" : int1_sprintf("+%d", v->srefs)),
	    vertex_name_string(v));


    /*
     * Children listings.
     */
	
    DO_ELIST(e, clist)
	mpdata	edata = e->data;
	int	ecallsum = mp_sum_calls(edata);
    
	fprintf(f, template2c1,
		"", "", "", "", "", "", 
		ecallsum,
		"",
		s1_sprintf("    %s", vertex_name_string(e->to)));
    END_DO
}

void
print_cycle_information(v, f, nbytes)
vertex	v;
FILE	*f;
int	nbytes;
{
    vecell	members = v->scc_members;
    mpdata	mdata = v->data;
    int		mbytesum = mp_sum_data(mdata);

    /*
     * Self listing.
     */
    print_self_line(f, v, nbytes, TRUE);

    /*
     * Members listing.
     */
    DO_VLIST(v, members)
	mpdata	vdata = v->data;
	int	vbytesum = mp_sum_data(vdata);
	fprintf(f, template2p1,
		"", "",
		vbytesum,
		percent_string(vbytesum, (mbytesum == 0 ? 1 : mbytesum)),
		type_fraction_string(vdata),
		relative_type_string(vdata, mdata),
		"", "", s1_sprintf("    %s", vertex_name_string(v)));
    END_DO
}


