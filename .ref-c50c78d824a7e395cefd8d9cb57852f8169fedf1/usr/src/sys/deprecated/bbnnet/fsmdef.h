/* $Header: fsmdef.h,v 1.4 85/06/18 14:28:45 walsh Exp $ */


#ifndef TRPT
char fstab[TCP_NSTATES][INOP] = {
{
/* STATE CLOSED */
/* unused X unused */				0,
/* CLOSED X from U:OPEN(await) */		1,
/* CLOSED X from N:RECEIVE(response) */		0,
/* CLOSED X from U:OPEN(request) */		4,
			/* user may close a socket after socket() or bind() */
/* CLOSED X from U:CLOSE(request) */		6,	/* was 0 in 4.1 */
/* CLOSED X from S:TIMER(response) */		24,
/* CLOSED X from U:RECEIVE(request) */		0,
/* CLOSED X from U:SEND(normal) */		0,
/* CLOSED X from U:ABORT(request) */		0,
/* CLOSED X from N:CLEAR(indication) */		24,
}, /* STATE CLOSED */

/* STATE LISTEN */
{
/* LISTEN X unused */				0,
/* LISTEN X from U:OPEN(await) */		0,         
/* LISTEN X from N:RECEIVE(response) */		2,       
/* LISTEN X from U:OPEN(request) */		0,         
/* LISTEN X from U:CLOSE(request) */		6,       
/* LISTEN X from S:TIMER(response) */		9,         
/* LISTEN X from U:RECEIVE(request) */		24,         
/* LISTEN X from U:SEND(normal) */		24,         
/* LISTEN X from U:ABORT(request) */		21,      
/* LISTEN X from N:CLEAR(indication) */		23,      
}, /* STATE LISTEN */                                    
						         
						         
/* STATE SYN_SENT */                                     
{
/* SYN_SENT X unused */	                        0,
/* SYN_SENT X from U:OPEN(await) */		0,
/* SYN_SENT X from N:RECEIVE(response) */       5,
/* SYN_SENT X from U:OPEN(request) */		0,
/* SYN_SENT X from U:CLOSE(request) */		6,
/* SYN_SENT X from S:TIMER(response) */		9,
/* SYN_SENT X from U:RECEIVE(request) */        24,
/* SYN_SENT X from U:SEND(normal) */		19,	/* was 24 in 4.1 */
/* SYN_SENT X from U:ABORT(request) */		21,
/* SYN_SENT X from N:CLEAR(indication) */       23,
}, /* STATE SYN_SENT */
						         
						         
/* STATE SYN_RCVD */                                     
{
/* SYN_RCVD X unused */	                        0,
/* SYN_RCVD X from U:OPEN(await) */		0,         
/* SYN_RCVD X from N:RECEIVE(response) */       3,     
/* SYN_RCVD X from U:OPEN(request) */		0,         
/* SYN_RCVD X from U:CLOSE(request) */		14,    
/* SYN_RCVD X from S:TIMER(response) */	        9,     
/* SYN_RCVD X from U:RECEIVE(request) */        24,	   
/* SYN_RCVD X from U:SEND(normal) */		24,         
/* SYN_RCVD X from U:ABORT(request) */          21,    
/* SYN_RCVD X from N:CLEAR(indication) */       23,    
}, /* STATE SYN_RCVD */


/* STATE L_SYN_RCVD */
{
/* L_SYN_RCVD X unused */                       0,
/* L_SYN_RCVD X from U:OPEN(await) */		0,
/* L_SYN_RCVD X from N:RECEIVE(response) */	3,
/* L_SYN_RCVD X from U:OPEN(request) */		0,
/* L_SYN_RCVD X from U:CLOSE(request) */	14,  
/* L_SYN_RCVD X from S:TIMER(response) */	9,                
/* L_SYN_RCVD X from U:RECEIVE(request) */	24,
/* L_SYN_RCVD X from U:SEND(normal) */		24,
/* L_SYN_RCVD X from U:ABORT(request) */	21,  
/* L_SYN_RCVD X from N:CLEAR(indication) */	23, 
}, /* STATE L_SYN_RCVD */


/* STATE ESTAB */
{
/* ESTAB X unused */                            0,
/* ESTAB X from U:OPEN(await) */		0,
/* ESTAB X from N:RECEIVE(response) */		7,
/* ESTAB X from U:OPEN(request) */		0,
/* ESTAB X from U:CLOSE(request) */	        14,
/* ESTAB X from S:TIMER(response) */	        9,        
/* ESTAB X from U:RECEIVE(request) */           20,
/* ESTAB X from U:SEND(normal) */               19,
/* ESTAB X from U:ABORT(request) */             22,
/* ESTAB X from N:CLEAR(indication) */	        23,
}, /* STATE ESTAB */


/* STATE FIN_WAIT_1 */
{
/* FIN_WAIT_1 X unused */                       0,
/* FIN_WAIT_1 X from U:OPEN(await) */		0,
/* FIN_WAIT_1 X from N:RECEIVE(response) */	15,
/* FIN_WAIT_1 X from U:OPEN(request) */		0,
/* FIN_WAIT_1 X from U:CLOSE(request) */	24,
/* FIN_WAIT_1 X from S:TIMER(response) */	9,             
/* FIN_WAIT_1 X from U:RECEIVE(request) */	20,  
/* FIN_WAIT_1 X from U:SEND(normal) */		25,
/* FIN_WAIT_1 X from U:ABORT(request) */	22,   
/* FIN_WAIT_1 X from N:CLEAR(indication) */	23,      
}, /* STATE FIN_WAIT_1 */


/* STATE FIN_WAIT_2 */
{
/* FIN_WAIT_2 X unused */                       0,
/* FIN_WAIT_2 X from U:OPEN(await) */		0,
/* FIN_WAIT_2 X from N:RECEIVE(response) */	16,
/* FIN_WAIT_2 X from U:OPEN(request) */		0,
/* FIN_WAIT_2 X from U:CLOSE(request) */	24,
/* FIN_WAIT_2 X from S:TIMER(response) */	9,               
/* FIN_WAIT_2 X from U:RECEIVE(request) */	20,  
/* FIN_WAIT_2 X from U:SEND(normal) */		24,
/* FIN_WAIT_2 X from U:ABORT(request) */	22,  
/* FIN_WAIT_2 X from N:CLEAR(indication) */	23,   
}, /* STATE FIN_WAIT_2 */


/* STATE TIME_WAIT */
{
/* TIME_WAIT X unused */                        0,
/* TIME_WAIT X from U:OPEN(await) */		0,
/* TIME_WAIT X from N:RECEIVE(response) */	18,
/* TIME_WAIT X from U:OPEN(request) */		0,
/* TIME_WAIT X from U:CLOSE(request) */		24,
/* TIME_WAIT X from S:TIMER(response) */	9,                         
/* TIME_WAIT X from U:RECEIVE(request) */	20,  
/* TIME_WAIT X from U:SEND(normal) */		25,
/* TIME_WAIT X from U:ABORT(request) */	        22,
/* TIME_WAIT X from N:CLEAR(indication) */	23,  
}, /* STATE TIME_WAIT */


/* STATE CLOSE_WAIT */
{
/* CLOSE_WAIT X unused */			0,
/* CLOSE_WAIT X from U:OPEN(await) */		0,
/* CLOSE_WAIT X from N:RECEIVE(response) */	17,
/* CLOSE_WAIT X from U:OPEN(request) */		0,
/* CLOSE_WAIT X from U:CLOSE(request) */	8,   
/* CLOSE_WAIT X from S:TIMER(response) */	9,             
/* CLOSE_WAIT X from U:RECEIVE(request) */	20,
/* CLOSE_WAIT X from U:SEND(normal) */	        19,
/* CLOSE_WAIT X from U:ABORT(request) */	22,  
/* CLOSE_WAIT X from N:CLEAR(indication) */	23,  
}, /* STATE CLOSE_WAIT */


/* STATE CLOSING_1 */
{
/* CLOSING_1 X unused */			0,
/* CLOSING_1 X from U:OPEN(await) */		0,
/* CLOSING_1 X from N:RECEIVE(response) */	10,
/* CLOSING_1 X from U:OPEN(request) */		0,
/* CLOSING_1 X from U:CLOSE(request) */		25,
/* CLOSING_1 X from S:TIMER(response) */	9,                  
/* CLOSING_1 X from U:RECEIVE(request) */	20,  
/* CLOSING_1 X from U:SEND(normal) */		25,
/* CLOSING_1 X from U:ABORT(request) */	        22,
/* CLOSING_1 X from N:CLEAR(indication) */	23,  
}, /* STATE CLOSING_1 */


/* STATE CLOSING_2 */
{
/* CLOSING_2 X unused */			0,
/* CLOSING_2 X from U:OPEN(await) */		0,
/* CLOSING_2 X from N:RECEIVE(response) */	11,
/* CLOSING_2 X from U:OPEN(request) */		0,
/* CLOSING_2 X from U:CLOSE(request) */		25,
/* CLOSING_2 X from S:TIMER(response) */	9,              
/* CLOSING_2 X from U:RECEIVE(request) */	20,  
/* CLOSING_2 X from U:SEND(normal) */		25,
/* CLOSING_2 X from U:ABORT(request) */	        22,
/* CLOSING_2 X from N:CLEAR(indication) */	23,  
}, /* STATE CLOSING_2 */


/* STATE RCV_WAIT */
{
/* RCV_WAIT X unused */				0,
/* RCV_WAIT X from U:OPEN(await) */		0,
/* RCV_WAIT X from N:RECEIVE(response) */	13,
/* RCV_WAIT X from U:OPEN(request) */		0,
/* RCV_WAIT X from U:CLOSE(request) */		25,
/* RCV_WAIT X from S:TIMER(response) */	        9,       
/* RCV_WAIT X from U:RECEIVE(request) */	12,  
/* RCV_WAIT X from U:SEND(normal) */		25,
/* RCV_WAIT X from U:ABORT(request) */	        22,
/* RCV_WAIT X from N:CLEAR(indication) */	23,  
}, /* STATE RCV_WAIT */


/* STATE CLOSED */
{
/* CLOSED X unused */				0,
/* CLOSED X from U:OPEN(await) */               1,
/* CLOSED X from N:RECEIVE(response) */		0,
/* CLOSED X from U:OPEN(request) */	        4,
/* CLOSED X from U:CLOSE(request) */		0,
/* CLOSED X from S:TIMER(response) */		24,
/* CLOSED X from U:RECEIVE(request) */		0,
/* CLOSED X from U:SEND(normal) */		0,
/* CLOSED X from U:ABORT(request) */		0,
/* CLOSED X from N:CLEAR(indication) */		24 
}  /* STATE CLOSED */

};

int null() {return(0);}
int lis_cls();     
int lis_netr();     
int syr_netr();     
int sys_cls();      
int sys_netr();   
int cls_opn();    
int est_netr();   
int cl2_clw();    
int timers();     
int cl1_netr();   
int cl2_netr();   
int cls_rwt();    
int rwt_netr();   
int fw1_syr();    
int fw1_netr();   
int fw2_netr();   
int cwt_netr();   
int sss_syn();    
int sss_snd();    
int sss_rcv();    
int cls_nsy();    
int cls_syn();    
int cls_act();    
int cls_err();

int (*fsactab[])() = {

	/* 0 */         null,          
        /* 1 */ 	lis_cls,        /* 1 */
        /* 2 */ 	lis_netr,       /* 3,4 */
        /* 3 */ 	syr_netr,       /* 5,33 */
        /* 4 */ 	sys_cls,        /* 6 */
        /* 5 */ 	sys_netr,       /* 8,9,11,32 */
        /* 6 */ 	cls_opn,        /* 10 */
        /* 7 */ 	est_netr,       /* 12,39 */
        /* 8 */ 	cl2_clw,        /* 13 */
        /* 9 */ 	timers,         /* 14,17,35,36,37,38 */
        /* 10 */	cl1_netr,       /* 15,18,22,23,30,39 */
        /* 11 */	cl2_netr,       /* 16,19,31,39 */
        /* 12 */	cls_rwt,        /* 20 */
        /* 13 */	rwt_netr,       /* 21,30 */
        /* 14 */	fw1_syr,        /* 24 */
        /* 15 */	fw1_netr,       /* 26,27,28,39 */
        /* 16 */	fw2_netr,       /* 29,39 */
        /* 17 */	cwt_netr,       /* 30,31,39 */
        /* 18 */ 	sss_syn,        /* 39 */
        /* 19 */	sss_snd,        /* 40,41 */
        /* 20 */	sss_rcv,        /* 42 */
        /* 21 */	cls_nsy,        /* 44 */
        /* 22 */	cls_syn,        /* 45 */
        /* 23 */	cls_act,        /* 47 ### this & INCLEAR unused on 4.2 */
	/* 24 */	null,		
	/* 25 */        cls_err
};

#endif TRPT
#ifdef TCPDEBUG

char *tcpstates[] = 
{
	"CLOSED",
	"LISTEN",        
	"SYN_SENT",      
	"SYN_RCVD",      
	"L_SYN_RCVD",    
	"ESTAB",         
	"FIN_W1",        
        "FIN_W2",           
        "TIME_WAIT",     
	"CLOSE_WAIT",    
	"CLOSING1",      
	"CLOSING2",      
	"RCV_WAIT",       
	"CLOSED"
};

char *tcpinputs[] =
{
	"BAD",
	"UOPENA",
	"NRECV",
	"UOPENR",
	"UCLOSE",
	"TIMER",
	"URECV",
	"USEND",
	"UABORT",
	"NCLEAR"
};

char *tcptimers[] =
{
	"",
	"INIT",
	"REXMT",
	"REXMTTL",
	"PERSIST",
	"FINACK",
	"DELACK",
	"NOACT"
};

#endif TCPDEBUG

