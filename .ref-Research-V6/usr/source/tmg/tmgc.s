/ tmg tables and global definitions

/ in addition to these definitions each routine
/ follows the convention
/ f = stack frame pointer during parse and translation
/ g = stack frame end during parse
/ i = interprested instruction counter during parse and translation

.globl tables
.globl x,si,j,k,n,g1,env
.globl ek,ek.fs,ep,ep.fs,fs
.globl lptr,sptr,rptr
.globl outb,outt,outw
.globl stkt,stkb,stke
.globl ktat,ktab
.globl input,cfile,lfile,dfile,ofile

/ parse stack frame layout
/ 0 is previous frame pointer
/ return address in (sp)
x = 2.	/exit bit, nonzero at end of rule
si = 4.	/ save location for instruction counter
j = 6.	/ input cursor counts characters
k = 8.	/ ktable water mark, last use location relative to base
n = 10.	/address of ignored character class
env = 12.	/ frame pointer for static environment
g1 = 14.	/ frame length during parse

/symbol table entry layout
/word 0 is for customer
lptr=2.	/index of next entry on tree to left
rptr=4.	/index of next entry to right
sptr=6.	/first character of string in this entry
		/next char is in 7, etc

/ translation frame layout
/ used as ek(f), ep(f), etc
/ x and si have same meaning as in parse stack frame
/ return address in (sp)
ek = 0	/ k environment, frame where bunlde address is in si
ep = 6	/ p environment, frame where si points to parameter list
fs = 10	/ frame size
ek.fs = ek+fs	/ k environment in next frame
ep.fs = ep+fs	/ p env in next frame

.data

tables = . 	/marks break between routines and tables

outt = 64.	/ output buffer top
stkt = 800.	/stack top for (f), not for (sp)
ktat = 1200.	/ k table top


input: -1	/ stream number of input
cfile: 1	/ stream number of current output
lfile: 1	/ last current output
dfile: 2	/ diagnostic file
ofile: 1	/output file


outw: 0	/ output write pointer, number of chars in buffer
.bss
outb: .=.+outt	/output buffer



ktab: . = .+ktat	/ contains translation rules that have been bundled

stkb: . = .+stkt	/ stack, (f) ponts into this
stke = .	/stack end
