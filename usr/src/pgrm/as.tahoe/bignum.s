.data
.data
_sccsid:.long	0x40282329
.long	0x6269676e
.long	0x756d312e
.long	0x6320342e
.long	0x3420362f
.long	0x33302f38
.long	0x33000000
.comm	_rusefile,32
.comm	_relfil,4
.comm	_listfile,4
.comm	_liston,4
.comm	_Znumber,12
.text
LL0:.align	1
.globl	_as_atoi
.data
L163:.long	0x10202
.long	0x3030303
.long	0x1020303
.space	4
.long	0x0
.space	4
.long	0x2030000
.space	4
.text
.data	1
L167:.ascii	"%s%s\12\0"
.text
.data	1
L168:.ascii	"n_n.num_tag != 0\0"
.text
.data	1
L169:.ascii	" Botch width computation\0"
.text
.lcomm	L170,12
.set	L106,0x1c00
.data
.text
_as_atoi:.word	L106
subl3	$92,fp,sp
movl	4(fp),r12
clrl	-92(fp)
clrl	-88(fp)
L112:tstb	(r12)
jeql	L111
cvtbl	(r12),r0
cmpl	r0,$43
jeql	L110
cmpl	r0,$45
jeql	L117
cmpl	r0,$48
jeql	L110
L111:movab	-72(fp),r1
movab	_Znumber,r0
movl	$12,r2
movblk
movab	-84(fp),r1
movab	_Znumber,r0
movl	$12,r2
movblk
subl3	$72,fp,r11
pushl	r11
callf	$8,_numclear
subl3	$84,fp,-56(fp)
pushl	-56(fp)
callf	$8,_numclear
L120:tstb	(r12)
jeql	L126
cvtbl	(r12),r0
casel	r0,$48,$54
.align 1
L2000007:
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L134-L2000007
.word	L124-L2000007
.word	L124-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L140-L2000007
.word	L140-L2000007
.word	L140-L2000007
.word	L140-L2000007
.word	L140-L2000007
.word	L140-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L126-L2000007
.word	L147-L2000007
.word	L147-L2000007
.word	L147-L2000007
.word	L147-L2000007
.word	L147-L2000007
.word	L147-L2000007
L126:pushl	r11
callf	$8,_posovf
orl2	r0,-92(fp)
tstl	-88(fp)
jeql	L156
bitl	$8,-92(fp)
jeql	L157
andl2	$-13,-92(fp)
jbr	L156
L117:xorl2	$1,-88(fp)
L110:incl	r12
jbr	L112
L124:cmpl	8(fp),$10
jlss	L126
L134:cvtbl	(r12),r0
subl3	$48,r0,-60(fp)
jbr	L121
L2000001:cmpl	r0,$10
jeql	L155
cmpl	r0,$16
jeql	L154
L151:pushl	-60(fp)
pushl	r11
pushl	r11
callf	$16,_numaddd
orl2	r0,-92(fp)
incl	r12
jbr	L120
L140:cmpl	8(fp),$16
jlss	L126
cvtbl	(r12),r0
subl3	$55,r0,-60(fp)
jbr	L121
L147:cmpl	8(fp),$16
jlss	L126
cvtbl	(r12),r0
subl3	$87,r0,-60(fp)
L121:movl	8(fp),r0
cmpl	r0,$8
jneq	L2000001
pushl	r11
pushl	r11
pushl	$3
L2000005:callf	$16,_numshift
L2000006:orl2	r0,-92(fp)
jbr	L151
L154:pushl	r11
pushl	r11
pushl	$4
jbr	L2000005
L155:pushl	r11
pushl	-56(fp)
pushl	$1
callf	$16,_numshift
orl2	r0,-92(fp)
pushl	r11
pushl	r11
pushl	$3
callf	$16,_numshift
orl2	r0,-92(fp)
pushl	r11
pushl	-56(fp)
pushl	r11
callf	$16,_numaddv
jbr	L2000006
L157:pushl	r11
pushl	r11
callf	$12,_numnegate
orl2	r0,-92(fp)
L156:tstl	-88(fp)
jeql	L99999
mnegl	$1,r0
jbr	L99998
L99999:clrl	r0
L99998:movl	r0,-60(fp)
clrl	r10
L2000003:cmpl	(r11)[r10],-60(fp)
jeql	L160
aoblss	$2,r10,L2000003
L160:decl	r10
jgeq	L164
clrl	r10
L164:movb	L163+8[r10],-64(fp)
jneq	L165
pushl	$L169
pushl	$L168
pushl	$L167
callf	$16,_panic
L165:movl	-92(fp),*12(fp)
subl3	$72,fp,r0
movab	L170,r1
movab	(r1),r1
movab	(r0),r0
movl	$12,r2
movblk
movab	L170,r0
ret#1
.align	1
.globl	_as_atof
.lcomm	L180,12
.set	L171,0x0
.data
.text
_as_atof:.word	L171
subl3	$64,fp,sp
movab	-64(fp),r1
movab	_Znumber,r0
movl	$12,r2
movblk
cvtlb	8(fp),-56(fp)
movl	8(fp),r0
cmpl	r0,$4
jeql	L179
cmpl	r0,$5
jeql	L178
L176:subl3	$64,fp,r0
movab	L180,r1
movab	(r1),r1
movab	(r0),r0
movl	$12,r2
movblk
movab	L180,r0
ret#1
L178:pushl	4(fp)
callf	$8,_atof
movl	r1,-60(fp)
movl	r0,-64(fp)
jbr	L176
L179:pushl	4(fp)
callf	$8,_atof
ldd	r0
cvdf
stf	-64(fp)
jbr	L176
.align	1
.globl	_posovf
.set	L181,0x1800
.data
.text
_posovf:.word	L181
subl3	$56,fp,sp
movl	4(fp),r12
clrl	-56(fp)
bitl	$-2147483648,4(r12)
jeql	L185
movl	$4,-56(fp)
L185:cmpl	4(r12),$-2147483648
jneq	L2000010
clrl	r11
L2000009:tstl	(r12)[r11]
jeql	L187
L2000010:movl	-56(fp),r0
ret#1
L187:decl	r11
jgeq	L2000009
orl2	$8,-56(fp)
jbr	L2000010
.align	1
.globl	_isclear
.set	L192,0x1000
.data
.text
_isclear:.word	L192
movl	4(fp),r12
pushl	$_Znumber
pushl	r12
callf	$12,_isunequal
tstl	r0
jneq	L99997
movl	$1,r0
jbr	L99996
L99997:clrl	r0
L99996:ret#1
.align	1
.globl	_isunequal
.set	L197,0x1c00
.data
.text
_isunequal:.word	L197
movl	4(fp),r12
movl	8(fp),r11
movl	$2,r10
L203:movl	r12,r0
addl2	$4,r12
movl	r11,r1
addl2	$4,r11
cmpl	(r0),(r1)
jeql	L202
movl	r10,r0
ret#1
L202:decl	r10
jneq	L203
clrl	r0
ret#1
.align	1
.globl	_numclear
.set	L205,0x1800
.data
.text
_numclear:.word	L205
movl	4(fp),r12
movl	$2,r11
L211:clrl	(r12)
addl2	$4,r12
decl	r11
jneq	L211
clrl	r0
ret#1
.align	1
.globl	_numshift
.set	L212,0x1fc0
.data
.text
_numshift:.word	L212
movl	8(fp),r12
movl	12(fp),r11
movl	$2,r10
tstl	4(fp)
jneq	L216
L219:movl	(r11),(r12)
addl2	$4,r11
addl2	$4,r12
decl	r10
jneq	L219
clrl	r0
ret#1
L216:clrl	r9
movl	4(fp),r0
shll	r0,$1,r0
subl3	$1,r0,r7
tstl	4(fp)
jleq	L220
L223:movl	(r11),r6
addl2	$4,r11
subl3	4(fp),$32,r0
shrl	r0,r6,r0
andl3	r7,r0,r8
movl	4(fp),r0
shll	r0,r6,r6
mcoml	r7,r0
andl2	r0,r6
orl3	r9,r6,(r12)
addl2	$4,r12
movl	r8,r9
decl	r10
jneq	L223
tstl	r9
jeql	L99995
movl	$2,r0
jbr	L99994
L99995:clrl	r0
L99994:ret#1
L220:mnegl	4(fp),4(fp)
addl2	$8,r11
addl2	$8,r12
L226:subl2	$4,r11
movl	(r11),r6
andl3	r7,r6,r8
movl	4(fp),r0
shrl	r0,r6,r6
subl3	r0,$32,r0
shll	r0,$1,r0
decl	r0
andl2	r0,r6
orl3	r9,r6,r0
subl2	$4,r12
movl	r0,(r12)
subl3	4(fp),$32,r0
shll	r0,r8,r9
decl	r10
jneq	L226
tstl	r9
jeql	L99993
movl	$2,r0
jbr	L99992
L99993:clrl	r0
L99992:ret#1
.align	1
.globl	_numaddd
.lcomm	L231,12
.set	L227,0x0
.data
.text
_numaddd:.word	L227
cvtlb	12(fp),L231+3
pushl	$L231
pushl	8(fp)
pushl	4(fp)
callf	$16,_numaddv
ret#1
.align	1
.globl	_numaddv
.set	L232,0x1fc0
.data
.text
_numaddv:.word	L232
subl3	$56,fp,sp
movl	4(fp),r12
movl	8(fp),r11
movl	12(fp),r10
clrl	r8
movl	$2,r9
L238:movl	(r11),r7
addl2	$4,r11
movl	(r10),r6
addl2	$4,r10
addl3	r6,r7,r0
addl3	r8,r0,-56(fp)
movl	-56(fp),(r12)
addl2	$4,r12
clrl	r8
cmpl	-56(fp),r7
jlssu	L99991
cmpl	-56(fp),r6
jgequ	L237
L99991:movl	$1,r8
L237:decl	r9
jneq	L238
tstl	r8
jeql	L99990
movl	$1,r0
jbr	L99989
L99990:clrl	r0
L99989:ret#1
.align	1
.globl	_numnegate
.set	L240,0x0
.data
.text
_numnegate:.word	L240
subl3	$56,fp,sp
pushl	8(fp)
pushl	4(fp)
callf	$12,_num1comp
movl	r0,-56(fp)
pushl	$1
pushl	4(fp)
pushl	4(fp)
callf	$16,_numaddd
orl2	r0,-56(fp)
movl	-56(fp),r0
ret#1
.align	1
.globl	_num1comp
.set	L244,0x1c00
.data
.text
_num1comp:.word	L244
movl	4(fp),r12
movl	8(fp),r11
movl	$2,r10
L250:mcoml	(r11),(r12)
addl2	$4,r11
addl2	$4,r12
decl	r10
jneq	L250
clrl	r0
ret#1
.align	1
.globl	_bignumprint
.data	1
L260:.ascii	"val[msd] = 0x%x, val[lsd] = 0x%x.\0"
.text
.data	1
L262:.ascii	"value %20.17f\0"
.text
.data	1
L264:.ascii	"value %20.17f\0"
.text
.set	L252,0x0
.data
.text
_bignumprint:.word	L252
movl	_num_type,r0
cmpl	r0,$3
jeql	L258
cmpl	r0,$4
jeql	L261
cmpl	r0,$5
jeql	L263
L256:ret#2
L258:pushl	4(fp)
pushl	8(fp)
pushl	$L260
L2000011:callf	$16,_printf
jbr	L256
L261:pushl	$0
pushl	4(fp)
pushl	$L262
jbr	L2000011
L263:pushl	8(fp)
pushl	4(fp)
pushl	$L264
jbr	L2000011

