.SC "A Large Example"
.PP
Here is the complete source for the three display equations
in the abstract of this guide.
.sp
.nf
.ps -2
.vs -2
 .EQ I
 G(z)~mark =~ e sup { ln ~ G(z) }
 ~=~ exp left ( 
 sum from k>=1 {S sub k z sup k} over k right )
 ~=~  prod from k>=1 e sup {S sub k z sup k /k}
 .EN
 .EQ I
 lineup = left ( 1 + S sub 1 z + 
 { S sub 1 sup 2 z sup 2 } over 2! + ... right )
 left ( 1+ { S sub 2 z sup 2 } over 2
 + { S sub 2 sup 2 z sup 4 } over { 2 sup 2 cdot 2! }
 + ... right ) ...
 .EN
 .EQ I
 lineup =  sum from m>=0 left (
 sum from
 pile { k sub 1 ,k sub 2 ,..., k sub m  >=0
 above
 k sub 1 +2k sub 2 + ... +mk sub m =m}
 { S sub 1 sup {k sub 1} } over {1 sup k sub 1 k sub 1 ! } ~
 { S sub 2 sup {k sub 2} } over {2 sup k sub 2 k sub 2 ! } ~
 ...
 { S sub m sup {k sub m} } over {m sup k sub m k sub m ! } 
 right ) z sup m
 .EN
.sp
.fi
.ps +2
.vs +2
.SC "Keywords, Precedences, Etc."
.PP
If you don't use braces,
.UC EQN
will
do operations in the order shown in this list.
.P1 3
.ft I
dyad vec under bar tilde hat dot dotdot
fwd  back  down  up
fat  roman  italic  bold  size
sub  sup  sqrt  over
from  to
.ft R
.P2
These operations group to the left:
.P1
.ft I
over  sqrt  left  right
.ft R
.P2
All others group to the right.
.PP
Digits, parentheses, brackets, punctuation marks, and these mathematical words
are converted
to Roman font when encountered:
.P1
sin  cos  tan  sinh  cosh  tanh  arc
max  min  lim  log  ln  exp
Re  Im  and  if  for  det
.P2
These character sequences are recognized and translated as shown.
.sp
.nf
.tr -\(mi
.in .5i
.ta 1i
>=		$>=$
<=		$<=$
==		$==$
!=		$!=$
+-		$+-$
->		$->$
<-		$<-$
<<		$<<$
>>		$>>$
inf		$inf$
partial		$partial$
half		$half$
prime		$prime$
approx		$approx$
nothing		$nothing$
cdot		$cdot$
times		$times$
del		$del$
grad		$grad$
\&...		$...$
,...,		$,...,$
sum		$sum$
.sp 3p
int		$int$
.sp 2p
prod		$prod$
union		$union$
inter		$inter$
.sp
.in
.fi
.tr --
.PP
To obtain Greek letters,
simply spell them out in whatever case you want:
.sp
.nf
.in .2i
.ta .7i 1.4i 2.1i
DELTA	$DELTA$	iota	$iota$
GAMMA	$GAMMA$	kappa	$kappa$
LAMBDA	$LAMBDA$	lambda	$lambda$
OMEGA	$OMEGA$	mu	$mu$
PHI	$PHI$	nu	$nu$
PI	$PI$	omega	$omega$
PSI	$PSI$	omicron	$omicron$
SIGMA	$SIGMA$	phi	$phi$
THETA	$THETA$	pi	$pi$
UPSILON	$UPSILON$	psi	$psi$
XI	$XI$	rho	$rho$
alpha	$alpha$	sigma	$sigma$
beta	$beta$	tau	$tau$
chi	$chi$	theta	$theta$
delta	$delta$	upsilon	$upsilon$
epsilon	$epsilon$	xi	$xi$
eta	$eta$	zeta	$zeta$
gamma	$gamma$
.sp
.in
.fi
.PP
These are all the words known to
.UC EQN
(except for characters with names),
together with the section where they are discussed.
.sp
.nf
.in .2i
.ta .7i 1.4i 2.1i
above	17, 18	lpile	17
back	21	mark	15
bar	13	matrix	18
bold	12	ndefine	20
ccol	18	over	9
col	18	pile	17
cpile	17	rcol	18
define	20	right	16
delim	19	roman	12
dot	13	rpile	17
dotdot	13	size	12
down	21	sqrt	10
dyad	13	sub	7
fat	12	sup	7
font	12	tdefine	20
from	11	tilde	13
fwd	21	to	11
gfont	12	under	13
gsize	12	up	21
hat	13	vec	13
italic	12	~, ^	4, 6
lcol	18	{ }	8
left	16	"..."	8, 14
lineup	15
.sp
.in 0
.fi
.SC Troubleshooting
.PP
If you make a mistake in an equation,
like leaving out a brace (very common)
or having one too many (very common)
or having a
.ul
sup
with nothing before it (common),
.UC EQN
will tell you with the message
.P1 2
.ft I
syntax error between lines x and y, file z
.ft R
.P2
where
.ul
x
and
.ul
y
are approximately the lines
between which the trouble occurred, and
.ul
z
is the name
of the file in question.
The line numbers are approximate _ look nearby as well.
There are also self-explanatory messages that arise if you leave out a quote
or try to run
.UC EQN
on a non-existent file.
.PP
If you want to check a document before actually printing it
(on
.UC UNIX 
only),
.P1
eqn  files >/dev/null
.P2
will
throw away the output but print the messages.
.PP
If you use something like dollar signs as delimiters,
it is easy to leave one out.
This causes very strange troubles.
The program
.ul
checkeq
(on
.UC GCOS ,
use
.ul
\&./checkeq
instead)
checks for misplaced or missing dollar signs
and similar troubles.
.PP
In-line equations can only be so big
because of an internal buffer in
.UC TROFF .
If you get a message
``word overflow'',
you have exceeded this limit.
If you print the equation as a displayed equation
this message will usually go away.
The message
``line overflow''
indicates you have exceeded an even bigger buffer.
The only cure for this is to break the equation into two separate ones.
.PP
On a related topic,
.UC EQN
does not break equations by itself _
you must split long equations up across multiple lines
by yourself,
marking each by a separate
.UC .EQ
\&...\&
.UC .EN
sequence.
.UC EQN
does warn about equations that are too long
to fit on one line.
