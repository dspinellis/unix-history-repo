TROFF=	vtroff -t

print: trofftxt
	vpr -t trofftxt

troff:
	/usr/ucb/soelim pxin?.n | tbl | ${TROFF}

trofftxt: fig2.3.n
	/usr/ucb/soelim pxin?.n | tbl | /usr/ucb/vtroff -t >trofftxt

fig2.3.n: fig2.3.raw
	sort fig2.3.raw >fig2.3.n
