FP_MAN = 	manDefs.rno manCh0.rno manCh1.rno\
		manCh2.rno manCh3.rno manCh4.rno manCh5.rno\
		manCh6.rno manCh7.rno refs.rno manApp.rno

man:		${FP_MAN}
		tbl ${FP_MAN} | eqn | vtroff -me
