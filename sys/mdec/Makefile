ALL=	htboot mtboot tmboot tsboot hpboot hkboot upboot noboot noboot \
	httoggle mttoggle tmtoggle tstoggle

all: ${ALL}

htboot: htboot.s
	as htboot.s
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=htboot conv=sync

mtboot: mtboot.s
	as mtboot.s
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=mtboot conv=sync

tmboot: tmboot.s
	as tmboot.s
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=tmboot conv=sync

tsboot: tsboot.s
	as tsboot.s
	nm -u a.out
	strip a.out
	dd if=a.out of=b.out bs=32 skip=1
	dd if=b.out of=tsboot conv=sync

hpboot: hpboot.s
	as hpboot.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=hpboot conv=sync

hkboot: hkboot.s
	as hkboot.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=hkboot conv=sync

upboot: upboot.s
	as upboot.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=b.out
	dd if=b.out of=upboot conv=sync

httoggle: httoggle.s
	as httoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=httoggle

mttoggle: mttoggle.s
	as mttoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=mttoggle

tstoggle: tstoggle.s
	as tstoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=tstoggle

tmtoggle: tmtoggle.s
	as tmtoggle.s
	nm -u a.out
	strip a.out
	dd if=a.out bs=32 skip=1 of=tmtoggle

noboot:
	echo | dd of=noboot conv=sync

clean:
	rm -f a.out b.out ${ALL}

install:
	cp *boot /usr/mdec
	rm -f /usr/mdec/mboot
	ln /usr/mdec/htboot /usr/mdec/mboot
	rm -f /usr/mdec/uboot
	ln /usr/mdec/hpboot /usr/mdec/uboot
