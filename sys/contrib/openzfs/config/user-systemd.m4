AC_DEFUN([ZFS_AC_CONFIG_USER_SYSTEMD], [
	AC_ARG_ENABLE(systemd,
		AC_HELP_STRING([--enable-systemd],
		[install systemd unit/preset files [[default: yes]]]),
		[enable_systemd=$enableval],
		[enable_systemd=check])

	AC_ARG_WITH(systemdunitdir,
		AC_HELP_STRING([--with-systemdunitdir=DIR],
		[install systemd unit files in dir [[/usr/lib/systemd/system]]]),
		systemdunitdir=$withval,systemdunitdir=/usr/lib/systemd/system)

	AC_ARG_WITH(systemdpresetdir,
		AC_HELP_STRING([--with-systemdpresetdir=DIR],
		[install systemd preset files in dir [[/usr/lib/systemd/system-preset]]]),
		systemdpresetdir=$withval,systemdpresetdir=/usr/lib/systemd/system-preset)

	AC_ARG_WITH(systemdmodulesloaddir,
		AC_HELP_STRING([--with-systemdmodulesloaddir=DIR],
		[install systemd module load files into dir [[/usr/lib/modules-load.d]]]),
		systemdmodulesloaddir=$withval,systemdmodulesloaddir=/usr/lib/modules-load.d)

	AC_ARG_WITH(systemdgeneratordir,
		AC_HELP_STRING([--with-systemdgeneratordir=DIR],
		[install systemd generators in dir [[/usr/lib/systemd/system-generators]]]),
		systemdgeneratordir=$withval,systemdgeneratordir=/usr/lib/systemd/system-generators)

	AS_IF([test "x$enable_systemd" = xcheck], [
		AS_IF([systemctl --version >/dev/null 2>&1],
			[enable_systemd=yes],
			[enable_systemd=no])
	])

	AC_MSG_CHECKING(for systemd support)
	AC_MSG_RESULT([$enable_systemd])

	AS_IF([test "x$enable_systemd" = xyes], [
		ZFS_INIT_SYSTEMD=systemd
		ZFS_MODULE_LOAD=modules-load.d
		DEFINE_SYSTEMD='--with systemd --define "_unitdir $(systemdunitdir)" --define "_presetdir $(systemdpresetdir)" --define "_generatordir $(systemdgeneratordir)"'
		modulesloaddir=$systemdmodulesloaddir
	],[
		DEFINE_SYSTEMD='--without systemd'
	])

	AC_SUBST(ZFS_INIT_SYSTEMD)
	AC_SUBST(ZFS_MODULE_LOAD)
	AC_SUBST(DEFINE_SYSTEMD)
	AC_SUBST(systemdunitdir)
	AC_SUBST(systemdpresetdir)
	AC_SUBST(systemdgeneratordir)
	AC_SUBST(modulesloaddir)
])
