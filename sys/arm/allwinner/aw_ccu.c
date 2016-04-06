/*-
 * Copyright (c) 2016 Jared McNeill <jmcneill@invisible.ca>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */

/*
 * Allwinner oscillator clock
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/bus.h>
#include <sys/rman.h>
#include <sys/kernel.h>
#include <sys/module.h>
#include <machine/bus.h>

#include <dev/fdt/simplebus.h>

#include <dev/ofw/ofw_bus.h>
#include <dev/ofw/ofw_bus_subr.h>

#include <dev/extres/clk/clk.h>

#include "clkdev_if.h"

#define	CCU_BASE	0x01c20000
#define	CCU_SIZE	0x400

struct aw_ccu_softc {
	struct simplebus_softc	sc;
	bus_space_tag_t		bst;
	bus_space_handle_t	bsh;
	struct mtx		mtx;
};

static struct ofw_compat_data compat_data[] = {
	{ "allwinner,sun4i-a10",	1 },
	{ "allwinner,sun7i-a20",	1 },
	{ "allwinner,sun6i-a31",	1 },
	{ "allwinner,sun6i-a31s",	1 },
	{ NULL, 0 }
};

static int
aw_ccu_check_addr(bus_addr_t addr)
{
	if (addr < CCU_BASE || addr >= (CCU_BASE + CCU_SIZE))
		return (EINVAL);
	return (0);
}

static int
aw_ccu_write_4(device_t dev, bus_addr_t addr, uint32_t val)
{
	struct aw_ccu_softc *sc;

	if (aw_ccu_check_addr(addr) != 0)
		return (EINVAL);

	sc = device_get_softc(dev);
	mtx_assert(&sc->mtx, MA_OWNED);
	bus_space_write_4(sc->bst, sc->bsh, addr - CCU_BASE, val);

	return (0);
}

static int
aw_ccu_read_4(device_t dev, bus_addr_t addr, uint32_t *val)
{
	struct aw_ccu_softc *sc;

	if (aw_ccu_check_addr(addr) != 0)
		return (EINVAL);

	sc = device_get_softc(dev);
	mtx_assert(&sc->mtx, MA_OWNED);
	*val = bus_space_read_4(sc->bst, sc->bsh, addr - CCU_BASE);

	return (0);
}

static int
aw_ccu_modify_4(device_t dev, bus_addr_t addr, uint32_t clr, uint32_t set)
{
	struct aw_ccu_softc *sc;
	uint32_t val;

	if (aw_ccu_check_addr(addr) != 0)
		return (EINVAL);

	sc = device_get_softc(dev);
	mtx_assert(&sc->mtx, MA_OWNED);
	val = bus_space_read_4(sc->bst, sc->bsh, addr - CCU_BASE);
	val &= ~clr;
	val |= set;
	bus_space_write_4(sc->bst, sc->bsh, addr - CCU_BASE, val);

	return (0);
}

static void
aw_ccu_device_lock(device_t dev)
{
	struct aw_ccu_softc *sc;

	sc = device_get_softc(dev);
	mtx_lock(&sc->mtx);
}

static void
aw_ccu_device_unlock(device_t dev)
{
	struct aw_ccu_softc *sc;

	sc = device_get_softc(dev);
	mtx_unlock(&sc->mtx);
}

static int
aw_ccu_probe(device_t dev)
{
	const char *name;
	device_t pdev;

	name = ofw_bus_get_name(dev);

	if (name == NULL || strcmp(name, "clocks") != 0)
		return (ENXIO);

	pdev = device_get_parent(dev);
	if (ofw_bus_search_compatible(pdev, compat_data)->ocd_data == 0)
		return (0);

	device_set_desc(dev, "Allwinner Clock Control Unit");
	return (BUS_PROBE_SPECIFIC);
}

static int
aw_ccu_attach(device_t dev)
{
	struct aw_ccu_softc *sc;
	phandle_t node, child;
	device_t cdev;
	int error;

	sc = device_get_softc(dev);
	node = ofw_bus_get_node(dev);

	simplebus_init(dev, node);

	/*
	 * Map CCU registers. The DT doesn't have a "reg" property for the
	 * /clocks node and child nodes have conflicting "reg" properties.
	 */
	sc->bst = bus_get_bus_tag(dev);
	error = bus_space_map(sc->bst, CCU_BASE, CCU_SIZE, 0, &sc->bsh);
	if (error != 0) {
		device_printf(dev, "couldn't map CCU: %d\n", error);
		return (error);
	}

	mtx_init(&sc->mtx, device_get_nameunit(dev), NULL, MTX_DEF);

	/* Attach child devices */
	for (child = OF_child(node); child > 0; child = OF_peer(child)) {
		cdev = simplebus_add_device(dev, child, 0, NULL, -1, NULL);
		if (cdev != NULL)
			device_probe_and_attach(cdev);
	}

	return (bus_generic_attach(dev));
}

static device_method_t aw_ccu_methods[] = {
	/* Device interface */
	DEVMETHOD(device_probe,		aw_ccu_probe),
	DEVMETHOD(device_attach,	aw_ccu_attach),

	/* clkdev interface */
	DEVMETHOD(clkdev_write_4,	aw_ccu_write_4),
	DEVMETHOD(clkdev_read_4,	aw_ccu_read_4),
	DEVMETHOD(clkdev_modify_4,	aw_ccu_modify_4),
	DEVMETHOD(clkdev_device_lock,	aw_ccu_device_lock),
	DEVMETHOD(clkdev_device_unlock,	aw_ccu_device_unlock),

	DEVMETHOD_END
};

DEFINE_CLASS_1(aw_ccu, aw_ccu_driver, aw_ccu_methods,
    sizeof(struct aw_ccu_softc), simplebus_driver);

static devclass_t aw_ccu_devclass;

EARLY_DRIVER_MODULE(aw_ccu, simplebus, aw_ccu_driver, aw_ccu_devclass,
    0, 0, BUS_PASS_BUS + BUS_PASS_ORDER_MIDDLE);

MODULE_VERSION(aw_ccu, 1);
