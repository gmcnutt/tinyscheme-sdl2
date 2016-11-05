#include <stdio.h>
#include "scheme-private.h"


pointer sdl2_init(scheme *sc, pointer args)
{
	printf("sdl2_init called\n");
	return sc->T;
}


void init_sdl2(scheme *sc)
{
	sc->vptr->scheme_define(
		sc, sc->global_env, sc->vptr->mk_symbol(sc, "sdl2-init"),
		sc->vptr->mk_foreign_func(sc, sdl2_init));
}
