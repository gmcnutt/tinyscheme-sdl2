#include <SDL2/SDL.h>
#include <SDL2/SDL_log.h>
#include "scheme-private.h"


/**
 * Initialize the SDL2 subsystem.
 */
static pointer sdl2_init(scheme *sc, pointer args)
{
	/* Initialize the video, event, threading and file i/o subsystems. */
	if (SDL_Init(SDL_INIT_VIDEO)) {
		SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
				"SDL_Init: %s\n", SDL_GetError());
		return sc->NIL;
	}
	return sc->T;
}


/**
 * Initialize this dynamic extension.
 *
 * This is called by tinyscheme in response to
 *
 *     (load-extension "sdl2")
 *
 * It adds all of our functions into the scheme environment.
 */
void init_sdl2(scheme *sc)
{
	sc->vptr->scheme_define(
		sc, sc->global_env, sc->vptr->mk_symbol(sc, "sdl2-init"),
		sc->vptr->mk_foreign_func(sc, sdl2_init));
}
