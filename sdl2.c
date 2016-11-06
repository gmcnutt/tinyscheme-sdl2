#include "log.h"
#include "scm.h"
#include <SDL2/SDL.h>


/**
 * Initialize the SDL2 subsystem.
 */
static pointer sdl2_init(scheme *sc, pointer args)
{
	/* Initialize the video, event, threading and file i/o subsystems. */
	if (SDL_Init(SDL_INIT_VIDEO)) {
		log_critical("SDL_Init: %s\n", SDL_GetError());
		return sc->NIL;
	}

	/* Cleanup SDL on exit. */
	atexit(SDL_Quit);

	/* Set logging level. */
	log_init();

	return sc->T;
}


/**
 * Wrapper for SDL_CreateWindow.
 */
static pointer sdl2_create_window(scheme *sc, pointer args)
{
	SDL_Window *window;

	log_debug("%s:enter\n", __FUNCTION__);

	/* Create the main window */
	if (! (window = SDL_CreateWindow(
		       "Demo", SDL_WINDOWPOS_UNDEFINED,
		       SDL_WINDOWPOS_UNDEFINED, 640, 480,
		       SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN))) {
		log_error("SDL_CreateWindow: %s\n", SDL_GetError());
		return sc->NIL;
	}

	/* Return the window as a foreign pointer. */
	return scm_mk_ptr(sc, window);
}


/**
 * Wrapper for SDL_DestroyWindow.
 *
 * (sdl2-destroy-window window)
 */
static pointer sdl2_destroy_window(scheme *sc, pointer args)
{
	SDL_Window *window=NULL;

	log_debug("%s:enter\n", __FUNCTION__);

    if (scm_unpack(sc, &args, "p", &window)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	SDL_DestroyWindow(window);
	return sc->T;
}


/**
 * Wrapper for SDL_CreateRenderer.
 *
 * (let ((renderer (sdl2-create-renderer window))) ...)
 */
static pointer sdl2_create_renderer(scheme *sc, pointer args)
{
	SDL_Window *window=NULL;
	SDL_Renderer *renderer=NULL;

	log_debug("%s:enter\n", __FUNCTION__);

    if (scm_unpack(sc, &args, "p", &window)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	if (! (renderer = SDL_CreateRenderer(window, -1, 0))) {
		log_error("SDL_CreateRenderer: %s\n", SDL_GetError());
		return sc->NIL;
	}

    log_debug("%s: created %p\n", __FUNCTION__, renderer);
	return scm_mk_ptr(sc, renderer);
}


/**
 * Wrapper for SDL_DestroyRenderer.
 *
 * (sdl2-destroy-renderer renderer)
 */
static pointer sdl2_destroy_renderer(scheme *sc, pointer args)
{
	SDL_Renderer *renderer=NULL;

	log_debug("%s:enter\n", __FUNCTION__);

    if (scm_unpack(sc, &args, "p", &renderer)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	SDL_DestroyRenderer(renderer);
	return sc->T;

}


/**
 * Wrapper for SDL_SetRenderDrawColor.
 *
 * (let ((red 255)
 *       (green 128)
 *       (blue 64)
 *       (alpha sdl2-alpha-opaque))
 *   (sdl2-set-render-draw-color renderer red green blue alpha)
 *   ...
 *   )
 */
static pointer sdl2_set_render_draw_color(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        int red, green, blue, alpha;
        log_debug("%s:enter\n", __FUNCTION__);
        if (scm_unpack(sc, &args, "pdddd", &renderer, &red, &green, &blue,
                       &alpha)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }
        log_debug("%s:%p %d %d %d %d\n", __FUNCTION__,
                  renderer, red, green, blue, alpha);
        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }
        SDL_SetRenderDrawColor(renderer, red, green, blue, alpha);
        log_debug("%s:exit\n", __FUNCTION__);
        return sc->NIL;
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
	scm_define_api_call(sc, "sdl2-init", sdl2_init);
	scm_define_api_call(sc, "sdl2-create-window", sdl2_create_window);
	scm_define_api_call(sc, "sdl2-destroy-window", sdl2_destroy_window);
	scm_define_api_call(sc, "sdl2-create-renderer",
			    sdl2_create_renderer);
	scm_define_api_call(sc, "sdl2-destroy-renderer",
			    sdl2_destroy_renderer);
	scm_define_api_call(sc, "sdl2-set-render-draw-color",
			    sdl2_set_render_draw_color);
    scm_define_int(sc, "sdl2-alpha-opaque", SDL_ALPHA_OPAQUE);
}
