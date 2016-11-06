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

	/* Create the main window */
	if (! (window = SDL_CreateWindow(
		       "Demo", SDL_WINDOWPOS_UNDEFINED,
		       SDL_WINDOWPOS_UNDEFINED, 640, 480,
		       SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN))) {
		log_error("SDL_CreateWindow: %s\n", SDL_GetError());
		return sc->NIL;
	}

	/* Return the window as a foreign pointer. */
    log_debug("%s:%p\n", __FUNCTION__, window);
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

    if (scm_unpack(sc, &args, "p", &window)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

    log_debug("%s:%p\n", __FUNCTION__, window);
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

    if (scm_unpack(sc, &args, "p", &window)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	if (! (renderer = SDL_CreateRenderer(window, -1, 0))) {
		log_error("SDL_CreateRenderer: %s\n", SDL_GetError());
		return sc->NIL;
	}

    log_debug("%s:%p\n", __FUNCTION__, renderer);
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

    if (scm_unpack(sc, &args, "p", &renderer)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	log_debug("%s:%p\n", __FUNCTION__, renderer);

	SDL_DestroyRenderer(renderer);
	return sc->T;

}


/**
 * Wrapper for SDL_SetRenderDrawColor.
 *
 * (sdl2-set-render-draw-color renderer red green blue alpha)
 *
 * Where red, green, blue and alpha are all 0-255.
 */
static pointer sdl2_set_render_draw_color(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        int red, green, blue, alpha;

        if (scm_unpack(sc, &args, "pdddd", &renderer, &red, &green, &blue,
                       &alpha)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }
        SDL_SetRenderDrawColor(renderer, red, green, blue, alpha);

        return sc->T;
}


/**
 * Wrapper for SDL_RenderClear.
 *
 * (sdl2-render-clear renderer)
 */
static pointer sdl2_render_clear(scheme *sc, pointer args)
{
	SDL_Renderer *renderer=NULL;

    if (scm_unpack(sc, &args, "p", &renderer)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	SDL_RenderClear(renderer);
	return sc->T;
}


/**
 * Wrapper for SDL_RenderDrawLine.
 *
 * (sdl2-render-draw-line renderer x0 y0 x1 y1)
 *
 * Draws a line on the current rendering target.
 */
static pointer sdl2_render_draw_line(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        int x0, y0, x1, y1;

        if (scm_unpack(sc, &args, "pdddd", &renderer, &x0, &y0, &x1, &y1)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }
        SDL_RenderDrawLine(renderer, x0, y0, x1, y1);
        return sc->T;
}


/**
 * Wrapper for SDL_RenderPresent.
 *
 * (sdl2-render-present renderer)
 *
 * Update the screen with any rendering performed since the previous call.
 */
static pointer sdl2_render_present(scheme *sc, pointer args)
{
	SDL_Renderer *renderer=NULL;

    if (scm_unpack(sc, &args, "p", &renderer)) {
            log_error("%s: %s\n", __FUNCTION__, scm_get_error());
            return sc->NIL;
	}

	SDL_RenderPresent(renderer);
	return sc->T;
}


/**
 * Wrapper for SDL_PollEvent.
 *
 * (let ((event (sdl2-poll-event))) ...)
 */
static pointer sdl2_poll_event(scheme *sc, pointer args)
{
        SDL_Event event;

        if (! SDL_PollEvent(&event)) {
                return sc->NIL;
        }

        return scm_mk_int(sc, event.type);
}


/**
 * Wrapper for SDL_Delay.
 *
 * (sdl2-delay ms)
 */
static pointer sdl2_delay(scheme *sc, pointer args)
{
        int ms;

        if (scm_unpack(sc, &args, "d", &ms)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->F;
        }
        SDL_Delay(ms);
        return sc->T;
}


/**
 * Wrapper for SDL_GetTicks.
 */
static pointer sdl2_get_ticks(scheme *sc, pointer args)
{
        return scm_mk_int(sc, SDL_GetTicks());
}

/**
 * Initialize this dynamic extension.
 *
 * This is called by tinyscheme in response to
 *
 *     (load-extension "sdl2")
 *
 * Adds all of our functions into the scheme environment.
 */
void init_sdl2(scheme *sc)
{
	scm_define_api_call(sc, "sdl2-init", sdl2_init);

	scm_define_api_call(sc, "sdl2-create-renderer", sdl2_create_renderer);
	scm_define_api_call(sc, "sdl2-create-window", sdl2_create_window);
	scm_define_api_call(sc, "sdl2-destroy-renderer", sdl2_destroy_renderer);
	scm_define_api_call(sc, "sdl2-destroy-window", sdl2_destroy_window);
	scm_define_api_call(sc, "sdl2-render-draw-line", sdl2_render_draw_line);
	scm_define_api_call(sc, "sdl2-set-render-draw-color", sdl2_set_render_draw_color);
    scm_define_api_call(sc, "sdl2-poll-event", sdl2_poll_event);
    scm_define_api_call(sc, "sdl2-render-clear", sdl2_render_clear);
    scm_define_api_call(sc, "sdl2-render-present", sdl2_render_present);
    scm_define_api_call(sc, "sdl2-delay", sdl2_delay);
    scm_define_api_call(sc, "sdl2-get-ticks", sdl2_get_ticks);

    scm_define_int(sc, "sdl2-alpha-opaque", SDL_ALPHA_OPAQUE);

    scm_define_int(sc, "sdl2-quit", SDL_QUIT);
    scm_define_int(sc, "sdl2-mouse-button-down", SDL_MOUSEBUTTONDOWN);
}
