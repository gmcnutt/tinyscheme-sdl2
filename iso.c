#include "log.h"
#include "scm.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#define MAP_HEIGHT 10
#define MAP_WIDTH 10
#define TILE_WIDTH 64
#define TILE_HEIGHT 32
#define TILE_WIDTH_HALF (TILE_WIDTH / 2)
#define TILE_HEIGHT_HALF (TILE_HEIGHT / 2)


/**
 * Unpack an SDL_Rect.
 */
static inline int _scm_unpack_rect(scheme *sc, pointer *args, SDL_Rect *rect)
{
        return scm_unpack(
                sc, args, "dddd", &rect->x, &rect->y, &rect->w, &rect->h);
}


static int screen_x(int map_x, int map_y)
{
        return (map_x - map_y) * TILE_WIDTH_HALF;
}


static int screen_y(int map_x, int map_y)
{
        return (map_x + map_y) * TILE_HEIGHT_HALF;
}


/**
 * Blit a texture to an iso grid.
 *
 * (iso-blit renderer texture textsrc mapx mapy)
 */
static pointer iso_blit(scheme *sc, pointer args)
{
        SDL_Rect screen_dst;
        SDL_Rect src, *psrc=NULL;
        SDL_Renderer *renderer=NULL;
        SDL_Texture *texture=NULL;
        pointer plist;
        int map_x, map_y;

        if (scm_unpack(sc, &args, "ppldd", &renderer, &texture, &plist,
                       &map_x, &map_y)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (plist != sc->NIL) {
                if (scm_unpack(sc, &plist, "dddd", &src.x, &src.y, &src.w,
                               &src.h)) {
                        log_error("%s:textsrc:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                psrc = &src;
        }

        screen_dst.x = screen_x(map_x, map_y) + screen_x(MAP_HEIGHT - 1, 0);
        screen_dst.y = screen_y(map_x, map_y);
        screen_dst.w = TILE_WIDTH;
        screen_dst.h = TILE_HEIGHT;
        SDL_RenderCopy(renderer, texture, psrc, &screen_dst);
        return sc->NIL;
}

/**
 * Fill an iso grid with a texture.
 *
 * (iso-fill renderer texture textsrc mapdst)
 */
static pointer iso_fill(scheme *sc, pointer args)
{
        SDL_Rect screen_dst;
        SDL_Rect src, map, *psrc=NULL;
        SDL_Renderer *renderer=NULL;
        SDL_Texture *texture=NULL;
        pointer plist, mlist;
        int row, col, off_x;


        if (scm_unpack(sc, &args, "ppll", &renderer, &texture, &plist,
                       &mlist)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (plist != sc->NIL) {
                if (_scm_unpack_rect(sc, &plist, &src)) {
                        log_error("%s:textsrc:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                psrc = &src;
        }

        if (_scm_unpack_rect(sc, &mlist, &map)) {
                log_error("%s:mapdst:%s\n", __FUNCTION__,
                          scm_get_error());
                return sc->NIL;
        }

        screen_dst.w = TILE_WIDTH;
        screen_dst.h = TILE_HEIGHT;

        off_x = screen_x(map.h - 1, 0);

        for (row = 0; row < map.h; row++) {
                for (col = 0; col < map.w; col++) {
                        screen_dst.x = screen_x(col, row) + off_x;
                        screen_dst.y = screen_y(col, row);
                        SDL_RenderCopy(renderer, texture, psrc, &screen_dst);
                }
        }
        return sc->NIL;
}

/**
 * Render isometrict grid lines.
 *
 * (iso-grid renderer width height)
 */
static pointer iso_grid(scheme *sc, pointer args)
{
        int row, col, width, height, offx;
        SDL_Renderer *renderer;

        if (scm_unpack(sc, &args, "pdd", &renderer, &width, &height)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        offx = screen_x(height, 0);
        for (row = 0; row <= height; row++) {
                SDL_RenderDrawLine(
                        renderer,
                        offx + screen_x(0, row),     screen_y(0, row),
                        offx + screen_x(width, row), screen_y(width, row));
        }
        for (col = 0; col <= width; col++) {
                SDL_RenderDrawLine(
                        renderer,
                        offx + screen_x(col, 0),      screen_y(col, 0),
                        offx + screen_x(col, height), screen_y(col, height));
        }
        return sc->NIL;
}

void init_iso(scheme *sc)
{
        scm_define_api_call(sc, "iso-blit", iso_blit);
        scm_define_api_call(sc, "iso-fill", iso_fill);
        scm_define_api_call(sc, "iso-grid", iso_grid);
}
