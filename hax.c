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
 * (hax-blit renderer texture textsrc mapx mapy)
 */
static pointer hax_blit(scheme *sc, pointer args)
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
 * (hax-fill renderer texture textsrc mapdst)
 */
static pointer hax_fill(scheme *sc, pointer args)
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

void init_hax(scheme *sc)
{
        scm_define_api_call(sc, "hax-blit", hax_blit);
        scm_define_api_call(sc, "hax-fill", hax_fill);
}
