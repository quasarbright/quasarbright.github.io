import sys
import pygame
import pygame.time
import noise
import colorsys

pygame.init()

sx = sy = .05
st = .5

size = width, height = (600,600)
screen = pygame.display.set_mode(size)
pw, ph = 100,100
poppy = pygame.Surface((pw, ph))

clock = pygame.time.Clock()

def getColor(x, y, t):
    h = noise.pnoise3(x * sx, y * sy, t * st, octaves=2)
    h = (1 + h) / 2
    h += t*st/10 % 1
    s = .9
    l = 1

    r,g,b = colorsys.hsv_to_rgb(h,s,l)
    r,g,b = [int(x*255) for x in (r,g,b)]
    return r,g,b


while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()
    
    for x in range(pw):
        for y in range(ph):
            poppy.set_at((x, y), getColor(x, y, pygame.time.get_ticks() / 1000))
    poppy_ = pygame.transform.scale(poppy, (width, height))
    screen.blit(poppy_, poppy_.get_rect())
    
    
    pygame.display.flip()

        