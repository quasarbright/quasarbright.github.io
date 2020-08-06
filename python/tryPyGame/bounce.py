import sys
import pygame
import pygame.time
import noise
pygame.init()
clock = pygame.time.Clock()

size = width, height = (400,800)

screen = pygame.display.set_mode(size)

speed = [1,2]
position = [0,0]
while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()
    pygame.draw.rect(screen, (51,51,51), (0,0,width,height))
    position[0] += speed[0]
    position[1] += speed[1]
    if position[0] < 0 or position[0] >= width - 50:
        speed[0] *= -1
    if position[1] < 0 or position[1] >= height - 50:
        speed[1] *= -1
    pygame.draw.rect(screen, (255,0,0), (position[0], position[1], 50, 50))
    pygame.display.flip()
    clock.tick_busy_loop(60)
    