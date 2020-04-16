from agents import *
from game import *

game = Game(10,10)
# game.body = [Vector(2,2)]
# game.fruit = Vector(3,2)
agent = MinimaxAgent(5, .5)

while not game.dead and len(game.get_legal_actions(SNAKE)) > 0:
    print(game)
    move = agent.choose_action(game)
    game.move_snake(move, should_respawn_fruit=True)

print(game.status())
