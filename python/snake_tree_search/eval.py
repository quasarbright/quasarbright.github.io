from agents import *
from game import *

game = Game(5,5)
agent = MinimaxAgent(14)

while not game.dead and len(game.get_legal_actions(SNAKE)) > 0:
    print(game)
    move = agent.choose_action(game)
    game.move_snake(move, should_respawn_fruit=True)

print(game.status())
