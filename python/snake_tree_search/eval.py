from agents import *
from game import *

game = Game(8,8)
agent = MinimaxAgent(10)

while not game.dead and game.age < 500 and len(game.get_legal_actions(SNAKE)) > 0:
    print(game)
    move = agent.choose_action(game)
    game.move_snake(move, should_respawn_fruit=True)

print(game.status())
