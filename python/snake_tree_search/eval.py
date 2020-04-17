from agents import *
from game import *

# game.body = [Vector(2,2)]
# game.fruit = Vector(3,2)
def run_game(game, agent):
    while not game.dead and len(game.get_legal_actions(SNAKE)) > 0:
        # print(game)
        move = agent.choose_action(game)
        game.move_snake(move, should_respawn_fruit=True)
    # print(game.status())
    return game.status()
def avg(xs):
    return sum(xs) / max(1, len(xs))

def eval(episodes, size, fuel, discount_rate):
    print(locals())
    ages = []
    lengths = []
    for _ in range(episodes):
        game = Game(size,size)
        agent = MinimaxAgent(fuel, discount_rate)
        status = run_game(game, agent)
        ages.append(status["age"])
        lengths.append(status["length"])
    return avg(ages), avg(lengths)

avg_age, avg_length = eval(50, 10, 2, .5)
print({"avg_age": avg_age, "avg_length": avg_length})
