from game import *
from functools import lru_cache

class Agent:
    def __init__(self, fuel):
        '''fuel is the max depth of the search
        put double the number of snake moves you want to think ahead by
        '''
        self.fuel = fuel
    
    def value(self, player, game : Game, fuel):
        raise NotImplementedError("implement value")

    def choose_action(self, game : Game):
        legal_actions = game.get_legal_actions(SNAKE)
        assert len(legal_actions) > 0
        if game.age == 118:
            zzz = 2
            pass
        def value_of_action(action):
            next_game, reward = game.try_action(SNAKE, action)
            return self.value(SNAKE, next_game, self.fuel)
        return max(legal_actions, key=value_of_action)

class MinimaxAgent(Agent):
    def __init__(self, fuel):
        '''fuel is the max depth of the search
        put double the number of snake moves you want to think ahead by
        '''
        super().__init__(fuel)
    
    @lru_cache(maxsize=256)
    def value(self, player, game : Game, fuel):
        '''returns the value for the snake in this state
        '''
        if game.dead or fuel <= 0 or len(game.get_legal_actions(player)) == 0:
            # terminal state
            return game.value()
        
        def help(game : Game, fuel, player, next_player, vals_fn):
            legal_actions = game.get_legal_actions(player)
            random.shuffle(legal_actions) # for random tie breaker
            assert len(legal_actions) > 0
            if player == FRUIT:
                legal_actions = [random.choice(legal_actions)] # CHEAP EXPECTIMAX !!!!!!!!
            # if len(legal_actions) == 0:
            #     return game.value()
            def value_of_action(action):
                next_game, reward = game.try_action(player, action)
                return self.value(next_player, next_game, fuel - 1)
            return vals_fn(list(map(value_of_action, legal_actions)))
        
        def expected_val(game : Game, fuel):
            return help(game, fuel, FRUIT, SNAKE, lambda vals : sum(vals) / max(1, len(vals)))
        
        def max_val(game : Game, fuel):
            return help(game, fuel, SNAKE, FRUIT, max)

        if player == SNAKE:
            return max_val(game, fuel)
        elif player == FRUIT:
            return expected_val(game, fuel)
        else:
            raise ValueError("unknown player: {}".format(player))
