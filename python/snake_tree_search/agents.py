from game import *
from functools import lru_cache

class Agent:
    def __init__(self, fuel):
        '''fuel is the max depth of the search
        put double the number of snake moves you want to think ahead by
        '''
        self.fuel = fuel
    
    def value(self, player, game : Game, fuel, rewards):
        raise NotImplementedError("implement value")

    def choose_action(self, game : Game):
        legal_actions = game.get_legal_actions(SNAKE)
        random.shuffle(legal_actions) # random tie breaker
        assert len(legal_actions) > 0
        def value_of_action(action):
            next_game, reward = game.try_action(SNAKE, action)
            return self.value(SNAKE, next_game, self.fuel, (reward,))
        return max(legal_actions, key=value_of_action)

class MinimaxAgent(Agent):
    def __init__(self, fuel, discount_rate):
        '''fuel is the max depth of the search
        put double the number of snake moves you want to think ahead by
        '''
        super().__init__(fuel)
        self.discount_rate = discount_rate
    
    @lru_cache(maxsize=256)
    def value(self, player, game : Game, fuel, rewards):
        '''returns the value for the snake in this state
        '''
        player = SNAKE ##
        if game.dead or fuel <= 0: #or len(game.get_legal_actions(player)) == 0:
            # terminal state
            discounted_reward = 0
            for reward in rewards[::-1]:
                discounted_reward = reward + self.discount_rate * discounted_reward
            if rewards[0] == 10:
                zzz = 10
                pass
            return discounted_reward
        
        def help(game : Game, fuel, player, next_player, vals_fn, rewards):
            legal_actions = game.get_legal_actions(player)
            random.shuffle(legal_actions) # for random tie breaker
            assert len(legal_actions) > 0
            if player == FRUIT:
                legal_actions = [random.choice(legal_actions)] # CHEAP EXPECTIMAX !!!!!!!!
            # if len(legal_actions) == 0:
            #     return game.value()
            def value_of_action(action):
                nonlocal rewards
                new_rewards = rewards
                next_game, reward = game.try_action(player, action)
                if player == SNAKE:
                    new_rewards += (reward,)
                return self.value(next_player, next_game, fuel - 1, new_rewards)
            return vals_fn(list(map(value_of_action, legal_actions)))
        
        def expected_val(game : Game, fuel, rewards):
            return help(game, fuel, FRUIT, SNAKE, lambda vals : sum(vals) / max(1, len(vals)), rewards)
        
        def max_val(game : Game, fuel, rewards):
            return help(game, fuel, SNAKE, FRUIT, max, rewards)

        if player == SNAKE:
            return max_val(game, fuel, rewards)
        elif player == FRUIT:
            return expected_val(game, fuel, rewards)
        else:
            raise ValueError("unknown player: {}".format(player))
