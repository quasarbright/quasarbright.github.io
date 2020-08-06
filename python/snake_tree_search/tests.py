import unittest
from game import *

class GameTests(unittest.TestCase):
    def setUp(self):
        self.game = Game(5, 5)
        self.game.fruit = Vector(1, 1)
        self.game.body = [Vector(1, 2)]
        '''
        0 0 0 0 0
        0 F 0 0 0
        0 H 0 0 0
        0 0 0 0 0
        0 0 0 0 0
        '''
    
    def test_death_by_tail(self):
        self.game.move_snake(UP)
        self.game.fruit = Vector(2, 1)
        self.game.move_snake(RIGHT)
        self.game.fruit = Vector(3, 1)
        self.game.move_snake(RIGHT)
        self.game.fruit = Vector(4, 1)
        self.game.move_snake(RIGHT)
        self.game.fruit = Vector(1, 0)
        self.game.move_snake(DOWN)
        self.game.move_snake(LEFT)
        self.game.move_snake(UP)
        self.assertTrue(self.game.dead)
        self.assertEqual(len(self.game), 5)
        self.assertEqual(self.game.age, 7)
    
    def test_death_by_tail_simple(self):
        self.game.move_snake(UP)
        self.game.fruit = Vector(1, 0)
        self.game.move_snake(UP)
        self.game.fruit = Vector(2, 1)
        self.game.move_snake(DOWN)
        self.assertTrue(self.game.dead)
        self.assertEqual(len(self.game), 3)
        self.assertEqual(self.game.age, 3)
    
    def test_death_by_wall(self):
        self.game.move_snake(UP)
        self.game.move_snake(UP)
        self.game.move_snake(UP)
        self.assertTrue(self.game.dead)
        self.assertEqual(len(self.game), 2)
        self.assertEqual(self.game.age, 3)
    
    def test_value(self):
        li0 = True
        li1 = False
        le0 = 5
        le1 = 50
        a0 = 100
        a1 = 10
        def make_game(li, le, a):
            g = Game(10,10)
            g.dead = li
            g.body = [Vector(0,0) for _ in range(le)]
            g.age = a
            return g
        g0 = make_game(li0, le0, a0)
        g1 = make_game(li1, le1, a1)
        self.assertLess(g0.value(), g1.value())
        g0 = make_game(li0, le1, a1)
        g1 = make_game(li1, le0, a0)
        self.assertLess(g0.value(), g1.value())
        g0 = make_game(li1, le0, a1)
        g1 = make_game(li1, le1, a0)
        self.assertLess(g0.value(), g1.value())
        g0 = make_game(li1, le1, a0)
        g1 = make_game(li1, le1, a1)
        self.assertLess(g0.value(), g1.value())

if __name__ == "__main__":
    unittest.main()