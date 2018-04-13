import re
import unittest


POS_STR_REGEXP = r'[a-hA-H][1-8]'


class Board(object):
    def __init__(self):
        self.arr = []
        self.turn = 'white'
        for i in range(8):
            row = []
            for j in range(8):
                row.append(None)
            self.arr.append(row)
        piece_type_order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        for i in range(len(piece_type_order)):
            letters = 'abcdefgh'
            letter = letters[i]

            piece_type = piece_type_order[i]

            self.quick_set_piece(letter+'2', Pawn, 'white')
            self.quick_set_piece(letter+'1', piece_type, 'white')
            self.quick_set_piece(letter+'7', Pawn, 'black')
            self.quick_set_piece(letter+'8', piece_type, 'black')
        # TODO put pieces in arr

    def encode_pos(pos_tup):
        '''(2,1) => "b3"'''
        row, col = pos_tup
        if row < 0 or row > 7:
            return False
        if col < 0 or col > 7:
            return False
        letters = 'abcdefgh'
        return letters[col] + str(row + 1)

    def decode_pos(pos_str):
        '''expects pos_str to be in format "a1"
        returns a (row,col) tuple for use in the board.arr index
        ex: "b3" => (2,1)'''
        match = re.search('^{0}$'.format(POS_STR_REGEXP), pos_str)
        if not match:
            return False
        letters = 'abcdefgh'
        pos_str = pos_str.lower()
        col = letters.index(pos_str[0])
        row = int(pos_str[1]) - 1
        return (row, col)

    def get_piece(self, pos_str):
        row, col = Board.decode_pos(pos_str)
        return self.arr[row][col]

    def set_piece(self, pos_str, piece):
        row, col = Board.decode_pos(pos_str)
        self.arr[row][col] = piece

    def quick_set_piece(self, pos_str, piece_type, color):
        self.set_piece(pos_str, piece_type(pos_str, color, self))

    def in_game_move_piece(self, pos_str, dest_str):
        '''Like move_piece, but cares about colors and check
        To be used in actual game. For testing use move_piece (unless you're testing this)'''
        # make sure the move is actually legal
        board_copy = deepcopy(self)
        if not board_copy.move_piece(pos_str, dest_str):
            return False

        # make sure the player isn't trying to move the other player's piece
        pos_piece = self.get_piece(pos_str)
        if pos_piece.color != self.turn:
            return False

        # make sure that the king isn't open after that move
        # note that board_copy now has the piece moved if the execution gets here
        if board_copy.check_check() == self.turn:
            # the player moved in a way that left his king open
            return False

        if self.turn == 'white':
            self.turn = 'black'
        elif self.turn == 'black':
            self.turn = 'white'
        self.move_piece(pos_str, dest_str)
        return True

    def move_piece(self, pos_str, dest_str):
        '''expects pos_str and dest_str to be in format "a1"'''
        pos_piece = self.get_piece(pos_str)
        dest_piece = self.get_piece(dest_str)
        if pos_piece is None:
            # make sure there is a piece there
            return False
        if pos_piece.move_to(dest_str):  # if the piece can move there
            if self.get_piece(dest_str) is not None:
                del pos_piece  # capture piece
                # TODO make sure you can't capture the king (implement check functionality)
            self.set_piece(dest_str, self.get_piece(pos_str))
            self.set_piece(pos_str, None)
            return True
        return False

    def vec_sum(a, b):
        '''accepts inputs in either 'a2' format or [0,1] format
        returns a+b in [0,1] format'''
        if isinstance(a, str):
            a = Board.decode_pos(a)
        if isinstance(b, str):
            b = Board.decode_pos(b)
        return [a[0] + b[0], a[1] + b[1]]

    def vec_dif(a, b):
        '''accepts inputs in either 'a2' format or [0,1] format
        returns a-b in [0,1] format'''
        if isinstance(a, str):
            a = Board.decode_pos(a)
        if isinstance(b, str):
            b = Board.decode_pos(b)
        return [a[0] - b[0], a[1] - b[1]]

    def get_possible_moves(self,pos_str):
        pos_piece = self.get_piece(pos_str)
        if pos_piece is None:
            return []
        legal_pos_strs = []
        for r in range(8):
            for c in range(8):
                check_pos_tup = (r, c)
                check_pos_str = Board.encode_pos(check_pos_tup)
                if pos_piece.is_move_legal(check_pos_str):
                    legal_pos_strs.append(check_pos_str)
        return legal_pos_strs

    def check_check(self): # haha
        colors_in_check = []
        for r in range(8):
            for c in range(8):
                pos_tup = (r, c)
                pos_str = Board.encode_pos(pos_tup)
                possible_move_strs = self.get_possible_moves(pos_str)
                for possible_move_str in possible_move_strs:
                    possible_move_piece = self.get_piece(possible_move_str)
                    if possible_move_piece is not None:
                        if isinstance(possible_move_piece, King):
                            colors_in_check.append(possible_move_piece.color)
        return colors_in_check

    def __str__(self):
        ans = '  A B C D E F G H\n'
        for i in range(len(self.arr)):
            row = self.arr[i]
            ans += str(i + 1) + '|'
            for piece in row:
                if piece == None:
                    ans += '_'
                else:
                    ans += str(piece)
                ans += '|'
            ans += '\n'
        return ans

    def __deepcopy__(self):
        ans = Board()
        ans.arr = self.arr[:]
        return ans


class Piece(object):
    '''super piece object'''

    def __init__(self, pos_str, color, board):
        '''pos_str is in format "a1"
        color is either "black" or "white"
        board is a board object'''
        self.pos_str = pos_str
        assert color == 'black' or color == 'white'
        self.color = color
        self.board = board

    def is_move_legal(self, dest_str):
        '''note: this only handles cases common between all pieces.
        this method needs to be implemented in all Piece subclasses
        to be called at the beginning of subclasses' is_move_legal overload'''
        match = re.search(r'^[a-hA-H][1-8]$',
                          dest_str)  # make sure it's on the board
        if not match:
            return False
        dest_piece = self.board.get_piece(dest_str)
        if dest_piece is not None:
            if dest_piece.color == self.color:
                return False
        return True

    def move_to(self, dest_str):
        if self.is_move_legal(dest_str):
            self.pos_str = dest_str
            return True
        else:
            return False

    def __str__(self):
        raise Exception('must overload __str__')

    def mystr(self):
        return str(vars(self))


class Pawn(Piece):
    # stuff to remember:
        # first move can be two step
        # diagonal capture
        # can't move backwards
        # transform if move to opposite side
        # can't forward capture
    def __init__(self, pos_str, color, board):
        self.has_moved = False
        super().__init__(pos_str, color, board)

    def move_to(self, dest_str):
        if super().move_to(dest_str):
            # print('super move to worked', vars(self))  # debug print
            self.has_moved = True
            return True

    def is_move_legal(self, dest_str):
        if not super().is_move_legal(dest_str):
            return False
        # else:
            # print('passed the super test!')  # debug print
        # remember vector [1,0] is like going from a1 to a2 so it's down (white starts on top)
        # print('testing if', self.pos_str, 'can go to', dest_str)  # debug print

        # initialize stuff
        base_vec_tup = None
        if self.color == 'white':
            base_vec_tup = [1, 0]
        elif self.color == 'black':
            base_vec_tup = [-1, 0]
        dest_piece = self.board.get_piece(dest_str)

        # move forward one?
        forward_one_pos_tup = Board.vec_sum(self.pos_str, base_vec_tup)
        # print('position of one forward', Board.encode_pos(
        # forward_one_pos_tup))  # debug print
        if dest_str == Board.encode_pos(forward_one_pos_tup):
            # print('you tried to move forward')  # debug print
            if dest_piece == None:
                # print('you can move forward')  # debug print
                return True
            else:
                return False

        # move forward two?
        double_base_vec_tup = Board.vec_sum(base_vec_tup, base_vec_tup)
        forward_two_pos_tup = Board.vec_sum(self.pos_str, double_base_vec_tup)
        # print('position of two forward', Board.encode_pos(
        #     forward_two_pos_tup))  # debug print
        if dest_str == Board.encode_pos(forward_two_pos_tup):
            if self.has_moved:
                return False  # can't move forward twice if you've already moved
            in_between_pos_tup = forward_one_pos_tup
            in_between_pos_str = Board.encode_pos(in_between_pos_tup)
            in_between_piece = self.board.get_piece(in_between_pos_str)
            if in_between_piece != None:
                return False  # can't jump over a piece
            # print('you tried to move forward two')  # debug print
            if dest_piece == None:
                # print('you can move forward two') # debug print
                return True
            else:
                return False

        # capture?
        if dest_piece != None and dest_piece.color != self.color:
            left_forward_base_vec_tup = None
            right_forward_base_vec_tup = None
            if self.color == 'white':
                left_forward_base_vec_tup = Board.vec_sum(base_vec_tup, [0, 1])
                right_forward_base_vec_tup = Board.vec_sum(
                    base_vec_tup, [0, -1])
            elif self.color == 'black':
                left_forward_base_vec_tup = Board.vec_sum(
                    base_vec_tup, [0, -1])
                right_forward_base_vec_tup = Board.vec_sum(
                    base_vec_tup, [0, 1])
            left_forward_pos_tup = Board.vec_sum(
                self.pos_str, left_forward_base_vec_tup)
            right_forward_pos_tup = Board.vec_sum(
                self.pos_str, right_forward_base_vec_tup)
            left_forward_pos_str = Board.encode_pos(left_forward_pos_tup)
            right_forward_pos_str = Board.encode_pos(right_forward_pos_tup)
            if dest_str == left_forward_pos_str or dest_str == right_forward_pos_str:
                return True
        return False

    def __str__(self):
        if self.color == 'white':
            return 'P'
        elif self.color == 'black':
            return 'p'


class Rook(Piece):
    def is_move_legal(self,dest_str):
        if not super().is_move_legal(dest_str):
            return False
        base_vec_tups = (
            (1,0),
            (-1,0),
            (0,1),
            (0,-1)
        )
        for base_vec_tup in base_vec_tups:
            for k in range(1,8):
                scaled_vec_tup =  [k*e for e in base_vec_tup]
                attempt_pos_tup = Board.vec_sum(self.pos_str, scaled_vec_tup)
                attempt_pos_str = Board.encode_pos(attempt_pos_tup)
                if not attempt_pos_str: # encode returns false if tup is invalid
                    break # stop checking this direction because you went out of bounds
                attempt_piece = self.board.get_piece(attempt_pos_str)
                #take advantage of k being checked in increasing order and make sure that you don't skip over a piece
                if attempt_pos_str == dest_str:
                    return True
                else:
                    if attempt_piece is not None:
                        break # stop checking this direction because there is a piece in the way
        return False

    def __str__(self):
        if self.color == 'white':
            return 'R'
        elif self.color == 'black':
            return 'r'


class Bishop(Piece):
    def is_move_legal(self, dest_str):
        if not super().is_move_legal(dest_str):
            return False
        base_vec_tups = (
            (1,1),
            (-1,-1),
            (-1,1),
            (1,-1)
        )
        for base_vec_tup in base_vec_tups:
            for k in range(1,8):
                scaled_vec_tup =  [k*e for e in base_vec_tup]
                attempt_pos_tup = Board.vec_sum(self.pos_str, scaled_vec_tup)
                attempt_pos_str = Board.encode_pos(attempt_pos_tup)
                if not attempt_pos_str: # encode returns false if tup is invalid
                    break # stop checking this direction because you went out of bounds
                attempt_piece = self.board.get_piece(attempt_pos_str)
                #take advantage of k being checked in increasing order and make sure that you don't skip over a piece
                if attempt_pos_str == dest_str:
                    return True
                else:
                    if attempt_piece is not None:
                        break # stop checking this direction because there is a piece in the way
        return False

    def __str__(self):
        if self.color == 'white':
            return 'B'
        elif self.color == 'black':
            return 'b'


class Queen(Piece):
    def is_move_legal(self, dest_str):
        if not super().is_move_legal(dest_str):
            return False
        base_vec_tups = (
            (1,1),
            (-1,-1),
            (-1,1),
            (1,-1),
            (1,0),
            (-1,0),
            (0,1),
            (0,-1)
        )
        for base_vec_tup in base_vec_tups:
            for k in range(1,8):
                scaled_vec_tup =  [k*e for e in base_vec_tup]
                attempt_pos_tup = Board.vec_sum(self.pos_str, scaled_vec_tup)
                attempt_pos_str = Board.encode_pos(attempt_pos_tup)
                if not attempt_pos_str: # encode returns false if tup is invalid
                    break # stop checking this direction because you went out of bounds
                attempt_piece = self.board.get_piece(attempt_pos_str)
                #take advantage of k being checked in increasing order and make sure that you don't skip over a piece
                if attempt_pos_str == dest_str:
                    return True
                else:
                    if attempt_piece is not None:
                        break # stop checking this direction because there is a piece in the way
        return False

    def __str__(self):
        if self.color == 'white':
            return 'Q'
        elif self.color == 'black':
            return 'q'


class Knight(Piece):
    def is_move_legal(self, dest_str):
        if not super().is_move_legal(dest_str):
            return False
        base_vec_tups = []
        for x in [-2,2]:
            for y in [-1,1]:
                base_vec_tups.append([x, y])
                base_vec_tups.append([y, x])
        for base_vec_tup in base_vec_tups:
            attempt_pos_tup = Board.vec_sum(base_vec_tup, self.pos_str)
            attempt_pos_str = Board.encode_pos(attempt_pos_tup)
            if not attempt_pos_str:
                # if it's off the board
                continue
            if attempt_pos_str == dest_str:
                return True
        return False

    def __str__(self):
        if self.color == 'white':
            return 'H' # h for horse
        elif self.color == 'black':
            return 'h'


class King(Piece):
    def is_move_legal(self, dest_str):
        if not super().is_move_legal(dest_str):
            return False
        base_vec_tups = set([])
        for x in [-1,0,1]:
            for y in [-1,0,1]:
                if x or y:
                    base_vec_tups.add((x, y))
                    base_vec_tups.add((y, x))
        # print(base_vec_tups) # debug print
        for base_vec_tup in base_vec_tups:
            attempt_pos_tup = Board.vec_sum(base_vec_tup, self.pos_str)
            attempt_pos_str = Board.encode_pos(attempt_pos_tup)
            if not attempt_pos_str:
                # if it's off the board
                continue
            if attempt_pos_str == dest_str:
                return True
        return False

    def __str__(self):
        if self.color == 'white':
            return 'D'# d for duque
        elif self.color == 'black':
            return 'd'


# testing
class TestEncodeDecode(unittest.TestCase):
    def test_encode_pos(self):
        # successes
        self.assertEqual(Board.encode_pos((2, 1)), 'b3')
        self.assertEqual(Board.encode_pos((0, 1)), 'b1')
        self.assertEqual(Board.encode_pos((7, 7)), 'h8')
        # failures
        self.assertFalse(Board.encode_pos((-1, -1)))
        self.assertFalse(Board.encode_pos((10, -1)))
        self.assertFalse(Board.encode_pos((10, 10)))

    def test_decode_pos(self):
        # successes
        self.assertEqual(Board.decode_pos('b3'), (2, 1))
        self.assertEqual(Board.decode_pos('b1'), (0, 1))
        self.assertEqual(Board.decode_pos('h8'), (7, 7))
        # failures
        self.assertFalse(Board.decode_pos('asdf'))
        self.assertFalse(Board.decode_pos('a9'))
        self.assertFalse(Board.decode_pos('j9'))
        self.assertFalse(Board.decode_pos('hey'))


class TestVecSumDif(unittest.TestCase):
    def test_sum(self):
        self.assertEqual(Board.vec_sum('a1', [1, 0]), [1, 0])
        self.assertEqual(Board.vec_sum('a2', [1, 0]), [2, 0])
        self.assertEqual(Board.vec_sum('a2', 'a2'), [2, 0])
        self.assertEqual(Board.vec_sum([1, 0], [2, 1]), [3, 1])

    def test_dif(self):
        self.assertEqual(Board.vec_dif('a1', 'b3'), [-2, -1])
        self.assertEqual(Board.vec_dif('a2', 'b3'), [-1, -1])
        self.assertEqual(Board.vec_dif('a1', 'a2'), [-1, 0])
        self.assertEqual(Board.vec_dif('a2', 'a1'), [1, 0])
        self.assertEqual(Board.vec_dif([0, 1], [0, 0]), [0, 1])
        self.assertEqual(Board.vec_dif([3, 1], [1, 1]), [2, 0])


class TestPawnMovement(unittest.TestCase):
    '''test whether certain moves result in the correct outcome'''

    def setUp(self):
        self.board = Board()
        self.board.set_piece('a7', Pawn('a7', 'black', self.board))
        self.board.set_piece('e7', Pawn('e7', 'black', self.board))
        self.board.set_piece('a2', Pawn('a2', 'white', self.board))
        self.board.set_piece('e2', Pawn('e2', 'white', self.board))

    def tearDown(self):
        del self.board

    def test_move_forward_one(self):
        '''test if moving forward one works properly'''
        # move a black forward one
        self.assertTrue(self.board.move_piece('a7', 'a6'))
        self.assertIsNotNone(self.board.get_piece('a6'))
        # move it forward again
        self.assertTrue(self.board.move_piece('a6', 'a5'))
        self.assertIsNotNone(self.board.get_piece('a5'))
        # move a white forward
        self.assertTrue(self.board.move_piece('a2', 'a3'))
        self.assertIsNotNone(self.board.get_piece('a3'))
        # move another white forward
        self.assertTrue(self.board.move_piece('e2', 'e3'))
        self.assertIsNotNone(self.board.get_piece('e3'))

    def test_move_forward_two(self):
        '''test if moving forward two works properly'''
        # move a black forward two
        self.assertTrue(self.board.move_piece('a7', 'a5'))
        self.assertIsNotNone(self.board.get_piece('a5'))
        # can't do two again
        self.assertFalse(self.board.move_piece('a5', 'a3'))
        self.assertIsNotNone(self.board.get_piece('a5'))
        # move a white forward two
        self.assertTrue(self.board.move_piece('e2', 'e4'))
        self.assertIsNotNone(self.board.get_piece('e4'))

    def test_diagonal_capture(self):
        '''test if capturing normally works properly'''
        # black capture white
        self.board.set_piece('b6', Pawn('b6', 'white', self.board))
        self.assertTrue(self.board.move_piece('a7', 'b6'))
        self.assertEqual(self.board.get_piece('b6').color, 'black')
        # white capture black
        self.board.set_piece('b3', Pawn('b3', 'black', self.board))
        self.assertTrue(self.board.move_piece('a2', 'b3'))
        self.assertEqual(self.board.get_piece('b3').color, 'white')

    def test_forward_one_capture(self):
        '''test if pawn can capture forward one'''
        # test for black capturing white
        self.board.set_piece('a6', Pawn('a6', 'white', self.board))
        self.assertFalse(self.board.move_piece('a7', 'a6'))
        self.assertEqual(self.board.get_piece('a6').color, 'white')
        # test for white capturing black
        self.board.set_piece('e3', Pawn('e3', 'black', self.board))
        self.assertFalse(self.board.move_piece('e2', 'e3'))
        self.assertEqual(self.board.get_piece('e3').color, 'black')

    def test_forward_two_capture(self):
        '''test if pawn can capture forward two'''
        # test for black capturing white
        self.board.set_piece('a5', Pawn('a5', 'white', self.board))
        self.assertFalse(self.board.move_piece('a7', 'a5'))
        self.assertEqual(self.board.get_piece('a5').color, 'white')
        # test for white capturing black
        self.board.set_piece('e4', Pawn('e4', 'black', self.board))
        self.assertFalse(self.board.move_piece('e2', 'e4'))
        self.assertEqual(self.board.get_piece('e4').color, 'black')

    def test_move_forward_two_piece_skip(self):
        '''test if pawn can skip over a piece by moving two'''
        # test for black skipping white
        self.board.set_piece('a6', Pawn('a6', 'white', self.board))
        self.assertFalse(self.board.move_piece('a7', 'a5'))
        # test for black skipping white
        self.board.set_piece('e3', Pawn('e3', 'black', self.board))
        self.assertFalse(self.board.move_piece('e2', 'e4'))

    def test_move_diagonal_without_capture(self):
        '''test if a pawn can move diagonally without a piece being there'''
        # test black
        self.assertFalse(self.board.move_piece('a7', 'b6'))
        # test white
        self.assertFalse(self.board.move_piece('e2', 'd3'))


class TestPawnHasMoved(unittest.TestCase):
    '''test p.has_moved for situations'''

    def setUp(self):
        self.board = Board()
        self.board.set_piece('a7', Pawn('a7', 'black', self.board))
        self.board.set_piece('e7', Pawn('e7', 'black', self.board))
        self.board.set_piece('a2', Pawn('a2', 'white', self.board))
        self.board.set_piece('e2', Pawn('e2', 'white', self.board))

    def tearDown(self):
        del self.board

    def test_capture_has_moved(self):
        '''test if it knows it moved after capturing'''
        # test white capture black
        self.assertFalse(self.board.get_piece('e2').has_moved)
        self.board.set_piece('d3', Pawn('d3', 'black', self.board))
        self.board.move_piece('e2', 'd3')
        self.assertTrue(self.board.get_piece('d3').has_moved)
        # test black capture white
        self.assertFalse(self.board.get_piece('a7').has_moved)
        self.board.set_piece('b6', Pawn('b6', 'white', self.board))
        self.board.move_piece('a7', 'b6')
        self.assertTrue(self.board.get_piece('b6').has_moved)

    def test_one_forward_has_moved(self):
        # test black
        self.assertFalse(self.board.get_piece('a7').has_moved)
        self.board.move_piece('a7', 'a6')
        self.assertTrue(self.board.get_piece('a6').has_moved)
        # test white
        self.assertFalse(self.board.get_piece('a2').has_moved)
        self.board.move_piece('a2', 'a3')
        self.assertTrue(self.board.get_piece('a3').has_moved)

    def test_two_forward_has_moved(self):
        # test black
        self.assertFalse(self.board.get_piece('e7').has_moved)
        self.board.move_piece('e7', 'e5')
        self.assertTrue(self.board.get_piece('e5').has_moved)
        # test white
        self.assertFalse(self.board.get_piece('e2').has_moved)
        self.board.move_piece('e2', 'e4')
        self.assertTrue(self.board.get_piece('e4').has_moved)


class TestCheck(unittest.TestCase):
    def setUp(self):
        self.board = Board()

        # clear board
        for i in range(8):
            for j in range(8):
                self.board.arr[i][j] = None

        # put king
        self.board.quick_set_piece('e8', King, 'black')

    def tearDown(self):
        del self.board

    def test_pawn_check(self):
        '''tests if a pawn can put a king in check'''
        self.board.quick_set_piece('f6', Pawn, 'white')
        self.assertEqual(self.board.check_check(), [])
        self.board.move_piece('f6','f7')
        self.assertEqual(self.board.check_check(), ['black'])

    def test_rook_check(self):
        '''tests if a rook can put a king in check'''
        self.board.quick_set_piece('e1', Rook, 'white')
        self.assertEqual(self.board.check_check(), ['black'])
        self.board.move_piece('e1','d1')
        self.assertEqual(self.board.check_check(), [])

    def test_bishop_check(self):
        '''tests if a bishop can put a king in check'''
        self.board.quick_set_piece('b5', Bishop, 'white')
        self.assertEqual(self.board.check_check(), ['black'])
        self.board.move_piece('b5','c4')
        self.assertEqual(self.board.check_check(), [])

    def test_queen_check(self):
        '''tests if a queen can put a king in check'''
        self.board.quick_set_piece('b5', Queen, 'white')
        self.assertEqual(self.board.check_check(), ['black'])
        self.board.move_piece('b5','c4')
        self.assertEqual(self.board.check_check(), [])
        self.board.move_piece('c4','c8')
        self.assertEqual(self.board.check_check(), ['black'])

    def test_king_check(self):
        self.board.quick_set_piece('d7', King, 'white')
        self.assertEqual(self.board.check_check(), ['black', 'white'])
        self.board.move_piece('d7','d6')
        self.assertEqual(self.board.check_check(), [])

class TestLandOnTeammates(unittest.TestCase):
    def setUp(self):
        self.board = Board()

        # clear board
        for i in range(8):
            for j in range(8):
                self.board.arr[i][j] = None

        # put pawn
        self.board.quick_set_piece('e7', Pawn, 'black')

    def tearDown(self):
        del self.board

    def test_pawn_teammate(self):
        '''test if a pawn can land on a teammate'''
        self.board.quick_set_piece('e8', Pawn, 'black')
        self.assertFalse(self.board.move_piece('e8', 'e7'))

    def test_rook_teammate(self):
        '''test if a rook can land on a teammate'''
        self.board.quick_set_piece('e8', Rook, 'black')
        self.assertFalse(self.board.move_piece('e8', 'e7'))

    def test_bishop_teammate(self):
        '''test if a bishop can land on a teammate'''
        self.board.quick_set_piece('d8', Bishop, 'black')
        self.assertFalse(self.board.move_piece('d8', 'e7'))

    def test_queen_teammate(self):
        '''test if a queen can land on a teammate'''
        self.board.quick_set_piece('d8', Queen, 'black')
        self.assertFalse(self.board.move_piece('d8', 'e7'))

    def test_king_teammate(self):
        '''test if a king can land on a teammate'''
        self.board.quick_set_piece('d8', King, 'black')
        self.assertFalse(self.board.move_piece('d8', 'e7'))


if __name__ == '__main__':
    switch = False
    if switch:
        board = Board()
        # board.quick_set_piece('a7', King, 'black')
        # board.quick_set_piece('b6', Pawn, 'white')
        # board.set_piece('b6', Pawn('b6', 'white', board))
        # board.move_piece('a7', 'a5')
        # # board.move_piece('b7', 'b5')
        # pawn = board.get_piece('a5')
        # print(board)
        # print(pawn.has_moved)
        response = ''
        while response != 'exit':
            print(board)
            response = input('>>>')
            match = re.search('^{0} to {0}$'.format(POS_STR_REGEXP),response)
            if not match:
                if response == 'exit':
                    continue
                else:
                    print('invalid input. type something like "a1 to a2" or "exit"')
                    continue
            pos_str, dest_str = re.findall(POS_STR_REGEXP,response)
            board.move_piece(pos_str, dest_str)
    else:
        unittest.main()


'''
* white starts on top
things to check on is_move_legal:
    * is there a friendly piece there (done at Piece)
    * is the king there
    * is is it off the board (done at Piece)
    * is it a legal move for that piece type
    * is it in the right direction for that color (if applicable)
    * for rooks, bishops, and queens, is there a piece in the way if you're trying to go past it
--pawn tests:
    --* move forward once
    --* move forward twice
    --* move forward twice after first move
    --* can only capture
general tests:
    --* can't hop over pieces if you shouldn't
    --* can't land on teammates
    * can't put king in vulnerability
    * win conditions
--subclassing Piece:
    is_move_legal returns true false (and calls super)
    testing classes
    __str__
to do:
    * check and chack mate stuff
    * handle assertion error when they put in a bad position
    * win condition
    * GUI
    --* test colors_in_check
'''
