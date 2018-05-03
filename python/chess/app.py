from graphics import *
import chess
def main():
    width = 500
    height = 500
    win = GraphWin('window', width, height)
    win.setBackground('black')

    w = width / 8
    h = height / 8
    board = chess.Board()

    def toPosStr(clickPoint):
        ''' Point(x, y) -> "a1"'''
        r = int(clickPoint.y // h)
        c = int(clickPoint.x // w)
        pos_tup = (r, c)
        pos_str = chess.Board.encode_pos(pos_tup)
        return pos_str

    def toPoint(pos_str):
        '''"a1" -> Point(x, y)
        returns the upper-left coordinate'''
        r, c = chess.Board.decode_pos(pos_str)
        return Point(c*w, r*h)

    def drawBoard():
        for y in range(8):
            for x in range(8):
                rect = Rectangle(Point(x*w, y*h), Point((x+1)*w, (y+1)*h))
                if (y + x) % 2 == 0:
                    rect.setFill('burlywood')
                else:
                    rect.setFill('saddle brown')
                rect.setOutline(None)
                rect.draw(win)

                pos_tup = (y, x)
                pos_str = chess.Board.encode_pos(pos_tup)
                pos_piece = board.get_piece(pos_str)
                # pos_text = Text(Point(x*w + w/2, y*h + h/2), str(pos_piece).lower())
                if pos_piece is not None:
                    # pos_text.setTextColor(pos_piece.color)
                    # pos_text.setStyle('bold')
                    # pos_text.draw(win)
                    classNameDict = {
                        chess.Pawn:'pawn',
                        chess.Bishop:'bishop',
                        chess.King:'king',
                        chess.Knight:'knight',
                        chess.Queen:'queen',
                        chess.Rook:'rook'
                        }
                    filename = 'images/{0} {1}.png'.format(pos_piece.color, classNameDict[type(pos_piece)])
                    img = Image(Point(x*w + w/2, y*h + h/2), filename)
                    img.draw(win)

    def highlight(pos_str):
        '''puts a green square outline on a space'''
        p1 = toPoint(pos_str)
        p2 = Point(p1.x+w, p1.y+h)
        rect = Rectangle(p1, p2)
        rect.setFill(None)
        rect.setOutline(color_rgb(0,255,0))
        rect.draw(win)

    def isSelectable(pos_str):
        return pos_piece is not None and pos_piece.color == board.turn

    while True:
        drawBoard()
        if board.maybe_get_winner():
            rect = Rectangle(Point(0, width*.3), Point(width, height*.7))
            rect.setFill('black')
            rect.draw(win)
            text = Text(Point(width/2, height/2), 'GAME OVER. {0} WINS!'.format(board.maybe_get_winner().upper()))
            text.setTextColor('white')
            text.setSize(24)
            text.draw(win)
            break
        # first click to select piece
        clickPoint = win.getMouse()
        pos_str = toPosStr(clickPoint)
        # validate first click
        pos_piece = board.get_piece(pos_str)
        if not isSelectable(pos_str):
            # if you shouldn't click on that piece
            continue
        #highlight legal pieces
        legal_pos_strs = board.get_in_game_possible_moves(pos_str)
        for pos_str_ in legal_pos_strs:
            # trailing underscore is necessary so as not to overwrite pos_str
            highlight(pos_str_)
        # second click to place piece
        clickPoint = win.getMouse()
        r = int(clickPoint.y // h)
        c = int(clickPoint.x // w)
        dest_tup = (r, c)
        dest_str = chess.Board.encode_pos(dest_tup)
        # move the piece
        board.in_game_move_piece(pos_str, dest_str)

    # img = Image(Point(100,100), 'images/black bishop.png')
    # img.draw(win)
    win.getMouse()
    win.close()


main()
'''
TODO
# -let user change mind after clicking piece
#     -maybe in second click, if it clicked a friendly piece,
#     restart with that as first click using checkMouse
# -highlight legal destinations
-move isSelectable to chess.py
'''
