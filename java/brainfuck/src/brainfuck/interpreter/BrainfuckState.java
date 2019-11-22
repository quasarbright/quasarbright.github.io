package brainfuck.interpreter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Represents the state of a brainfuck program (tape of cells and a pointer position)
 */
public class BrainfuckState {
    private final List<Integer> tape;
    private int position;
    private final Scanner in;
    private final Appendable output;

    public BrainfuckState(Appendable output) {
        tape = new ArrayList<>();
        tape.add(0);
        position = 0;
        in = new Scanner(System.in);
        in.useDelimiter("");
        this.output = output;
    }

    private void fillTo() {
        while(tape.size() <= position) {
            tape.add(0);
        }
    }

    /**
     * Increment the cell under the pointer.
     */
    public void increment() {
        fillTo();
        tape.set(position, tape.get(position)+1);
    }

    /**
     * Decrement the cell under the pointer.
     */
    public void decrement() {
        fillTo();
        tape.set(position, tape.get(position)-1);
    }

    /**
     * Move the pointer right by one.
     */
    public void moveRight() {
        fillTo();
        position++;
    }

    /**
     * Move the pointer left by one.
     */
    public void moveLeft() {
        fillTo();
        position--;
        if(position < 0) {
            throw new IllegalStateException("went left at beginning of tape");
        }
    }

    /**
     * Read a character from user input and store its integer value in the cell under the pointer.
     */
    public void input() {
        fillTo();
        tape.set(position, (int) in.next().charAt(0));
    }

    /**
     * Convert the cell under the pointer to a character and print it.
     */
    public void output() {
        fillTo();
        try {
            output.append((char) (int) tape.get(position));
        } catch (IOException e) {
            throw new IllegalStateException("unable to append to output");
        }
    }

    /**
     * Get the current position of the pointer.
     *
     * @return the position of the pointer
     */
    public int getPosition() {
        return position;
    }

    /**
     * Get the integer value of the cell under the pointer.
     *
     * @return the integer value of the cell under the pointer
     */
    public int getValue() {
        return tape.get(position);
    }

    /**
     * Return the tape.
     *
     * @return the tape (list of integers)
     */
    public List<Integer> getTape() {
        return new ArrayList<>(tape);
    }
}
