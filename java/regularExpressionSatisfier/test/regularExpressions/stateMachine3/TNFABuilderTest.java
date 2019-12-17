package regularExpressions.stateMachine3;

import org.junit.Test;

public class TNFABuilderTest extends TNFABuilderTestUtilities {
  @Test
  public void testChar() {
    actualMachine = character('a');
    testBuilder(actualMachine,
            set(1, 2), 1, 2, set(transition(1, 2, 'a')));
  }

  @Test
  public void testEmpty() {
    actualMachine = empty();
    testBuilder(actualMachine,
            set(1), 1, 1, set());
  }

  @Test
  public void testStarSimple() {
    actualMachine = star(character('a'));
    // makes a using 1 and 2, then stars it using 3 and 4
    // in a diagram it would be 3 1 2 4
    testBuilder(actualMachine,
            set(3, 1, 2, 4), 3, 4,
            set(
                    transition(1, 2,'a'),// original machine
                    transition(3, 1),// new start to old start
                    transition(2, 4),// old end to new end
                    transition(3, 4),//skip option
                    transition(2, 1)));// repeat
  }

  @Test
  public void testStarComplex() {
    actualMachine = star(or(character('a'), character('b')));
    // a uses 1 2
    // b uses 3 4
    // or uses 5 6
    // star uses 7 8
    /*
         1 2
    7  5     6  8
         3 4
     */
    testBuilder(actualMachine,
            set(7,5,1,2,3,4,6,8), 7, 8,
            set(
                    transition(7,5),// star to or
                    // <or>
                    transition(5,1),// or to a
                    transition(5, 3),// or to b
                    transition(1,2,'a'),// a
                    transition(3,4,'b'),// b
                    transition(2,6),// a to or
                    transition(4,6),// b to or
                    // </or>
                    transition(6,8),// or to star
                    transition(7,8),// skip or
                    transition(6,5)// repeat or
            ));
  }

  @Test
  public void testOrSimple() {
    actualMachine = or(character('a'), character('b'));
    // a uses 1 2
    // b uses 3 4
    // or uses 5 6
    /*
      1 2
    5     6
      3 4
     */
    testBuilder(actualMachine,
            set(1,2,3,4,5,6), 5, 6,
            set(
                    transition(5,1),// start to a machine
                    transition(5,3),// start to b machine
                    transition(1,2,'a'),// a machine
                    transition(3,4,'b'),// b machine
                    transition(2,6),// a machine to end
                    transition(4,6)// b machine to end
            ));
  }

  @Test
  public void testConcatenateSimple() {
    actualMachine = cat(character('a'), character('b'));
    // a uses 1 2
    // b uses 3 4
    // cat makes a 2 -> 3 empty transition
    // diagram looks like 1 2 3 4
    testBuilder(actualMachine,
            set(1,2,3,4), 1 ,4,
            set(
                    transition(1,2,'a'),// a machine
                    transition(3,4,'b'),// b machine
                    transition(2,3)// link between machines
            ));
  }

  @Test
  public void testComplex() {
    // (a*b|c|d)*e*
    actualMachine = cat(star(or(cat(star(character('a')), character('b')), character('c'), character('d'))), star(character('e')));

    testBuilder(actualMachine,
            set(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), 13, 18,
            set(
                    transition(13, 11),
                    transition(13, 14),
                    transition(11, 3),
                    transition(11, 7),
                    transition(11, 9),
                    transition(14, 17),
                    transition(3,1),
                    transition(3,4),
                    transition(7,8, 'c'),
                    transition(9,10,'d'),
                    transition(17,15),
                    transition(17,18),
                    transition(1,2,'a'),
                    transition(4,5),
                    transition(8,12),
                    transition(10,12),
                    transition(15,16,'e'),
                    transition(2,4),
                    transition(5,6,'b'),
                    transition(12,14),
                    transition(16,18),
                    transition(6,12),
                    // I missed these :(
                    // they're the repeat transitions of the stars
                    transition(2,1),
                    transition(12,11),
                    transition(16,15)
            ));
  }
}