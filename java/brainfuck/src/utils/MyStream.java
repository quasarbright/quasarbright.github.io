package utils;

import java.util.ArrayList;
import java.util.List;

public class MyStream<T> {
    private final List<T> items;
    private int currentIndex;

    public MyStream(List<T> items) {
        this.items = new ArrayList<>(items);
        currentIndex = 0;
    }

    private void assertNotDone() {
        if(isDone()) {
            throw new IllegalStateException();
        }
    }

    public T peek() {
        assertNotDone();
        return items.get(currentIndex);
    }

    public void advance() {
        assertNotDone();
        currentIndex++;
    }

    public T next() {
        T ans = peek();
        advance();
        return ans;
    }

    public boolean isDone() {
        return currentIndex >= items.size();
    }
}
