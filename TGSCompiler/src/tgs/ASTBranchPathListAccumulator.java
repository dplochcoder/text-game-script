package tgs;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

// Implementing class adds items to the list.
public class ASTBranchPathListAccumulator<T>
    extends ASTBranchPathVisitor<ASTBranchPathListAccumulator.State<T>> {

  public static class State<T> {
    private boolean possiblyEmpty = true;
    private List<T> arbitraryMaxLength = new ArrayList<>();

    public static <T> State<T> identity() {
      return new State<>();
    }

    public static <T> State<T> of(T elem) {
      State<T> state = new State<>();
      state.possiblyEmpty = false;
      state.arbitraryMaxLength.add(elem);
      return state;
    }

    public State<T> copy() {
      State<T> newState = State.identity();
      newState.possiblyEmpty = this.possiblyEmpty;
      newState.arbitraryMaxLength.addAll(this.arbitraryMaxLength);
      return newState;
    }

    public boolean possiblyEmpty() {
      return possiblyEmpty;
    }

    public List<T> arbitraryMaxLength() {
      return arbitraryMaxLength;
    }

    public State<T> merge(State<T> other) {
      this.possiblyEmpty = this.possiblyEmpty & other.possiblyEmpty;
      this.arbitraryMaxLength.addAll(other.arbitraryMaxLength);
      return this;
    }
  }

  @Override
  public final State<T> identity() {
    return State.identity();
  }

  @Override
  public final State<T> reduce(State<T> value1, State<T> value2) {
    return value1.merge(value2);
  }

  @Override
  public final State<T> reduceBranches(State<T> value, List<State<T>> branches) {
    List<State<T>> allStates =
        branches.stream().map(b -> value.copy().merge(b)).collect(Collectors.toList());
    value.arbitraryMaxLength =
        allStates
            .stream()
            .max(Comparator.comparing(s -> s.arbitraryMaxLength.size()))
            .get()
            .arbitraryMaxLength;
    value.possiblyEmpty = value.possiblyEmpty && allStates.stream().anyMatch(State::possiblyEmpty);
    return value;
  }
}
