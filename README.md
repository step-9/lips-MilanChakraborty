# Assignments

## Prerequisites

This project needs `babashka v1.3.180` or higher. You can install it by typing `brew install borkdude/brew/babashka` on a Mac.

## Instructions

1. Fork this repo
2. Clone the forked repo
3. Execute `bin/test` from the project root(the dir where you cloned it)

Doing the above will run tests and install all the test related dependencies.

### Running tests in watch mode

Clojure takes a while to start, so it is best to run your tests in watch mode. To do it, type in

```sh
bin/test --watch
```

By default, when you do this you will see a report of some pending tests. This is expected. The tests have been marked as pending.

### Marking tests as implemented/implementing.

Here is a sample test:

```clojure
(deftest ^:kaocha/pending safe-division-test
  (testing "non zero denominator"
    (is (= 2 (c/safe-divide 4 2))))
  (testing "zero denominator"
    (is (nil? (c/safe-divide 3 0)))))
```

The `^:kaocha/pending` is simply an "annotation" (metadata) that helps the test runner decide whether to run the test or not. `kaocha` is the name of the test runner and `:kaocha/pending` is simply a keyword that is qualified by the namespace `kaocha`.

In order to make this test run, mark it as `:implementing`. As follows:

```clojure
(deftest ^:implementing safe-division-test
  (testing "non zero denominator"
    (is (= 2 (c/safe-divide 4 2))))
  (testing "zero denominator"
    (is (nil? (c/safe-divide 3 0)))))
```

If you are running your tests in watch mode and you save the above, at this point you should have test failures. Go ahead and implement the function `safe-divide` until all the assertions pass. Once you are done, mark the test as `:implemented`

### Further test configuration.

Check out `tests.edn` to see how the tests are configured. For further information check out [kaocha](https://cljdoc.org/d/lambdaisland/kaocha/1.88.1376/doc/readme)

### Other annotations.

You will notice that functions are marked with additional metadata.

```clojure
{:level :easy
 :use   '[when-not zero?]}
```

This additional metadata is harmless and only present for you to know what library functions to use and how hard that function might be to implement(easy, medium etc)
