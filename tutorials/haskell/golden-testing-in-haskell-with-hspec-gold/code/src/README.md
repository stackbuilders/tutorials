# Golden testing tutorial

This repository is a guide on how to implement the [hspec-golden](https://github.com/stackbuilders/hspec-golden) library to create Golden tests (or Golden Master Tests) using hspec.

# What are golden tests?
Golden testing is pretty similar to unit testing, except that the test output is stored in a separate file known as the "Golden File". 

For an in depth explanation refer to this [link](https://www.stackbuilders.com/tutorial/golden-testing-in-haskell-with-hspec-gold).

# How to test this project:
* **Step 1:** **Download** or **Clone** this repo by console:

``` 
$ git clone git@github.com:MedAdrian/hspec-golden-tutorial.git
```

* **Step 2:** Change directories to your project:

``` 
$ cd path/to/hspec-golden-tutorial
```

* **Step 3:** Once inside the directory run the following commands on the console:

  * Install the dependencies:
    ``` 
    $ stack install
    ```
  * Run the test suite:
    ```
    $ stack test
    ```

* **Step 4:** (May be needed if the lib is still not on stackage)

  * On your stack.yml file you'll need to add the folowing extra dependencies:

  ```
  extra-deps:
  - hspec-golden-0.1.0.0
  - simple-get-opt-0.3
  ```
  * Once done, run the test command:
  ```
  $ stack test
  ```

  Please do give feedback if needed!