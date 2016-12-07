This is intended to be a framework for machine learning which includes various forms of input pre-processing, classifiers, and validators. I have been using this as a means to learn Haskell and, as such, this should not be inferred to be an example of "production" or even "good" Haskell code.

There are many data sets available at [UCI](https://archive.ics.uci.edu/ml/datasets.html), though they may not be compatible with this application.

Building:
Eventually, this will use cabal. At the moment, you'll need to install GHC and any needed libraries manually. Once GHC is properly installed, the `run` script should inform you of missing libraries, which you can install with cabal.

Running:
A short shell script is provided in the main directory that runs the main Haskell program, in which directions are provided for the user:

```
./run
```

Testing:
Similarly, there is a short shell script which will run the test suite:

```
./test
```

Contributing:
Although this is a personal project, I welcome any contributions in the way of comments, suggestions, and pull requests.

