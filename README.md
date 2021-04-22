spamfilter
=========

A Haskell port of Peter Seibel's [Naive Bayesian spam
filter](http://www.gigamonkeys.com/book/practical-a-spam-filter.html),
for teaching purposes in the Haskell course at the University of
Brighton. It is used to demonstrate functional problem-solving,
writing a program with several modules, basic IO techniques and
interacting with a relational database.

Details on the techniques used can be found here:
http://en.wikipedia.org/wiki/Naive_Bayes_spam_filtering

Setting up the project
----------------------

This project makes use of a local SQLite database, so 
SQLite must be installed and in your path. If you do the 
work in the Linux labs at the university, this is already
the case. A bash script is used to Set up the database so
if you are using Windows you will need the help of something 
like [CygWin](https://www.cygwin.com/) or 
[WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

* Clone the project:
    
```bash
    $ git clone git@github.com:jimburton/spamfilter.git
```

* Build the program and set up the database:

  ```bash
    $ cd spamfilter
    $ cabal configure
    $ cabal build
	$ ./setupdb.sh
  ```

* Train the filter by downloading some collections of spam/ham and
  running the program over them. You can begin by training on the small 
  number of ham and spam examples provided with this repository:
  
  ```
  $ cabal run spamfilter -- train Ham etc/mail/ham
  $ cabal run spamfilter -- train Spam etc/mail/spam
  $ cabal run spamfilter -- classify etc/mail/ham/ham1.email 
  Up to date
  Ham: 0.32777208
  $ cabal run spamfilter -- classify etc/mail/spam/spam1.email 
  Up to date
  Spam: 0.72610325
  ```
  
  But the more you train it, the more reliable the filter will
  become. Developers of spam filters have made large collections of
  ham and spam messages available for this purpose online.  For
  example, download the tarballs from
  https://spamassassin.apache.org/publiccorpus/, and extract them in a
  convenient place. Another good one resource is
  http://www.aueb.gr/users/ion/data/enron-spam/. 
  
  Don't expect perfect results by the way! Reliable, modern spam
  filters like [SpamAssassin](http://spamassassin.apache.org/) have
  been in development since the late 1990s and use a variety of
  techniques. In particular, using a naive Bayesian approach such as
  this program does is known to generate quite a large number of false
  positives (spam mistakenly identified as ham).
  
Ways to improve the program
---------------------------

* Write some tests using Quickcheck or HUnit,

* Refactor to use applicative and monadic styles where it makes the
  code easier to read,

* Refactor the `Classify` module to use a statistics library,

* Make it more robust by adding error checking around the CLI, reading
  files etc,

* Write the score assigned to each email into the `X-Spam-Status`
  header of the file and save the modified file,
  
* Make the program capable of working as a real filter, by enabling it
  to read the contents of messages from `stdin`,
  
* Make the program self-training, so that every time it classifies an
  email, update the word counts in the database accordingly...
  
* Add some documentation and improve on the Cabal metadata.


