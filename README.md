spamfilter
=========

A Haskell port of Peter Seibel's [Naive Bayesian spam filter](http://www.gigamonkeys.com/book/practical-a-spam-filter.html), for teaching purposes in the Haskell course at the University of Brighton. It is used to demonstrate functional problem-solving, writing a program with several modules, basic IO techniques and interacting with a relational database.

Details on the techniques used (and their limitations) can be found here: (http://en.wikipedia.org/wiki/Naive_Bayes_spam_filtering)

Setting up the project
----------------------

* Clone the project:
    $ git clone git@github.com:jimburton/spamfilter.git
* Build the program:
```
    $ cd spamfilter
    $ cabal install
```
* Run the script `setupdb.sh`

* Train the filter by downloading some collections of spam/ham and running the program over them. For example, download the tarballs from https://spamassassin.apache.org/publiccorpus/, and extract them in a convenient place. Then, if you have extracted a tarball of ham messages into a folder called HAM_FOLDER, and a tarball of spam message into a folder called SPAM_FOLDER:
```    
    $ spamfilter train Ham HAM_FOLDER/
    $ spamfilter train Spam SPAM_FOLDER
```  
  The more you train it, the more reliable it will become. Don't expect perfect results by the way! More reliable spam filters like SpamAssasin have been in development since the late 1990s and use a variety of techniques. In particular, using a naive Bayesian approach such as this program does is known to generate quite a large number of false positives (spam mistakenly identified as ham).
* Once you have trained the program on several collections of good and bad emails, test its ability to classify messages correctly. There are several messages you can experiment with in etc/mail:
```   
    $ spamfilter classify etc/mail/ham1.email
```
Note that you can modify the `testAndTrain` function in the `Main` module to make initial training easier to do -- supply the paths to your own collections of spam/ham and your test messages.

Ways to improve the program
---------------------------

* Write some tests using Quickcheck or HUnit,
* Refactor to use applicative and monadic styles where it makes the code easier to read,
* Make it more robust by adding error checking around the CLI, reading files etc.,
* Write the score assigned to each email into the `X-Spam-Status` header of the file and save the modified file,
* Make the program capable of working as a real filter, by enabling it to read the contents of messages from `stdin`,
* Make the program self-training, so that every time it classifies an email, update the word counts in the database accordingly... 
* Add some documentation and improve on the Cabal metadata.


