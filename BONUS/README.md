### Overview
The only significant differences between this version and the main version are the scope of the program (this version contains information regarding every undergraduate course offered by USU) and the source of the data. While I copied the data manually for the main version, this version automatically collected information from the [USU Catalog](https://catalog.usu.edu/content.php?catoid=38&navoid=28875). The gathered data can be found in [allData.json](./allData.json), and was collected by running [crawler.py](./crawler.py). It was then assemled into GPS rules by running [dataFormatter.py](./dataFormatter.py).

This version works well for many of the courses. However, my web scraper ran into issues when courses had 'co-requisites- rather than just pre-requisites. Additionally, when there were several options to satisfy a requirement (i.e. you can take CS1410 OR ECE1410), there was inconsistent formatting on the catalog. As a result, without manually checking each of the 4,761 courses, I can't vouch for the accuracy of the results. Thus, I have included my efforts here for you to see, but as an addendum to - rather than a central part of - the final submission.

### Running the program
I used the boilerplate code provided in assignment 10. I have assumed an initial state of the world where no classes have been taken. In order to change this, feel free to add courses (i.e. acct-4500 or eled-4030) to the \*registration-world\* list on line 48 of [ops2.lisp](./ops2.lisp). Once you have chosen your initial state of the world, open a linux terminal and navigate to the the current directory. Once there, start a lisp interpreter with the command 'clisp' and run the following commands:

```lisp
(load "gps2.lisp")
(use *registration-ops*)
;;;The next command is optional
(trace-gps) ;;; you can run this command if you would like to see the 'thought-process' of GPS
(gps *registration-world* `(GOAL)) ;;; replace 'GOAL' with the desired course, i.e. cs-3450 or cs-5000
```

### Sample Results

#### Proper Behavior
```
From no classes to ACCT-4510
> (gps *registration-world* `(acct-4510))
Goal: ACCT-4510
Consider: TAKE-ACCT-4510
  Goal: ACCT-3110
  Consider: TAKE-ACCT-3110
    Goal: ACCT-2020
    Consider: TAKE-ACCT-2020
      Goal: ACCT-2010
      Consider: TAKE-ACCT-2010
      Action: TAKE-ACCT-2010
    Action: TAKE-ACCT-2020
    Goal: ACCT-2010
  Action: TAKE-ACCT-3110
Action: TAKE-ACCT-4510
((START) 
(EXECUTE TAKE-ACCT-2010) 
(EXECUTE TAKE-ACCT-2020) 
(EXECUTE TAKE-ACCT-3110) 
(EXECUTE TAKE-ACCT-4510))
```
#### Incorrect Behavior
```
From no classes to CS-2420
> (gps *registration-world* `(cs-2420))
Goal: CS-2420
Consider: TAKE-CS-2420
  Goal: ECE-1410
  Consider: TAKE-ECE-1410
    Goal: ECE-1400
    Consider: TAKE-ECE-1400
      Goal: MATH-1050
      Consider: TAKE-MATH-1050
      Action: TAKE-MATH-1050
    Action: TAKE-ECE-1400
  Action: TAKE-ECE-1410
  Goal: CS-1410
  Consider: TAKE-CS-1410
    Goal: CS-1400
    Consider: TAKE-CS-1400
      Goal: MATH-1050
    Action: TAKE-CS-1400
  Action: TAKE-CS-1410
Action: TAKE-CS-2420
((START) 
(EXECUTE TAKE-MATH-1050) 
(EXECUTE TAKE-ECE-1400) 
(EXECUTE TAKE-ECE-1410) 
(EXECUTE TAKE-CS-1400) 
(EXECUTE TAKE-CS-1410) 
(EXECUTE TAKE-CS-2420))
```