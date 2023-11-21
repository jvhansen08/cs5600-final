# CS5600 Project 2

### Overview
Per my project proposal, I have defined GPS operators to plan out what courses must be taken in order to take some Computer Science course (i.e. If I want to take CS2420, I must first take CS1400 and CS1410). I found that, after doing so, I had a little extra time, so I tried to expand the scope of the project to include all undergraduate courses at USU. While it wasn't entirely successful, these results can be found in the [BONUS](./BONUS) directory. This data was manually sourced from the [USU catalog](https://catalog.usu.edu/preview_program.php?catoid=38&poid=36772).

### Running the program
I used the boilerplate code provided in assignment 10. I have assumed an initial state of the world where no classes have been taken. In order to change this, feel free to add courses (i.e. cs-1400 or cs-1410) to the \*registration-world\* list on line 48 of [ops.lisp](./ops.lisp). Once you have chosen your initial state of the world, open a linux terminal and navigate to the the current directory. Once there, start a lisp interpreter with the command 'clisp' and run the following commands:
```lisp
(load "gps.lisp")
(use *registration-ops*)
;;;The next command is optional
(trace-gps) ;;; you can run this command if you would like to see the 'thought-process' of GPS
(gps *registration-world* `(GOAL)) ;;; replace 'GOAL' with the desired course, i.e. cs-3450 or cs-5000
```
### Sample results
I have included a couple of the runs below:
#### With (trace-gps) enabled:
```
From no Classes to CS-3450
> (gps *registration-world* `(cs-3450))
Goal: CS-3450   
Consider: TAKE-CS-3450  
  Goal: ADMITTED    
  Consider: REGISTER-FOR-PROFESSIONAL-PROGRAM   
    Goal: CS-1400   
    Consider: TAKE-CS-1400  
      Goal: MATH-1050   
      Consider: TAKE-MATH-1050  
      Action: TAKE-MATH-1050    
    Action: TAKE-CS-1400    
    Goal: CS-1410   
    Consider: TAKE-CS-1410  
      Goal: CS-1400 
    Action: TAKE-CS-1410    
    Goal: CS-1440   
    Consider: TAKE-CS-1440  
      Goal: CS-1400 
    Action: TAKE-CS-1440    
    Goal: CS-2410   
    Consider: TAKE-CS-2410  
      Goal: CS-1410 
    Action: TAKE-CS-2410    
    Goal: CS-2420   
    Consider: TAKE-CS-2420  
      Goal: CS-1410 
    Action: TAKE-CS-2420    
    Goal: CS-2610   
    Consider: TAKE-CS-2610  
      Goal: CS-1410 
    Action: TAKE-CS-2610    
    Goal: MATH-1210 
    Consider: TAKE-MATH-1210    
      Goal: MATH-1050   
      Goal: MATH-1060   
      Consider: TAKE-MATH-1060  
      Action: TAKE-MATH-1060    
    Action: TAKE-MATH-1210  
    Goal: MATH-3310 
    Consider: TAKE-MATH-3310    
      Goal: MATH-1210   
    Action: TAKE-MATH-3310  
  Action: REGISTER-FOR-PROFESSIONAL-PROGRAM 
  Goal: CS-2420 
  Goal: CS-2610 
Action: TAKE-CS-3450    
((START) 
    (EXECUTE TAKE-MATH-1050)    
    (EXECUTE TAKE-CS-1400)  
    (EXECUTE TAKE-CS-1410)  
    (EXECUTE TAKE-CS-1440)  
    (EXECUTE TAKE-CS-2410)  
    (EXECUTE TAKE-CS-2420)  
    (EXECUTE TAKE-CS-2610)  
    (EXECUTE TAKE-MATH-1060)    
    (EXECUTE TAKE-MATH-1210)    
    (EXECUTE TAKE-MATH-3310)    
    (EXECUTE REGISTER-FOR-PROFESSIONAL-PROGRAM) 
    (EXECUTE TAKE-CS-3450)) 
```
#### Without (trace-gps) enabled
```
From no classes to CS5000
((START) 
(EXECUTE TAKE-MATH-1050) 
(EXECUTE TAKE-CS-1400) 
(EXECUTE TAKE-CS-1410) 
(EXECUTE TAKE-CS-1440) 
(EXECUTE TAKE-CS-2410) 
(EXECUTE TAKE-CS-2420)
(EXECUTE TAKE-CS-2610) 
(EXECUTE TAKE-MATH-1060) 
(EXECUTE TAKE-MATH-1210) 
(EXECUTE TAKE-MATH-3310) 
(EXECUTE REGISTER-FOR-PROFESSIONAL-PROGRAM)
(EXECUTE TAKE-CS-5000))
```