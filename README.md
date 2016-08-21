# genprompt
 A library for asking the user to fill in generic datatypes on the command line
 
 ** This project is incomplete! **


This library exports 3 typeclasses:
  ```Haskell
  class Prompt a where
    prompt :: (CommandLine m, Describe a) => m a
  ```  
  ```Haskell
  class Describe a where
    describe :: a -> T.Text
  ```  
  ```Haskell
  class Monad m => CommandLine m where
    putStrLn :: String -> m ()
    getLine :: m String
  ```
Where CommandLine comes with an IO instance.

Example:
```Haskell
data ExampleType = LeftSide Int | OtherSide Bool (Char, Text)

...

    example <- prompt
```
Would lead to:
```
Now filling a ExampleType:
Choose between LeftSide or OtherSide
l or r?
r
I need a Bool:
False
I need a (Char, Text):
('e', "Hi")
```



Current issues / TODOs:
  * Currently only works with datatypes entirely consisting of sub-types with
    both Read and Describe instances. There is a plan to allow recursively Prompt-able
    datatypes.
  * There is a lot to do to make the interaction better.
    * sumtypes > 2 are pretty hideously communicated right now.
    * sumtypes could tell you more than the Constructor name
    * in general, there could be better information about where you are in the datatype
    * print out selector names for records
