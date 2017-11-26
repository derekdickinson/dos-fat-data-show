# Pure Assembly DOS FAT example program

This is a 25 year old school project. Just posted it on the off chance that 
someone might want be able to use it as an example.

I wrote this in assembly to see if it were substantially more difficult than 
using C. It wasn't too bad. I broke most of the assembly up into modestly 
sized macros and almost created a language from the macros. The main point
is not to fear assembly. A little up front effort and fairly tricky problems
can be handled with it. That said, there is not that much reason to use 
assembly anymore. 

### Quick Assembly Trick

If you have to write assembly, there is a good chance that RAM is not working
yet. An easy way to code something on an unfamiliar processor is to:

 1. Write the code in C. 
 2. Disassemble the C into another file.
 3. Note the RAM references and allocate registers instead.
 
Since you are unlikely to need a lot of assembly, this can usually help you
get a start on the code needed to get the RAM and chip selects programmed.

