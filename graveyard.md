# The Graveyard

After the stack, there is a region of dropped data that remains, nonetheless,
valid.  This region is called the graveyard.

Operationally, the graveyard is defined as a stack of values.

In addition to their normal stack actions, every instruction has an additional
graveyard action. These are:

 * **drop:** the dropped value is pushed to the top of the graveyard.
 * **resurrect:** a value is popped from the graveyard, and pushed to the data
   stack.  This is UB if the graveyard is empty.
 * **swap:** the graveyard is unmodified.
 * Any other instruction *exorcises the graveyard*, clearing it to contain no
   values.
