- ProjectOne.hs
- TestOne.hs
are working* implementations evalProp.
The example queries defined in TestOne.hs can directly be executed in console.

- ProjectOne.hs
- TestOne.hs
are working* implementation of evalOne.
The example queries defined in TestOne.hs can directly be executed in console.

* = These eval functions only support simple queries as seen at their examples, not stacked ones.


- ProjectMulti.hs
- TestMulti.hs
is a NOT working implementation of evalMulti.
Unfortunatelly, due to time constraints and pesky bugs we were unable to complete this part of the assignment.
Mainly some bugs took way to much time to fix, to the point that we are already a day to late.

In the ProjectMulti.hs there is documentation of the main functions
The most important ones of these are
-evalMulti
-evalS
-unifyS
-unifier
-unifier'
-standardize
The documentation above these functions should provide some insight in our plan to make this work.

The TestMulti.hs file contains a simple version of the royal family program and some example queries.
Since the program does not work, they are rather useless, however, some functionality can still be tested using this program.
Under the program and the example queries, there are some tests showing the workings of unifications and standardizations.


===========================================
To show the programs of TestOne and TestMulti in prolog style notation, use "ppp mp" in console.
All other console output will automatically be shown in prolog style notation due to special Show Instances.
