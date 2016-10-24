Authors: Callum Campbell (i2t8, 12881124), Gulipek Candan (d4c9, 53906137), & Caledonia Thomson (b9b9, 17711136)

// What is the problem? //

When designing a new building, architects must be meticulous in making sure that their designs conform to the building by-laws of their city. These by-laws are multitudinous as well as sometimes complex and situation-specific. In our proof-of-concept prolog program we aimed to build the prototype of a system that models a subset of building regulations in order to prove that Prolog can be used to create a tool that aids Architects in verifying that their designs follow the building regulations of the particular kind of building they are designing.

We chose simpler rules than those in the real BC Building By-laws for the purpose of keeping our system intuitive and easier to understand, and also because building even the basic common sense guidelines that rooms and houses must conform to takes quite a lot of rules and becomes quite complex, as we discovered.

We started with the most basic building blocks, walls, windows and doors. Treating these objects as lines, we verified that they were placed logically to form rooms. Using the wall positions we calculated the area of each room and used this, along with the objects belonging to that room, to validate whether a particular room was a valid member of the room type specified by the user (the architect). Based on the rooms belonging to a house, and the square foot area of those rooms combined we then validated whether a particular house was a valid member of the house type specified by the user.

// How it works: //

The user inputs the type of house they are building, the type of rooms belonging to the house, the walls of those rooms (each wall is a pair of points designating the two ends of the wall), the locations of doors and windows, and, if there are multiple floors, which floor each room, window or door belongs to. The program will check, based on this input, whether the house they have outlined is a passes the criteria for the type of house they specified. This includes checking that each room fulfills the criteria of the room type they specified, and that it is a valid room to begin with. It also requires checking that walls, doors and windows are defined by valid coordinates (they must be lines).

// What is the something extra? //

Objects such as walls, windows and doors are defined by two X, Y coordinates. We did this because, in a real version of the program, there would be a user interface where the user would draw out these objects. Involving coordinates instead of simply designating that a window a belonged to room y required us to build many mathematical queries in prolog, in order to find the room containing a object. These queries included finding the cross product, dot product, square of the lengths of two lines, and others.
We also added floors to the houses. Floors must contain valid rooms in order to be valid. When a multiple story house has floors, the validity check for that house recurses through the floors, and through the rooms of those floors, to check that everything is “up to code”. When checking that a door or window is a part of a room, we must check that the door or window belongs to the same floor as that room. The floor of rooms, windows and doors is defined by the user input.

// What did we learn from doing this? //

We believe that, overall, logic programming is suited to this task, but ideally would like to outsource some of the numerical computations to a different tool.

We learned that prolog is not a great tool for solving numerical equations, which our program required quite a bit of. A lot of the geometrical analysis would be handled better in a separate system. For example, we needed to compare lines to see if one line contained another but were not able to easily solve systems of equations without the use of a library. We found an alternative way to compare the lines, using dot and cross products, but it took a long time to implement something that could have been done easily in a different language. Another example of this is checking whether line segments in a room intersect with themselves. Generally sweep-line algorithms to check this use a self balancing binary tree to keep track of active points, which we deemed beyond the scope of the project. Several steps were taken in order to prepare for intersection checking, such as associated a ‘handedness’ to the end points of a line, and storing that along with the index of the line segment. An object oriented language makes those steps much simpler than prolog does. 

Aside from that, Prolog was a good tool for building up our definitions of different types of rooms and houses. Starting from the most basic building blocks of a house and working up, defining what makes each object valid at each level, worked well. Everything at the top worked smoothly, relying on the rules of the objects making up the house.

The most difficult part was deciding on how the system would be designed in the first place, the scope, and where to start.

Known problems:

* Trace becomes extremely long and console can run out of memory when tracing mathematical queries, or when there are a lot of different objects to check through.
* By inputting different user data examples into the same file at the same time, it makes the program less efficient because it needs to sort through objects that belong to different houses. In the real world, this data might be loaded in separately.

Known program limitations/unhandled edge cases:

* Rooms can have shapes where walls intersect each other. We started building the logic to handle this case, and we know that it would be possible to check for this in prolog, but decided not to complete it and spend our time elsewhere. The partial working code is in the file line_helpers.pl.
* Windows and doors can be located on top of other windows and doors.
Our program does not check the positioning of the rooms in a building: We can check if the rooms are valid rooms and if they are part of the building; yet, the placement of the rooms with respect to each other and weather their placement fits to the outline of a house are the two cases the program does not handle.
* However, we have implemented the basic tools that such feature would have used, which is the dot and cross product checks. The limitation for this feature arose from the fact that we would have to introduce many new properties, which is not very easy to manage with Prolog.

