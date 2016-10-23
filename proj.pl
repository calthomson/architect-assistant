%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% User Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO find a way to store and load user data from a second file.%%%%%%%%

prop(hallway, type, bathroom).
prop(hallway, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(hallway, room_in, myhouse).
prop(master_bedroom, room_in, myhouse).
prop(master_bedroom, type, bedroom).
prop(master_bedroom, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(win1, location, ((1,1),(2,2))).
prop(win1, type, window).
prop(door1, location, ((2,2),(1,8))).
prop(door1, type, door).
prop(myhouse, type, house).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%% Object Verification %%%%%%%

% line((X1,Y1),(X2,Y2)) is true if the endpoints (X1,Y1) and (X2,Y2) form a valid line:
% A valid line is one where X1,Y1,X2,Y2 are all numbers and (X1,Y1) and (X2,Y2) are not the same points
line((X1,Y1),(X2,Y2)) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% wall/window/door((X1,Y1),(X2,Y2)) returns true if the corners (X1,Y1) and (X2,Y2) form a valid wall:
% A valid wall, window or door is a line. These checks are seperated in case we want to add more conditions.
wall((X1,Y1),(X2,Y2)) :- line((X1,Y1),(X2,Y2)).
window(((X1,Y1),(X2,Y2))) :- line((X1,Y1),(X2,Y2)).
door(((X1,Y1),(X2,Y2))) :- line((X1,Y1),(X2,Y2)).

% room(W) returns true when W is a list of walls that forms a valid room R
% A valid room has 3 or more walls, where each wall is connected to the preceeding wall, and the
% first wall is connected to the last
room([H,I|T]):- length(T,L), L>0,connectedFirstLast(H,[I|T]), connectedrest([H,I|T]).
connectedrest([H,I|T]):- connected(H,I), connectedrest([I|T]).
connectedrest([_]).

%%%%%%% Wall Connectivity Checks %%%%%%%

% connected(W1,W2) returns true when walls W1 and W2 intersect
% Walls W1 and W2 intersect return true when the second point of W1 and the first point of
% W2 are the same and both W1 and W2 are valid walls
connected(((X1,Y1),(X2,Y2)), ((X2,Y2),(_,_))) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% connectedFirstLast(W1, W2) returns true when the first wall W1 and the last wall W2 intersect
% Base case: Walls W1 and W2 intersect when the first point of W1 intersects with the last point of W2
% and W1 and W2 are both valid walls.
connectedFirstLast(((X1,Y1),_), [((X2,Y2),(X1,Y1))]) :- number(X2), number(Y2), number(X1), number(Y1),
	wlength(((X2,Y2),(X1,Y1)), L), L \= 0.0.
% Loop: Iterate through the list of walls until we reach the last wall so that we may compare it to the first
connectedFirstLast(F, [_|T]) :- connectedFirstLast(F,T).

%%%%%%% Object Validity Checks %%%%%%%

% valid(X) gets the set of all types that the object is claimed to be a type of 
% (including subtype / supertype relations that we define )
% And then checks that the object is a valid member of each of those types.
valid(X) :- setof(T, prop(X,type, T), TL),  validListOfTypes(X,TL).
% Go through the list of supertypes and check that X is a valid member of each of these.
% TODO: Most general supertype is defined first in the file so that it is checked first.
validListOfTypes(X, [H|T]) :- valid(X, type, H), validListOfTypes(X, T).
validListOfTypes(X, [H]) :- valid(X, type, H). 

% Valid(X, type, building) finds all rooms claimed to be in the house and then checks that the rooms are valid.  
valid(X, type, building):- setof(R, prop(R, room_in, X), RL), validRooms(RL). 
valid(X, type, house) :- setof(R, prop(R, room_in, X), RL), totalArea(RL, A), A<3000, countRoomType(bathroom, RL, C), C>0 . 
valid(X, type, mansion):- setof(R, prop(R, room_in, RL), RL), totalArea(RL, A), A>3000, countRoomType(bathroom, RL, C), C>3 . 
validRooms([R|RL]) :- valid(R), validRooms(RL).
validRooms([R]):- valid(R).
valid(X, type, room) :- prop(X, walls, R), room(R), area(R,A), A>0.
valid(X, type, bedroom) :- prop(W, window_in, X), prop(D, door_in, X).
valid(X, type, bathroom).

% valid(X, type, Y) is true when X is a valid member of type Y
% W is a valid window that is located in the wall of a room R
valid(X, type, window) :- prop(W, location, L), window(L), prop(W, window_in, R).
% D is a valid door that is located in the wall of a room R
valid(X, type, door) :- prop(D, location, L), door(L), prop(D, door_in, R).

%%%%%%% Object Properties %%%%%%%

prop(X,area,A):- prop(X,walls, R), area(R,A).
% TODO: Make sure that the most general supertype is defined first in the file so that it can be checked first.

prop(X, type, room) :- prop(X,type, bedroom).
prop(X, type, building):- prop(X, type, house).

%%% Window %%%
% prop(W, window_in, R) is true when room R contains window W
prop(W, window_in, R) :- prop(R, walls, WL), prop(W, location, L), find_containing_line_from_list(L, WL).

%%% Door %%%
% prop(W, door_in, R) is true when room R contains door D
prop(W, door_in, R) :- prop(R, walls, WL), prop(D, location, L), find_containing_line_from_list(L, WL).

%%% Object Property Helpers %%%
countRoomType(_, [], 0).
countRoomType(T, [R|RL], C1):- prop(R, type, T), countRoomType(T, RL, C), C1 is C + 1. 
countRoomType(T, [R|RL], C):- \+ prop(R, type,T), countRoomType(T,RL, C).

%%%%%%% Geometry Helper Queries %%%%%%%

% get_containing_line_from_list(X, WL) is true when a line in list of lines WL contains line X
find_containing_line_from_list(X, [H|T]) :- line_contains_line(H, X).
find_containing_line_from_list(X, [H|T]) :- find_containing_line_from_list(X, T).

%line_contains_line(A, B) is true when line A contains line B
line_contains_line(((X1,Y1),(X2,Y2)), ((X3,Y3),(X4,Y4))) :-
	line_contains_point((X1,Y1),(X2,Y2),(X3,Y3)),
	line_contains_point((X1,Y1),(X2,Y2),(X4,Y4)).

% line_contains_point(L1, L2, P) is true when point P lies on the line between points L1 and L2
line_contains_point(L1, L2, P) :-
	cross_product(L1, L2, P, CP), abs(CP, A), A < 1,
	dot_product(L1, L2, P, DP), DP > -1,
	squared_length(L1, L2, SL), \+ DP > SL.

% cross_product(L1, L2, P, CP) is true when CP is the cross product of (L2-L1) and (P-L1)
cross_product((X1,Y1), (X2,Y2), (X3,Y3), CP) :- CP is (Y3-Y1)*(X2 - X1) - (X3 - X1)*(Y2 - Y1).

% dot_product(L1, L2, P, DP) is true when DP is the dot product of (L2-L1) and (P-L1)
dot_product((X1,Y1), (X2,Y2), (X3,Y3), DP) :- DP is (X3 - X1)*(X2 - X1) + (Y3 - Y1)*(Y2 - Y1).

% squared_length(L1, L2, SL) is true when SL is the squared length of points L1 and L2
squared_length((X1,Y1), (X2,Y2), SL) :- SL is (X2 - X1)*(X2 - X1) + (Y2 - Y1)*(Y2 - Y1).

% This call checks the area of a non-crossing irregular n-gon, where the last segment connects to
% the first. This can be assumed because all rooms are checked for connectedness elsewhere. 

totalArea([], 0).
totalArea([R|RL], A):- prop(R, walls, W), area(W, A1), totalArea(RL, Rest), A is A1 + Rest. 

% This call checks the area of a non-crossing irregular n-gon, where the last segment connects to the first. This can be assumed because all rooms are checked for connectedness elsewhere. 

area(R,A) :- segmentListToListOfCrossProducts(R,L), sumlist(L,S), A is abs(div(S,2)).

sumlist([], 0).
sumlist([H|T], S) :-
   sumlist(T, R),
   S is H + R.

segmentListToListOfCrossProducts([((X1,Y1),(X2,Y2))|T], [P|L]) :- P is (X1*Y2 - Y1*X2), segmentListToListOfCrossProducts(T, L).
segmentListToListOfCrossProducts([], []).

% wlength returns true when L, the length between two points, is equal to the result
% of the pythagoras theorem
wlength(((X1,Y1),(X2,Y2)), L) :- L is sqrt((X1-X2)^2 + (Y1-Y2)^2).


