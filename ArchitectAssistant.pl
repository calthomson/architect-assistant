%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% User Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Valid 1 Story House: h1 %%%
prop(h1, type, house).
single_story(h1).
prop(master_bedroom_h1, room_in, h1).
prop(master_bedroom_h1, type, bedroom).
prop(master_bedroom_h1, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(bathroom_h1, room_in, h1).
prop(bathroom_h1, type, bathroom).
prop(bathroom_h1, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(win1_h1, in_house, h1).
prop(win1_h1, type, window).
prop(win1_h1, location, ((1,1),(2,2))).
prop(door1_h1, in_house, h1).
prop(door1_h1, type, door).
prop(door1_h1, location, ((2,2),(1,8))).
prop(door2_h1, in_house, h1).
prop(door2_h1, type, door).
prop(door2_h1, location, ((7,4),(7,5))).

%%% Valid 3 Story House: h2 %%%
prop(h2, type, house).
prop(f1, floor_in, h2).
prop(f2, floor_in, h2).
prop(f3, floor_in, h2).
prop(living_room_h2, room_in, h2).
prop(living_room_h2, type, room).
prop(living_room_h2, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(living_room, on_floor, f1).
prop(bathroom_h2, room_in, h2).
prop(bathroom_h2, type, bathroom).
prop(bathroom_h2, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(bathroom_h2, on_floor, f2).
prop(bedroom_h2, room_in, h1).
prop(bedroom_h2, type, bedroom).
prop(bedroom_h2, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(bedroom_h2, on_floor, f3).
prop(win1_h2, in_house, h2).
prop(win1_h2, on_floor, f3).
prop(win1_h2, type, window).
prop(win1_h2, location, ((1,1),(2,2))).
prop(door1_h2, in_house, h2).
prop(door1_h2, on_floor, f3).
prop(door1_h2, type, door).
prop(door1_h2, location, ((2,2),(1,8))).
prop(door2_h2, in_house, h2).
prop(door2_h2, on_floor, f2).
prop(door2_h2, type, door).
prop(door2_h2, location, ((7,4),(7,5))).

%%% Invalid House: inv_h1 %%%
%%% Bedroom has no door %%%
prop(inv_h1, type, house).
prop(bedroom_inv_h1, room_in, inv_h1).
prop(bedroom_inv_h1, type, bedroom).
prop(bedroom_inv_h1, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(bathroom_inv_h1, room_in, inv_h1).
prop(bathroom_inv_h1, type, bathroom).
prop(bathroom_inv_h1, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(win1_inv_h1, in_house, inv_h1).
prop(win1_inv_h1, location, ((1,1),(2,2))).
prop(win1_inv_h1, type, window).
prop(door1_inv, in_house, inv_h1).
prop(door1_inv, type, door).
prop(door1_inv, location, ((7,5),(7,6))).

%%% Valid Mansion: m1 %%%
prop(m1, type, mansion).
single_story(m1).
prop(bedroom1_m1, room_in, m1).
prop(bedroom1_m1, type, bedroom).
prop(bedroom1_m1, walls, [((1,1),(2,2)),((2,2),(1000,80)), ((1000,80),(7,4)), ((7,4),(1,1))]).
prop(bathroom1_m1, room_in, m1).
prop(bathroom1_m1, type, bathroom).
prop(bathroom1_m1, walls, [((1,8), (7,4)), ((7,4),(7,5000)), ((7,5000),(1,9)), ((1,9),(1,8))]).
prop(bathroom2_m1, room_in, m1).
prop(bathroom2_m1, type, bathroom).
prop(bathroom2_m1, walls, [((1,8), (7,4)), ((7,4),(7,5000)), ((7,5000),(1,9)), ((1,9),(1,8))]).
prop(bathroom3_m1, room_in, m1).
prop(bathroom3_m1, type, bathroom).
prop(bathroom3_m1, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(bathroom4_m1, room_in, m1).
prop(bathroom4_m1, type, bathroom).
prop(bathroom4_m1, walls, [((1,8), (7,4)), ((7,4),(7,5)), ((7,5),(1,9)), ((1,9),(1,8))]).
prop(win1_m1, in_house, m1).
prop(win1_m1, type, window).
prop(win1_m1, location, ((1,1),(2,2))).
prop(door1_m1, in_house, m1).
prop(door1_m1, type, door).
prop(door1_m1, location, ((2,2),(1,8))).
prop(door2_m1, in_house, m1).
prop(door2_m1, type, door).
prop(door2_m1, location, ((1,9),(1,8))).
prop(door3_m1, in_house, m1).
prop(door3_m1, type, door).
prop(door3_m1, location, ((7,4),(7,5))).
prop(door4_m1, in_house, m1).
prop(door4_m1, type, door).
prop(door4_m1, location, ((7,5),(1,9))).

%%% Invalid Mansion: inv_m1 %%%
%%% Less than 4 bathrooms %%%
prop(m1, type, mansion).
single_story(m1).
prop(bedroom1_m1, room_in, m1).
prop(bedroom1_m1, type, bedroom).
prop(bedroom1_m1, walls, [((1,1),(2,2)),((2,2),(1000,80)), ((1000,80),(7,4)), ((7,4),(1,1))]).
prop(bathroom1_m1, room_in, m1).
prop(bathroom1_m1, type, bathroom).
prop(bathroom1_m1, walls, [((1,8), (7,4)), ((7,4),(7,5000)), ((7,5000),(1,9)), ((1,9),(1,8))]).
prop(bathroom2_m1, room_in, m1).
prop(bathroom2_m1, type, bathroom).
prop(win1_inv_m1, in_house, m1).
prop(win1_inv_m1, type, window).
prop(win1_inv_m1, location, ((1,1),(2,2))).
prop(door1_inv_m1, in_house, m1).
prop(door1_inv_m1, type, door).
prop(door1_inv_m1, location, ((2,2),(1,8))).
prop(door2_inv_m1, in_house, m1).
prop(door2_inv_m1, type, door).
prop(door2_inv_m1, location, ((1,9),(1,8))).

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

% validListOfTypes(X, [H|T]) goes through the list of supertypes and checks that 
% X is a valid member of each of these.
% Most general supertype is defined first in the file so that it is checked first.
validListOfTypes(X, [H|T]) :- valid(X, type, H), validListOfTypes(X, T).
validListOfTypes(X, [H]) :- valid(X, type, H). 

% valid(X, type, building) finds all rooms in the house and checks that these rooms are valid.
valid(X, type, building):- setof(F, prop(F, floor_in, X), FL), validFloors(FL).   
valid(X, type, building):- setof(R, prop(R, room_in, X), RL), validRooms(RL). 
valid(X, type, house) :- setof(R, prop(R, room_in, X), RL), totalArea(RL, A), A<3000, countRoomType(bathroom, RL, C), C>0 . 
valid(X, type, mansion):- setof(R, prop(R, room_in, X), RL), totalArea(RL, A), A>3000, countRoomType(bathroom, RL, C), C>3 . 

% valid(X, type, Y) returns true when X is a valid member of type Y
valid(X, type, room) :- prop(X, walls, R), room(R), area(R,A), A>0.
valid(X, type, bedroom) :- prop(W, window_in, X), prop(D, door_in, X).
valid(X, type, bathroom) :- prop(D, door_in, X).
valid(X, type, window) :- prop(W, location, L), window(L), prop(W, window_in, R).
valid(X, type, door) :- prop(D, location, L), door(L), prop(D, door_in, R).
valid(X, type, floor) :- setof(R, prop(R, room_in, X), RL), validRooms(RL).

% validFloors([F|FL]) goes through the list of floors [F|FL] and checks that each floor is valid
validFloors([F|FL]) :- valid(F), validFloors(FL).
validFloors([F]):- valid(F).

% validRooms([R|RL]) goes through the list of rooms [R|RL] and checks that each room is valid
validRooms([R|RL]) :- valid(R), validRooms(RL).
validRooms([R]):- valid(R).

%%%%%%% Object Properties %%%%%%%

prop(X,area,A):- prop(X,walls, R), area(R,A).

prop(X, type, room) :- prop(X,type, bedroom).
prop(X, type, building):- prop(X, type, house).

%%% Door & Window In Property %%%
% prop(W, door_in, R) and prop(D, window_in, R) are true when room R contains door D or window W
% The door and window must be on the same floor as room R and in the same houes as room R
prop(D, door_in, R) :-
	prop(D, type, door),
	get_containing_room(D, R).

prop(W, window_in, R) :-
	prop(W, type, window),
	get_containing_room(W, R).

%%% Object Property Helpers %%%

get_containing_room(O, R) :- 
	prop(R, walls, WL),
	prop(O, location, L),
	find_containing_line_from_list(L, WL),
	prop(R, room_in, H),
	prop(O, in_house, H),
	check_floor(R, O).

% check_floor(R, O) returns true when object O is on the same floor as room R
check_floor(R, W) :- prop(R, room_in, H), single_story(H).
check_floor(R, W) :- prop(R, on_floor, F), prop(W, on_floor, F).

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


