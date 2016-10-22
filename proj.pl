% line((X1,Y1),(X2,Y2)) returns true if the corners (X1,Y1) and (X2,Y2) form a valid line:
% A valid line is one where X1,Y1,X2,Y2 are all numbers and (X1,Y1) and (X2,Y2) are not the same points
line((X1,Y1),(X2,Y2)) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% wall((X1,Y1),(X2,Y2)) returns true if the corners (X1,Y1) and (X2,Y2) form a valid wall:
% A valid wall is a line.
wall((X1,Y1),(X2,Y2)) :- line((X1,Y1),(X2,Y2)).

% wlength returns true when L, the length between two points, is equal to the result
% of the pythagoras theorem
wlength(((X1,Y1),(X2,Y2)), L) :- L is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% room(W) returns true when W is a list of walls that forms a valid room R
% A valid room has 3 or more walls, where each wall is connected to the preceeding wall, and the
% first wall is connected to the last
room([H,I|T]):- length(T,L), L>0,connectedFirstLast(H,[I|T]), connectedrest([H,I|T]).
connectedrest([H,I|T]):- connected(H,I), connectedrest([I|T]).
connectedrest([_]).

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

%TODO check validity of X against a list of types
% Get TL which is a list of all T types that X is a member of (ex. master_bedroom is a type of
bedroom and a type of room). Recursively check that X is a valid member of each of its supertypes.
valid(X) :- setof(T, prop(X,type, T), TL). %, validListOfTypes(X,TL).
% Go through the list of supertypes and check that X is a valid member of each of these.
% TODO: Most general supertype is defined first in the file so that it is checked first.
validListOfTypes(X, [H|T]) :- valid(X, type, H), validListofTypes(X, T).
valid(X, type, room) :- prop(X, walls, R), room(R), area(R,A), A>0.
valid(X, type, bedroom) :- prop(X, area, A), A < 2000.
prop(X, type, room) :- prop(X,type, bedroom).

prop(X,area,A):- prop(X,walls, R), area(R,A).
% TODO: Make sure that the most general supertype is defined first in the file so that it can be checked first.
prop(X, type, room) :- prop(X,type, bedroom).
prop(X, type, superroom) :- prop(X, type, room).

%%%%%%% Window Properties %%%%%%%
% prop(W, window_in, R) is true when room R contains window W
prop(W, window_in, R) :- prop(R, walls, WL), prop(W, location, L), find_containing_line_from_list(L, WL).

% window(W) is true when W is a valid window that is located in the wall of a room
valid(W) :- prop(W, location, L), window(L), prop(W, window_in, R).
window(((X1,Y1),(X2,Y2))) :- line((X1,Y1),(X2,Y2)).

%%%%%%% User Data %%%%%%%%
%TODO find a way to store and load user data from a second file.%%%%%%%%
prop(master_bedroom, type, bedroom).
prop(master_bedroom, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
prop(win1, location, ((1,1),(2,2))).
prop(win2, location, ((0,0),(1,1))).
prop(door1, location, ((2,2),(1,8))).
% User can also supply extra information if they know which room a window or door is in
% prop(win1, window_in, master_bedroom).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
area(R,A) :- segmentListToListOfCrossProducts(R,L), sumlist(L,S), A is abs(div(S,2)).

sumlist([], 0).
sumlist([H|T], S) :-
   sumlist(T, Rest),
   S is H + Rest.

segmentListToListOfCrossProducts([((X1,Y1),(X2,Y2))|T], [P|L]) :- P is (X1*Y2 - Y1*X2), segmentListToListOfProducts(T, L).
segmentListToListOfCrossProducts([], []).


