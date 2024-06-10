% cengiz bilal sari
% 2021400201
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% 1- agents_distance(+Agent1, +Agent2, -Distance)
my_abs(X, X) :- X >= 0.
my_abs(X, AbsX) :- X < 0, AbsX is -X.

get_agent_position(X,Y,Agent):-   % it get the position according to its dictionary and do not backtrack if it is satisfied.
    get_dict(x, Agent, X), get_dict(y, Agent, Y),!.

agents_distance(Agent1,Agent2,Distance):-  % it finds the coordinates of agents and calculate their manhattan distance
    % get the positions with helper function provided
    get_agent_position(X1,Y1,Agent1),
    get_agent_position(X2,Y2,Agent2),
    my_abs(X1-X2,DistanceX),
    my_abs(Y1-Y2,DistanceY),
    Distance is DistanceX+ DistanceY.


% 2- number_of_agents(+State, -NumberOfAgents)

% Extract agents from state and give it to helper method
number_of_agents(State,NumberOfAgents):- State=[Agents|_],number_of_agents_helper(Agents,NumberOfAgents).

% exract pairs from dictionary and find the number of pairs with length_Dict method
number_of_agents_helper(Agents,Number):-
    dict_pairs(Agents,_,Pairs),
    length_Dict(Pairs,0,Number).
% base case for length dict is empty list and equate the length to counter.
length_Dict([],Length,Length).   

%This method find the length with recursion.In every recursive step, it gives the tail of the list
% and increment the counter by one because of the head.
length_Dict([_|Tail],Count,Length):-
    NewCount is Count+1,
    length_Dict(Tail,NewCount,Length).



% 3- value_of_farm(+State, -Value)
%extract objects and agents from state and give it to  helper method
value_of_farm(State,Value):- State=[Agents,Objects|_], value_of_farm_helper(Agents,Objects,Value).

% this method just call sum_value for objects and agents and add them.
value_of_farm_helper(Agents,Objects,TotalValue):-
    sum_value(Agents,AgentsValue),   
    sum_value(Objects,ObjectsValue),
    TotalValue is AgentsValue + ObjectsValue.

% this method separate pairs of the dictionary 
sum_value(Dictionary,Total):-
    dict_pairs(Dictionary,_,Pairs),
    sum_value_helper(Pairs,0,Total).

% this method sum the values of objects according to their type
sum_value_helper([],Total,Total).
sum_value_helper([Item|Tail],Count,Total):-
    type_of_item(Item,X),
    ( X\=wolf->
    value(X,ValueCurrentObject),
    NewCount is Count+ ValueCurrentObject,
    sum_value_helper(Tail,NewCount,Total)
    ;
    sum_value_helper(Tail,Count,Total)).
    
% this gives the type of the item    
type_of_item(_-ValueE,X):- get_dict(subtype,ValueE,X).



% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
within_bounds(X,Y):-
    width(Width),height(Height),
    X>0,
    Y>0,
    X<Width-1,
    Y<Height-1
    .
is_occupied_by(State,AgentId,X,Y):-  % this method calculates the places whether they are occupied or not according to the type.
    State=[Agents|_],
    get_agent(State,AgentId,Agent),
    get_dict(subtype,Agent,Subtype),
    ((Subtype=wolf->
            get_dict(Id, Agents, AgentSome), Id \= AgentId, Agent.x =:= X, AgentSome.y =:= Y, AgentSome.subtype=wolf);
    (Subtype \=wolf->
            get_dict(Id, Agents, AgentSome), Id \= AgentId, AgentSome.x =:= X, AgentSome.y =:=Y
    )).


% basic append method
append([],L,L).
append([Head|Tail],List2,[Head|List3]):-
        append(Tail,List2,List3).

find_food_coordinates(State,AgentId,Coordinates):-
    %find consumeable foods and their coordinates according to just type of the agent with find all method.
    get_agent(State,AgentId,Agent),
    get_dict(subtype,Agent,Subtype),
    (
        (Subtype=cow->
            FoodTypes=[grass,grain], findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_object(State,_,Object),
                                            get_dict(subtype,Object,Type),
                                            get_dict(x,Object,Xfood),
                                            get_dict(y,Object,Yfood)),Coordinates));
        (Subtype=chicken ->
            FoodTypes=[grain,corn],findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_object(State,_,Object),
                                            get_dict(subtype,Object,Type),
                                            get_dict(x,Object,Xfood),
                                            get_dict(y,Object,Yfood)),Coordinates));
        (Subtype=wolf ->
            FoodTypes=[cow,chicken],findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_agent(State,_,Agent2),
                                            get_dict(subtype,Agent2,Type),
                                            get_dict(x,Agent2,Xfood),
                                            get_dict(y,Agent2,Yfood)),Coordinates))
    ).



% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)

extract_distance(DistanceRequired-_-_,DistanceRequired).

%sorting method
bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
   
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X>Y,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X=<Y,bubble(Y,T,NT,Max).

find_nearest_agent(State,AgentId,Coordinates,NearestAgent):-
    get_agent(State,AgentId,Agent), % find the given agent
    %find all other agents and the distance
    findall(Distance-AgentId2-Coords,
            (get_agent(State,AgentId2,Agent2),
            AgentId\=AgentId2,
            Agent2= agents{children:_ ,energy_point:_,subtype:_,type:_,x:X2,y:Y2},
            agents_distance(Agent,Agent2,Distance),
            Coords=[X2,Y2]),Distances),
    % with my_maplist method extract distance from Distances     
    my_maplist(extract_distance,Distances,OnlyDistances),
    bubble_sort(OnlyDistances,SortedDistances),
    [DistanceWeNeed|_]= SortedDistances,  % the first element is distance we need
    member(DistanceWeNeed-AgentIdWeNeed-_,Distances),
    get_agent(State,AgentIdWeNeed,NearestAgent),
    % get agent and give its dictionary and coordinates
    NearestAgent=agents{children:_ ,energy_point:_,subtype:_,type:_,x:X3,y:Y3},
    Coordinates = [X3,Y3].




% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)


find_nearest_food(State,AgentId,Coordinates,FoodType,Distance):-
State=[Agents,Objects,_,_],
Agents.AgentId= agents{children:_ ,energy_point:_,subtype:AnimalType,type:_,x:X1,y:Y1}, % extract X1,Y1

find_food_coordinates(State,AgentId,FoodCoordinates),  %Food coordinates are coordinate tuples
% find all distance x-y tuples
findall(Distance1-X2-Y2,(
    member([X2,Y2],FoodCoordinates),
    my_abs(X1-X2,DistanceX),
    my_abs(Y1-Y2,DistanceY),
    Distance1 is DistanceX+ DistanceY),
    FoodCordinatesWithDistances),
% get distances and sort them then take the first one    
my_maplist(extract_distance,FoodCordinatesWithDistances,OnlyDistances),
bubble_sort(OnlyDistances,SortedDistances),
[DistanceWeNeed|_]= SortedDistances,
member(DistanceWeNeed-XNeeded-YNeeded,FoodCordinatesWithDistances),
% extract coordinates, food type and distance.
(
        (AnimalType=cow->
                get_object_from_position(XNeeded,YNeeded,Objects,_,ObjectId),
                Objects.ObjectId= object{subtype:Subtype, type:_, x:_, y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=Subtype,
                Distance= DistanceWeNeed);
        (AnimalType=chicken ->
                get_object_from_position(XNeeded,YNeeded,Objects,_,ObjectId),
                Objects.ObjectId= object{subtype:Subtype, type:_, x:_, y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=Subtype,
                Distance= DistanceWeNeed);
        (AnimalType=wolf ->
                get_agent_from_position(XNeeded,YNeeded,Agents,AgentAim),
                AgentAim=agents{children:_ ,energy_point:_,subtype:AimAgentType,type:_,x:_,y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=AimAgentType,
                Distance= DistanceWeNeed

          )
    ).


    
% maplist method to apply a method for all elements of the list
my_maplist(_,[],[]).
my_maplist(P,[X|Xs],[Y|Ys]):-
    call(P,X,Y),
    my_maplist(P,Xs,Ys).

    
    
% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State,AgentId,X,Y,ActionList,DepthLimit):-
    % main method with bfs
    State=[Agents,_,_,_],
    Agents.AgentId= agents{children:_ ,energy_point:_,subtype: Subtype,type:_,x:X1,y:Y1}, % extract X1,Y1
    can_reach1(X1,Y1,X,Y,State,[],AgentId,Subtype,[],ActionList,DepthLimit).


%this method calculates whether the agent can reach X2,Y2 or not by BFS.
can_reach1(X1,Y1,X2,Y2,State,VisitedList,AgentId,Subtype,ParentList,ActionList,DepthLimit):-
    bfs1([[X1,Y1]],X2,Y2,State,VisitedList,AgentId,Subtype,ParentList,VisitedListResult,ParentListResult),
    % after bfs reverse the visited list calculate path according to parent list and visited list.
    reverse(VisitedListResult,ReversedList),
    append([[0,0]],ParentListResult,ParentListResultLast),
    append(ReversedList,[[X2,Y2]],ReversedListLast),
    find_path(ReversedListLast,ParentListResultLast,[X2,Y2],_,[[X2,Y2]],RealPath),    % path is empty at the beginning
    my_length_method(RealPath,Length_of_path),
    % compare the length according to depth limit
    Length_of_path < DepthLimit+3,
    RealPath=[_|TailOne],
    L is Length_of_path-2,
    generate_actions_from_path(TailOne,[],L,ActionList).


reverse([], []).
reverse([Head | Tail], List):- 
    reverse(Tail, Result),  
    append(Result, [Head], List).


% this bfs method apply breadth first search 
append2(0, _, ParentList, ParentList). % Base case: when A is 0, return the original ParentList
append2(A, [[X, Y]], ParentList, Result) :-
    A > 0,                             % Ensure A is positive
    append(ParentList, [[X, Y]], NewList), % Append [X,Y] to ParentList
    A1 is A - 1,                        % Decrement A
    append2(A1, [[X, Y]], NewList, Result). % Recur with the decremented A and the updated list

% Define a predicate to find the length of a list
my_length_method([], 0).             % Base case: an empty list has length 0
my_length_method([_ | Tail], Length) :-  % Recursive case: the length is 1 + the length of the tail
    my_length_method(Tail, TailLength),
    Length is TailLength + 1.

bfs1([[X,Y]|_],X,Y,_,VisitedList,_,_,ParentList,VisitedList,ParentList):- !. % Found a path to destination for base case.
bfs1([[X,Y]|Queue],X2,Y2,State,Visited,AgentId,Subtype,ParentList,VisitedListResult,ParentListResult):-
    % basic bfs method, take first near elements, add them into queue , extract them from queue end explore them,
    % add their children to queue etc...
    findall([NX,NY],
         (
        ((Subtype=cow -> member([DX, DY], [[-1, 0], [1, 0], [0, -1], [0, 1]]));
         (Subtype=chicken -> member([DX, DY], [[-1, -1], [1, 1], [-1, +1], [+1, -1]]));
         (Subtype=wolf -> member([DX, DY], [[-1, -1], [1, 1], [-1, +1], [+1, -1],[-1, 0], [1, 0], [0, -1], [0, 1]]))),
        NX is X+DX , NY is Y + DY,
        within_bounds(NX,NY),
        \+is_occupied_by(State,AgentId,NX,NY),
        \+member([NX,NY],Visited),
        \+member([NX,NY],Queue)
        ),NextMoves),
    my_length_method(NextMoves,A),    
    append(Queue,NextMoves,NewQueue),
    append2(A,[[X,Y]],ParentList,Result),
    bfs1(NewQueue,X2,Y2,State,[[X,Y]|Visited],AgentId,Subtype,Result,VisitedListResult,ParentListResult).


% Base case: When the node is the root node (i.e., it has no parent),
% the path is just the node itself.
% Recursive case: Find the path from the given node to its parent.
find_index(Index,[Node|_],Node,Index).
find_index(Index,[_|Tail],Node,IndexOut):-
    NewIndex is Index+1,
    find_index(NewIndex,Tail,Node,IndexOut).

find_element_at_given_index(0,[Node|_],Node).
find_element_at_given_index(Index,[_|Tail],Node):-
    Index1 is Index-1,
    find_element_at_given_index(Index1,Tail,Node).


find_path(_, _,[0,0],_,Path,Path).
find_path(Visited, Parents, [X3,Y3], Parent,Path,RealPath) :-
    % Find the index of the node in the visited list
    find_index(0,Visited,[X3,Y3],Index),
    %nth0(Index, Visited, [X3,Y3]),
    % Get the parent node coordinates corresponding to the same index
    find_element_at_given_index(Index,Parents,Parent),
    %nth0(Index, Parents, Parent),
    % Recur with the parent node
    find_path(Visited, Parents
    ,Parent,Parent1,[Parent|Path] ,RealPath).


generate_actions_from_path(_,NewActionList,0,NewActionList). % if there is only one element it is ended.
generate_actions_from_path([[X1,Y1],[X2,Y2]|Rest],ActionList,CounterForAction,NewActionListLast):-

    generate_actions_helper([X1,Y1],[X2,Y2],Action),
    append(ActionList,[Action],NewActionList),
    NewCounter is CounterForAction-1,
    generate_actions_from_path([[X2,Y2]|Rest],NewActionList,NewCounter,NewActionListLast).


generate_actions_helper([X1,Y1],[X2,Y2], Action) :-
    Dx is X2 - X1,
    Dy is Y2 - Y1,
    (   Dx = 1, Dy = 1 -> Action ='move_down_right'
    ;   Dx = -1, Dy = -1 -> Action = 'move_up_left'
    ;   Dx = 1, Dy = -1 -> Action = 'move_up_right'
    ;   Dx = -1, Dy = 1 -> Action = 'move_down_left'
    ;   Dx = 0, Dy = -1 -> Action = 'move_up'
    ;   Dx = -1, Dy = 0 -> Action = 'move_left'
    ;   Dx = 1, Dy = 0 -> Action = 'move_right'
    ;   Dx = 0, Dy = 1 -> Action = 'move_down'
    ).



% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
% just use method find nearest food, then go to this coordinate which is found with move to coordinate method
move_to_nearest_food(State,AgentId,ActionList,DepthLimit):-
    find_nearest_food(State,AgentId,Coordinates,_,_),
    Coordinates= [X,Y],
    move_to_coordinate(State,AgentId,X,Y,ActionList,DepthLimit).
    

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)

consume_all(State,AgentId,NumberOfMoves,Value,NumberOfChildren,DepthLimit):-
    % do the actual job in helper method, then calculate the value of the farm and number of children and number of movement
    consume_all_helper(State,AgentId,DepthLimit,0,NumberOfMovements,LastState),
    value_of_farm(LastState,Value),
    LastState=[Agents,_,_,_],
    Agents.AgentId= agents{children:Children_number ,energy_point:_,subtype:_,type:_,x:_,y:_},% extract number_of_children
    NumberOfChildren is Children_number,
    NumberOfMoves is NumberOfMovements.


consume_all_helper(State,_,_,ActionNumber,ActionNumber,State,[]).

consume_all_helper(State,AgentId,DepthLimit,ActionNumber,NumberOfMovements,LastState):-
    % find nearest reachable food and go it, if there is no reachable food then give the result
    (
    (find_nearest_reachable_food(State,AgentId,Coordinates,FoodType,Distance,DepthLimit), 
    Coordinates= [XCoordinate,Ycoordinate],
    move_to_coordinate(State,AgentId,XCoordinate,Ycoordinate,ActionList,DepthLimit),
    make_series_of_actions(ActionList,State,AgentId,NewState),
    my_length_method(ActionList,Length_of_the_action_list),
    NewActionNumber is ActionNumber + Length_of_the_action_list,
    eat(NewState,AgentId,NewestState),
    consume_all_helper(NewestState,AgentId,DepthLimit,NewActionNumber,NumberOfMovements,LastState));
    (LastState = State,
    NumberOfMovements= ActionNumber
    )
    ).


find_reachable_food_coordinates(State,AgentId,Coordinates,DepthLimit):-
    %  find consumeable  and reachable foods according to  type of the agent and reachability of the coordinates.
    get_agent(State,AgentId,Agent),
    get_dict(subtype,Agent,Subtype),
    get_dict(x,Agent,AgentX),
    get_dict(y,Agent,AgentY),
    (
        (Subtype=cow->
            FoodTypes=[grass,grain], findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_object(State,_,Object),
                                            get_dict(subtype,Object,Type),
                                            get_dict(x,Object,Xfood),
                                            get_dict(y,Object,Yfood),
                                            move_to_coordinate(State,AgentId,Xfood,Yfood,ActionList,DepthLimit)
                                            ),Coordinates));
        (Subtype=chicken ->
            FoodTypes=[grain,corn],findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_object(State,_,Object),
                                            get_dict(subtype,Object,Type),
                                            get_dict(x,Object,Xfood),
                                            get_dict(y,Object,Yfood),
                                            move_to_coordinate(State,AgentId,Xfood,Yfood,ActionList,DepthLimit)
                                            ),Coordinates));
        (Subtype=wolf ->
            FoodTypes=[cow,chicken],findall([Xfood,Yfood],
                                            (member(Type,FoodTypes),
                                            get_agent(State,_,Agent2),
                                            get_dict(subtype,Agent2,Type),
                                            get_dict(x,Agent2,Xfood),
                                            get_dict(y,Agent2,Yfood),
                                             move_to_coordinate(State,AgentId,Xfood,Yfood,ActionList,DepthLimit)
                                            ),Coordinates))
    ).
find_nearest_reachable_food(State,AgentId,Coordinates,FoodType,Distance,DepthLimit):-
State=[Agents,Objects,_,_],
Agents.AgentId= agents{children:_ ,energy_point:_,subtype:AnimalType,type:_,x:X1,y:Y1}, % extract X1,Y1

find_reachable_food_coordinates(State,AgentId,FoodCoordinates,DepthLimit),  %Food coordinates are coordinate tuples
% find nearest one from all coordinates
findall(Distance1-X2-Y2,(
    member([X2,Y2],FoodCoordinates),
    my_abs(X1-X2,DistanceX),
    my_abs(Y1-Y2,DistanceY),
    Distance1 is DistanceX+ DistanceY),
    FoodCordinatesWithDistances),
    my_maplist(extract_distance,FoodCordinatesWithDistances,OnlyDistances),
    bubble_sort(OnlyDistances,SortedDistances),
    [DistanceWeNeed|_]= SortedDistances,
    member(DistanceWeNeed-XNeeded-YNeeded,FoodCordinatesWithDistances),
    % extract coordinates, food type and distance.
(
        (AnimalType=cow->
                get_object_from_position(XNeeded,YNeeded,Objects,_,ObjectId),
                Objects.ObjectId= object{subtype:Subtype, type:_, x:_, y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=Subtype,
                Distance= DistanceWeNeed);
        (AnimalType=chicken ->
                get_object_from_position(XNeeded,YNeeded,Objects,_,ObjectId),
                Objects.ObjectId= object{subtype:Subtype, type:_, x:_, y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=Subtype,
                Distance= DistanceWeNeed);
        (AnimalType=wolf ->
                get_agent_from_position(XNeeded,YNeeded,Agents,AgentAim),
                AgentAim=agents{children:_ ,energy_point:_,subtype:AimAgentType,type:_,x:_,y:_},
                Coordinates = [XNeeded,YNeeded],
                FoodType=AimAgentType,
                Distance= DistanceWeNeed

          )
    ).
        
