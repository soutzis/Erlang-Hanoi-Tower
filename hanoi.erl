-module(hanoi).
-export([create_towers/1, display_towers/1, move/4, solve/1]).

%This function will create a list with a representation of the 3 towers and the number of disks inputted, on tower1
create_towers(N) ->
  [  {tower1,lists:seq(1,N)}, {tower2, []}, {tower3, []}  ].                                        %returns a list with 3 tuples, each containing a sublist. The first sublist will list numbers sequentially from 1 to the number specified.

%This function will print that towers created by "create_towers" function, in a human friendly way
display_towers(T) ->
  io:format("----------------------------- ~n"),
  io:format("~w: ~w ~n", [element(1, lists:nth(1, T)), lists:reverse(element(2, lists:nth(1, T)))]), %io:format is a print function of erlang, which uses ~p to print an element and ~n for a new line.
  io:format("~w: ~w ~n", [element(1, lists:nth(2, T)), lists:reverse(element(2, lists:nth(2, T)))]), %lists:reverse, is a function that will take a list and return a new list with all elements in reverse order.
  io:format("~w: ~w ~n" , [element(1, lists:nth(3, T)), lists:reverse(element(2, lists:nth(3, T)))]),%~w instead of ~p, because ~w avoids interpreting data, and it would just print them as numbers. While ~p could make the program think that it's printing a string
  io:format("----------------------------- ~n").

%move/4 function, will move 1 disk of the list of towers "T", from the "Source" tower to the "Destination" tower.
move(T, Source, Destination, 1) ->
  LengthSource = length(element(2, lists:nth(Source,T))),                                           %Gives the Number of Disks of the Tower to remove a disk from
  case LengthSource =:= 0 of                                                                        %If no disks on tower, terminate function, if there are disks available, continue
    true -> io:format("~nIllegal move detected. Can't take a disk from an empty tower! ~n~n"),
    T;
    false-> case Source == Destination of                                                           %If at least 1 disk on source tower, but source tower equals to destination tower, terminate function.
      true -> io:format("~nIllegal move detected. Can't move disk to the same tower! ~n~n"),
      T;
      false->                                                                                       %If at least 1 disk on source tower and source tower is not equal to destination tower, continue

      [Head|Tail] = element(2,(lists:nth(Source,T))),                                               %Dividing the list that a disk will be removed from, into head and tail. Head is the element to be removed.
      DiskAdded = [Head] ++ element(2,lists:nth(Destination,T)),                                    %Composing a new list, by adding Head to the list that a disk will be added to, using the '++' concatenation method.

      SourceTuple = setelement(2, (lists:nth(Source, T)),Tail),                                     %Creating a new tuple, with its sublist updated, to replace the tuple of source tower in list "T"
      DestTuple = setelement(2, (lists:nth(Destination, T)), DiskAdded),                            %Creating a new tuple, with its sublist updated, to replace the tuple of destination tower in list "T"

      UnsortedList = [ SourceTuple, DestTuple, lists:nth(other(Source, Destination),T) ],           %Creating a new updated list, with the updated tuples and the tuple which was left intact, but the list is unsorted. I am using lists:keyfind/3 function, to find and place the tuples in the correct order in the new list created below.
      SortedList = [(lists:keyfind(tower1, 1, UnsortedList)),(lists:keyfind(tower2, 1, UnsortedList)),(lists:keyfind(tower3, 1, UnsortedList))],

      display_towers(SortedList),                                                                   %Displaying the updated and sorted list with the display_towers/1 fucntion
      SortedList                                                                                    %Return SortedList.
      end
    end;

%move/4 function, will move a specified number of disks(DiskNum) of the list of towers "T", from the "Source" tower to the "Destination" tower, recursively.
  move(T, Source, Destination, DiskNum) ->
    LengthSource = length(element(2, lists:nth(Source,T))),                                         %Gives the Number of Disks of the Tower to remove a disk from
    case LengthSource =:= 0 of                                                                      %If no disks on tower, terminate function, if there are disks available, continue
      true -> io:format("~nIllegal move detected. Can't take a disk from an empty tower! ~n~n"),
      T;
      false-> case Source == Destination of
        true -> io:format("~nIllegal move detected. Can't move disk to the same tower! ~n~n"),
        T;
        false->
          Step1 = move(T, Source, (other(Source, Destination)), DiskNum-1),                         %These 3 steps will recursively move the number of disks specified, from the source tower to the destination tower.
          Step2 = move(Step1, Source, Destination, 1),
          Step3 = move(Step2, other(Source, Destination), Destination, DiskNum-1),
          Step3
      end
    end.

%other/2 function will calculate which tower is the one that will have no disks when the move function finishes
  other(Source, Destination)->
    X = 6 - (Source+Destination),                                                                    %Source+Destination+X = 1+2+3, Simple math equation to find the tower that is unused at this moment
    X.

%solve/1 function will call move/4 function and will recursively move All of the disks on tower1.
  solve(T) ->
  LengthT1= length(element(2, lists:nth(1,T))),                                                      %Returns the number of disks(DiskNum) on tower 1, by calculating the lists length.
  move(T,1,3,LengthT).
