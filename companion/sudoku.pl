% A Sudoku solver.  The basic idea is for each position,
% check that it is a digit with `digit`.  Then verify that the digit
% chosen doesn't violate any constraints (row, column, and cube).
% If no constraints were violated, proceed further.  If a constraint
% was violated, then backtrack to the last digit choice and move from
% there (the Prolog engine should handle this for you automatically).
% If we reach the end of the board with this scheme, it means that
% the whole thing is solved.

digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

numBetween(Num, Lower, Upper) :-
        Num >= Lower,
        Num =< Upper.

% cubeBounds: (RowLow, RowHigh, ColLow, ColHigh, CubeNumber)
cubeBounds(0, 2, 0, 2, 0).
cubeBounds(0, 2, 3, 5, 1).
cubeBounds(0, 2, 6, 8, 2).
cubeBounds(3, 5, 0, 2, 3).
cubeBounds(3, 5, 3, 5, 4).
cubeBounds(3, 5, 6, 8, 5).
cubeBounds(6, 8, 0, 2, 6).
cubeBounds(6, 8, 3, 5, 7).
cubeBounds(6, 8, 6, 8, 8).

% Given a board and the index of a column of interest (0-indexed),
% returns the contents of the column as a list.
% columnAsList: (Board, ColumnNumber, AsRow)
columnAsList([], _, []).
columnAsList([Head|Tail], ColumnNum, [Item|Rest]) :-
        nth0(ColumnNum, Head, Item),
        columnAsList(Tail, ColumnNum, Rest).

% given which row and column we are in, gets which cube
% is relevant.  A helper ultimately for `getCube`.
% cubeNum: (RowNum, ColNum, WhichCube)
cubeNum(RowNum, ColNum, WhichCube) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, WhichCube),
        numBetween(RowNum, RowLow, RowHigh),
        numBetween(ColNum, ColLow, ColHigh).

% Drops the first N elements from a list.  A helper ultimately
% for `getCube`.
% drop: (InputList, NumToDrop, ResultList)
drop([], _, []).
drop(List, 0, List).
drop([_|Tail], Num, Rest) :-
        Num > 0,
        NewNum is Num - 1,
        drop(Tail, NewNum, Rest).

% Takes the first N elements from a list.  A helper ultimately
% for `getCube`.
% take: (InputList, NumToTake, ResultList)
take([], _, []).
take(_, 0, []).
take([Head|Tail], Num, [Head|Rest]) :-
        Num > 0,
        NewNum is Num - 1,
        take(Tail, NewNum, Rest).

% Gets a sublist of a list in the same order, inclusive.
% A helper for `getCube`.
% sublist: (List, Start, End, NewList)
sublist(List, Start, End, NewList) :-
        drop(List, Start, TempList),
        NewEnd is End - Start + 1,
        take(TempList, NewEnd, NewList).

% Given a board and cube number, gets the corresponding cube as a list.
% Cubes are 3x3 portions, numbered from the top left to the bottom right,
% starting from 0.  For example, they would be numbered like so:
%
% 0  1  2
% 3  4  5
% 6  7  8
%
% getCube: (Board, ColumnIndex, ContentsOfCube)
getCube(Board, Number, AsList) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, Number),
        sublist(Board, RowLow, RowHigh, [Row1, Row2, Row3]),
        sublist(Row1, ColLow, ColHigh, Row1Nums),
        sublist(Row2, ColLow, ColHigh, Row2Nums),
        sublist(Row3, ColLow, ColHigh, Row3Nums),
        append(Row1Nums, Row2Nums, TempRow),
        append(TempRow, Row3Nums, AsList).

% Given a board, solve it in-place.
% After calling `solve` on a board, the board should be fully
% instantiated with a satisfying Sudoku solution.
solve(Board) :- 

        %algorithm: https://github.com/rg3/sudoku/blob/master/sudoku.c

        %algorithm: https://en.wikipedia.org/wiki/Sudoku_solving_algorithms

        %reference: http://programmablelife.blogspot.com/2012/07/prolog-sudoku-solver-explained.html

        %Given a board and the index of a column of interest (0-indexed),
        %for() 
        columnAsList(Board,0,C0),
        columnAsList(Board,1,C1),
        columnAsList(Board,2,C2),
        columnAsList(Board,3,C3),
        columnAsList(Board,4,C4),
        columnAsList(Board,5,C5),
        columnAsList(Board,6,C6),
        columnAsList(Board,7,C7),
        columnAsList(Board,8,C8),

        %Given a board and cube number, gets the corresponding cube as a list.
        %divide and conquer
        %for()
        getCube(Board,0,Cub0),
        getCube(Board,1,Cub1),
        getCube(Board,2,Cub2),
        getCube(Board,3,Cub3),
        getCube(Board,4,Cub4),
        getCube(Board,5,Cub5),
        getCube(Board,6,Cub6),
        getCube(Board,7,Cub7),
        getCube(Board,8,Cub8),

        %board -> List of List 
        %--> head->first row || then rest--> tails here
        man([Cub0,Cub1,Cub2,Cub3,Cub4,Cub5,Cub6,Cub7,Cub8],[C0,C1,C2,C3,C4,C5,C6,C7,C8],Board,0).

%List operation::::;
%member/2
%append/3
%append/2
%prefix/2
%select/3
%selectchk/3
%select/4
%selectchk/4
%nextto/3
%delete/3
%nth0/3
%nth1/3
%nth0/4
%nth1/4
%last/2
%proper_length/2
%same_length/2
%reverse/2
%permutation/2
%flatten/2
%max_member/2
%min_member/2
%sum_list/2
%max_list/2
%min_list/2
%numlist/3
%is_set/1
%list_to_set/2
%intersection/3
%union/3
%subset/2
%subtract/3

%man(_,[],_,_).
%man(CubList,ColumnsList,[F|E],Index,) :-
        %processRow(CubList,F,E,0,Index,ColumnsList),
        %Index2 is Index + 1,    
        %man(CubList,ColumnsList,E,Index2).

%processRow(_, _, [], _, _, _). 
%processRow(Cubes, EachRow, [Head|Tail], Indexnew, Index, ColumnsList) :-
        %is_set(EachRow),
        %Indexnew2 is Indexnew + 1,
        %getCol(ColumnsList, Indexnew),
        %processRow(Cubes, EachRow, Tail, Indexnew2, Index, ColumnsList).


%getCol(ColumnsList, Index) :-
        %nth0(Index, ColumnsList, Col),    %True when Elem is the Index'th element of List.



%***************************************************************************
man(_,_,[],_).
man(CubeList,ColList,[Head|Tail],Offset) :-     % [1, 2, 3] = [H|T].
                                                % ANSWER: (H = 1, T = [2, 3]) ==> recursive on head
        Newoffset is Offset + 1, % recursive call to update current offset number 
        %var(Head),
        is_set(Head),
        processRow(CubeList, ColList,Head,Head,0,Offset), %process current row and then do recursion to continue 
        man(CubeList,ColList,Tail,Newoffset).

getCol(ColList, Index) :-
        nth0(Index, ColList, SingleCol), %True when Elem is the Index th element of List.Counting starts at 0.
        is_set(SingleCol).

getRow(Board, Index) :- 
        nth0(Index, Board, S),
        is_set(S).

getcubes(CubeList,Offset,Index) :-
        cubeNum(Offset,Index,CubeOffset),
        nth0(CubeOffset,CubeList,SingleCube), %True when Elem is the Index th element of List.
        is_set(SingleCube).

inspectgrid(CubeList, ColList, Offset, Index) :-
        getCol(ColList, Index),
        getcubes(CubeList, Offset, Index). 

processRow(_, _, _, [], _, _). 
processRow(CubeList, ColList, EachRow, [Head|Tail], Index, Offset) :-
        NewIndex is Index + 1,
        var(Head), %True if currently is a free variable
        digit(Head), %Char is a digit. http://www.swi-prolog.org/pldoc/man?predicate=char_type/2
        inspectgrid(CubeList, ColList, Offset, Index),  
        is_set(EachRow),
        processRow(CubeList, ColList, EachRow, Tail, NewIndex, Offset).

processRow(CubeList, ColList, EachRow, [Head|Tail], Index, Offset) :-
        NewIndex is Index + 1,
        not(var(Head)),  %not a free variable.
        is_set(EachRow),
        processRow(CubeList, ColList, EachRow, Tail, NewIndex, Offset).


%***************************************************************************











% Prints out the given board.
printBoard([]).
printBoard([Head|Tail]) :-
        write(Head), nl,
        printBoard(Tail).

test1(Board) :-
        Board = [[2, _, _, _, 8, 7, _, 5, _],
                 [_, _, _, _, 3, 4, 9, _, 2],
                 [_, _, 5, _, _, _, _, _, 8],
                 [_, 6, 4, 2, 1, _, _, 7, _],
                 [7, _, 2, _, 6, _, 1, _, 9],
                 [_, 8, _, _, 7, 3, 2, 4, _],
                 [8, _, _, _, _, _, 4, _, _],
                 [3, _, 9, 7, 4, _, _, _, _],
                 [_, 1, _, 8, 2, _, _, _, 5]],
        solve(Board),
        printBoard(Board).

test2(Board) :-
        Board = [[_, _, _, 7, 9, _, 8, _, _],
                 [_, _, _, _, _, 4, 3, _, 7],
                 [_, _, _, 3, _, _, _, 2, 9],
                 [7, _, _, _, 2, _, _, _, _],
                 [5, 1, _, _, _, _, _, 4, 8],
                 [_, _, _, _, 5, _, _, _, 1],
                 [1, 2, _, _, _, 8, _, _, _],
                 [6, _, 4, 1, _, _, _, _, _],
                 [_, _, 3, _, 6, 2, _, _, _]],
        solve(Board),
        printBoard(Board).
