%% xo_game
%  игра крестики-нолики
%
:- module( xo, [
                xo_get_params/6,   % GetParams
                xo_get_params/7,
                xo_set_params/6,   % SetParams
                xo_set_params/7,
                xo_params/1,       % Params
                xo_init/0,         % NewGame
                xo_cell/2,         % Cell
                xo_solve/2,        % Solve
                xo_step/4,         % Step
                xo_step_back/4,    % Back
                xo_win/4,          % Win
                xo_tie/1,          % Tie
                xo_play/4,         % Play
                xo_mark_cell/3,    % MarkCell
                xo_back/0,         % Back
                xo_back/4,
                xo_forth/0,        % Forth
                xo_forth/4,
                xo_rate/6,         % Rate
                xo_review/7,       % Review
                xo_rule/4,         % Rule
                xo_test/1          % Test
               ] ).

:- dynamic([xo_params/1, xo_rule/2]).
:- dynamic([xo_cell_id/2, xo_cell_state/4]).
:- dynamic([xo_solve_cells/3, xo_solve_state/7, xo_cell_solves/3, xo_solve_cached/8]).
:- dynamic([xo_step/4, xo_step_back/4 ]).
:- dynamic([xo_cell_state_sim/4, xo_solve_state_sim/7]).

% параметры игры
% xo_params([Size, Line, Level, Go, ModeOpt])
%   Size = size(PosBegin, PosEnd) - размер игрового поля
%   Line = line(WinLength) - длина линии выигрыша
%   Level = level(Level) - уровень игры
%   Go = go(CompMark, UserMark)CompMark, UserMark) - отметки хода
%   ModeOpt - опции режима
xo_params( [
    size(0, 19),
    line(5),
    max_solve_qty(20),
    level(9),
    go(x, o),
    mode_opt([
              level(echo, -1),
              rules(normal, [
                             tie_by_chance,
                             random_best_pos,
                             random_best_chance,
                             next_step_win,
                             free_border,
                             dash_mark,
                             fork,
                             random_chance,
                             random_free_cell,
                             -
                            ]),
              rules(echo, [
                             tie_by_chance,
                             random_best_pos,
                             random_best_chance,
                             next_step_win,
                             -free_border,
                             -dash_mark,
                             fork,
                             random_chance,
                             random_free_cell,
                             -
                            ]),
              -
             ])
] ).


% пространство ячеек
% xo_cell_id(ID, X-Y)
% xo_cell_state(ID, Mark, Ver, TimeStamp)
% xo_cell(Coor, Mark)
%   Coor = X-Y
%   Mark = {x ; o ; n}
xo_cell(X-Y, Mark) :-
    xo_get_cell(_ID, X-Y, Mark).

xo_get_cell(ID, X-Y, Mark) :-
    xo_get_cell(ID, X-Y, Mark, _Ver, _TimeStamp).
xo_get_cell(ID, X-Y, Mark, Ver) :-
    xo_get_cell(ID, X-Y, Mark, Ver, _TimeStamp).
xo_get_cell(ID, X-Y, Mark, Ver, TimeStamp) :-
    ( ground(ID) -> true ; xo_cell_id(ID, X-Y) ),
    xo_cell_state_(ID, StateMark, Ver, TimeStamp),
    Mark = StateMark,
    true.

xo_cell_state_(ID, StateMark, Ver, TimeStamp):-
    xo_cell_state_sim(ID, StateMark, Ver, TimeStamp),
    !.
xo_cell_state_(ID, StateMark, Ver, TimeStamp):-
    xo_cell_state(ID, StateMark, Ver, TimeStamp),
    !.

xo_set_cell(ID, Coor, Mark) :-
    xo_set_cell(ID, Coor, Mark, 0).
% xo_set_cell(ID, X-Y, Mark, Sim)
xo_set_cell(ID, X-Y, Mark, 0) :-
    ( ground(ID) -> true ; xo_cell_id(ID, X-Y) ),
    xo_cell_state(ID, _, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_cell_state(ID, Mark, Ver1, TimeStamp) ),
    !.
xo_set_cell(ID, _, Mark, 1) :-
    ground(ID), xo_cell_id(ID, _),
    xo_cell_state_(ID, _, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_cell_state_sim(ID, Mark, Ver1, TimeStamp) ),
    !.

% пространство решений
% xo_solve(Solve, State/HasChanceMark)
%   Solve = [ cell(Coor, Mark) | _ ]
%   Coor = X-Y
%   Mark = {x ; o ; n}
%   State = [x-Qty1, o-Qty2, n-Qty3]
%   HasChanceMark
% xo_solve_state(ID, HasChanceMark, X, O, N, Ver, TimeStamp)
xo_solve(Solve, State) :-
    xo_solve(_Solve_ID, Solve, State).
xo_solve(ID, Solve, State) :-
    ground(ID),
    \+ xo_solve_state_sim(_, _, _, _, _, _, _),
    once( xo_solve_cached(ID, Solve, HasChanceMark, X, O, N, _Ver, _TimeStamp) ),
    once( xo_solve_state(ID, HasChanceMark1, X1, O1, N1, _, _) ),
    [HasChanceMark, X, O, N] = [HasChanceMark1, X1, O1, N1],
    State = [x-X, o-O, n-N] / HasChanceMark,
    !.
xo_solve(ID, Solve, State) :-
    ( ground(ID),
      xo_solve_cells(ID, SolveCells, _)
     -> true
    ; xo_solve_cells(ID, SolveCells, _) ),
    xo_solve_cells_state(SolveCells, Solve),
    xo_get_solve_state(ID, State),
    ( ground(ID),
      \+ xo_solve_state_sim(_, _, _, _, _, _, _),
      State = [x-X, o-O, n-N] / HasChanceMark,
      ( once( xo_solve_cached(ID, _, _, _, _, _, Ver, _) ), succ(Ver, Ver1) -> true ; Ver1 = 1),
      get_time(TimeStamp),
      asserta( xo_solve_cached(ID, Solve, HasChanceMark, X, O, N, Ver1, TimeStamp) )
     -> true
    ; true ),
    true.

xo_solve_cells_state([], []).
xo_solve_cells_state([cell(Coor, ID) | SolveCells], [cell(Coor, Mark) | SolveRest]) :-
    xo_get_cell(ID, Coor, Mark),
    !,
    xo_solve_cells_state(SolveCells, SolveRest).

xo_get_solve_state(ID, State) :-
    xo_get_solve_state(ID, State, _Ver, _TimeStamp).
xo_get_solve_state(ID, State, Ver) :-
    xo_get_solve_state(ID, State, Ver, _TimeStamp).
xo_get_solve_state(ID, State, Ver, TimeStamp) :-
    xo_solve_state_(ID, HasChanceMark, X, O, N, Ver, TimeStamp),
    State = [x-X1, o-O1, n-N1] / HasChanceMark1,
    [HasChanceMark, X, O, N] = [HasChanceMark1, X1, O1, N1],
    !.

xo_solve_state_(ID, HasChanceMark, X, O, N, Ver, TimeStamp):-
    xo_solve_state_sim(ID, HasChanceMark, X, O, N, Ver, TimeStamp),
    !.
xo_solve_state_(ID, HasChanceMark, X, O, N, Ver, TimeStamp):-
    xo_solve_state(ID, HasChanceMark, X, O, N, Ver, TimeStamp),
    !.

xo_set_solve_state(ID, State) :-
    xo_set_solve_state(ID, State, 0).
% xo_set_solve_state(ID, State, Sim)
xo_set_solve_state(ID, State, 0) :-
    State = [x-X, o-O, n-N] / HasChanceMark,
    xo_solve_state(ID, _HasChanceMark, _X, _O, _N, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_solve_state(ID, HasChanceMark, X, O, N, Ver1, TimeStamp) ),
    !.
xo_set_solve_state(ID, State, 1) :-
    ground(ID),
    State = [x-X, o-O, n-N] / HasChanceMark,
    xo_solve_state_(ID, _HasChanceMark, _X, _O, _N, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_solve_state_sim(ID, HasChanceMark, X, O, N, Ver1, TimeStamp) ),
    !.

% формирование пространства ячеек
% xo_make_cells
xo_make_cells :-
    xo_params(Params),
    memberchk(size(PosBegin, PosEnd), Params),
    xo_gen_cells(PosBegin, PosEnd).

xo_gen_cells(PosBegin, PosEnd) :-
    Ps = [xo_cell_id/2, xo_cell_state/4, xo_cell_state_sim/4],
    dynamic(Ps),
    forall( member(P, Ps), abolish(P) ),
    dynamic(Ps),
    %
    between(PosBegin, PosEnd, X),
    between(PosBegin, PosEnd, Y),
    ( xo_cell_id(ID, _), succ(ID, ID1)  -> true ; ID1 = 1),
    asserta( xo_cell_id(ID1, X-Y) ),
    get_time(TimeStamp),
    asserta( xo_cell_state(ID1, n, 0, TimeStamp) ),
    fail.
xo_gen_cells(_, _) :-
    %once( xo_cell(_, _) ),
    %Ps = [xo_cell_id/2],
    %compile_predicates(Ps),
    !.

% пространство движений для поиска решения
% xo_solve_moves(SolveMoves, MoveType)
%   SolveMoves = [ move(DeltaX, DeltaY) | _ ]
xo_solve_moves([move(1, 0), move(-1, 0)], h).    % горизонталь
xo_solve_moves([move(0, 1), move(0, -1)], v).    % вертикаль
xo_solve_moves([move(1, 1), move(-1, -1)], d1).   % диагональ1
xo_solve_moves([move(1, -1), move(-1, 1)], d2).   % диагональ2

% xo_solve_cells(Solve_ID, [cell(X-Y, Cell_ID_1), ..., cell(X-Y, Cell_ID_WinLength)], MoveType)
% xo_solve_state(Solve_ID, HasChanceMark, X, O, N, Ver, TimeStamp)
% xo_cell_solves(Cell_ID, SolveQty, [Solve_ID_1, ..., Solve_ID_K])

% формирование пространства решений
% xo_make_solves
xo_make_solves :-
    Ps = [ xo_solve_cells/3, xo_solve_state/7, xo_cell_solves/3,
           xo_solve_cached/8, xo_solve_state_sim/7],
    dynamic(Ps),
    forall( member(P, Ps), abolish(P) ),
    dynamic(Ps),
    xo_params(Params),
    memberchk(line(WinLength), Params),
    xo_cell_id(Cell_ID, X-Y),
    xo_solve_moves(Moves, MoveType),
    xo_collect_solve(Moves, X-Y, X-Y, WinLength, [cell(X-Y, Cell_ID)], SolveCells),
    \+ xo_solve_cells(_, SolveCells, _),
    ( xo_solve_cells(ID, _, _), succ(ID, ID1)  -> true ; ID1 = 1),
    asserta( xo_solve_cells(ID1, SolveCells, MoveType) ),
    get_time(TimeStamp),
    asserta( xo_solve_state(ID1, n, 0, 0, WinLength, 0, TimeStamp) ),
    fail.
xo_make_solves :-
    xo_cell_id(ID, _),
    findall( Solve_ID,
             ( xo_solve_cells(Solve_ID, SolveCells, _), memberchk(cell(_, ID), SolveCells) ),
             CellSolves
    ),
    length(CellSolves, SolveQty),
    asserta( xo_cell_solves(ID, SolveQty, CellSolves) ),
    fail.
xo_make_solves :-
    once( xo_solve(_, _) ),
    %Ps = [xo_solve_cells/3, xo_cell_solves/3],
    %compile_predicates(Ps),
    !.

% собрать решение по ячейке
% xo_collect_solve(Moves, BeginCoor, CurrentCoor, WinLength, SolveCoors, SolveCells)
xo_collect_solve(_, _, _, WinLength, SolveCells0, SolveCells) :-
    length(SolveCells0, WinLength),
    sort(SolveCells0, SolveCells),
    !.
xo_collect_solve([Move | Moves], BeginCoor, X-Y, WinLength, SolveCells0, SolveCells) :-
    Move = move(DeltaX, DeltaY),
    plus(X, DeltaX, X1),
    plus(Y, DeltaY, Y1),
    xo_cell_id(ID1, X1-Y1),
    SolveCells1 = [cell(X1-Y1, ID1) | SolveCells0],
    !,
    xo_collect_solve([Move | Moves], BeginCoor, X1-Y1, WinLength, SolveCells1, SolveCells).
xo_collect_solve([_ | Moves], BeginCoor, _, WinLength, SolveCells0, SolveCells) :-
    !,
    xo_collect_solve(Moves, BeginCoor, BeginCoor, WinLength, SolveCells0, SolveCells).

% xo_mode_valid_rule(+Mode, +RuleName, -WinLength, -ModeLevel, -ModeGo)
xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, go(CompMark, UserMark)) :-
    xo_params(Params),
    memberchk(mode_opt(ModeOpt), Params),
    memberchk(rules(Mode, Rules), ModeOpt),
    memberchk(RuleName, Rules),
    memberchk(level(Level), Params),
    xo_mode_level(Mode, ModeOpt, Level, ModeLevel),
    xo_rule(RuleName, RuleOpt),
    memberchk(level(RuleLevel), RuleOpt),
    ModeLevel >= RuleLevel,
    memberchk(line(WinLength), Params),
    memberchk(go(Mark1, Mark2), Params),
    xo_mode_go(Mode, go(Mark1, Mark2), go(CompMark, UserMark)),
    true.

% xo_mode_level(Mode, ModeOpt, Level, ModeLevel)
xo_mode_level(Mode, ModeOpt, Level, ModeLevel) :-
    Mode = echo,
    memberchk(level(Mode, Diff), ModeOpt),
    ModeLevel is Level + Diff,
    !.
xo_mode_level(_, _, Level, Level).

% xo_mode_go(Mode, Go, ModeGo)
xo_mode_go(echo, go(Mark1, Mark2), go(Mark2, Mark1)) :-
    !.
xo_mode_go(normal, Go, Go).

% есть шанс для выигрыша
% xo_has_chance(Mark, Solve_ID, MarkedQty)
xo_has_chance(Mark, Solve_ID, MarkedQty) :-
    ( ground(Solve_ID) -> true ; xo_solve_cells(Solve_ID, _SolveCells, _MoveType) ),
      xo_solve_state_(Solve_ID, HasChanceMark, X, O, N, _Ver, _TimeStamp),
    xo_chance_state(Mark, HasChanceMark, X, O, N, MarkedQty),
    true.

xo_chance_state(Mark, n, X, O, N, MarkedQty) :-
    xo_marked_qty(Mark, X, O, N, MarkedQty),
    !.
xo_chance_state(Mark, Mark, X, O, N, MarkedQty) :-
    xo_marked_qty(Mark, X, O, N, MarkedQty),
    !.

xo_marked_qty(x, X, _O, _N, X).
xo_marked_qty(o, _X, O, _N, O).
xo_marked_qty(n, _X, _O, N, N).

% выигрыш
% xo_win(Mode, Mark, X, Y)
xo_win(Mode, Mark, X, Y) :-
    xo_win(Mode, Mark, Solve),
    member(cell(X-Y, _), Solve).
% xo_win(Mode, Mark, Solve)
xo_win(Mode, Mark, Solve) :-
    xo_params(Params),
    memberchk(line(WinLength), Params),
    memberchk(go(CompMark, UserMark), Params),
    member(Mode-Mark, [normal-CompMark, echo-UserMark]),
    xo_has_chance(Mark, Solve_ID, WinLength),
    xo_solve(Solve_ID, Solve, _),
    !.

% ничья
% xo_tie(Mode)
xo_tie(Mode) :-
    RuleName = tie_by_chance,
    xo_mode_valid_rule(Mode, RuleName, _WinLength, _ModeLevel, go(CompMark, UserMark)),
    \+ xo_has_chance(CompMark, _, _),
    \+ xo_has_chance(UserMark, _, _),
    !.
xo_tie(_) :-
    \+ xo_cell(_, n),
    !.

% xo_threat_four_cells(MarkedQty, ShapeLen, go(CompMark, UserMark), PlayCells)
xo_threat_four_cells(MarkedQty, ShapeLen, go(CompMark, UserMark), PlayCells) :-
    length(ShapeCells, ShapeLen),
    ( Mark = CompMark ; Mark = UserMark ),
    xo_marked_solves(Mark, MarkedQty, MarkedSolves),
    %
    findall(
        PlayCell,
        xo_threat_four_cells_(Mark, ShapeCells, MarkedSolves, PlayCell),
    AllPlayCells ),
    \+ AllPlayCells = [],
    %check_point,
    sort(AllPlayCells, PlayCells),
    true.

% xo_marked_solves(Mark, MarkedQty, MarkedSolves)
xo_marked_solves(Mark, MarkedQty, MarkedSolves) :-
    findall( xo_solve(Solve_ID, Solve, MoveType),
             ( xo_solve_cells(Solve_ID, SolveCells, MoveType),
               xo_solve_state_(Solve_ID, Mark1, X, O, N, _, _),
               Mark1 = Mark,
               xo_marked_qty(Mark, X, O, N, MarkedQty),
               xo_solve_cells_state(SolveCells, Solve)
             ),
    MarkedSolves ),
    !.

% xo_threat_four_cells_(Mark, ShapeCells, MarkedSolves, PlayCell)
xo_threat_four_cells_(Mark, ShapeCells, MarkedSolves, PlayCell) :-
    select(xo_solve(_, Solve1, _), MarkedSolves, MarkedSolves1),
    select(xo_solve(_, Solve2, _), MarkedSolves1, _),
    %intersection(Solve1, Solve2, ShapeCells),
    append(_, ShapeCells, Solve1), append(ShapeCells, _, Solve2),
    FreeCell = cell(Coor, n),
    member(FreeCell, ShapeCells),
    PlayCell = cell(Coor, Mark),
    true.


% игра
% xo_play(Mode, X, Y, Rule)
xo_play(Mode, X, Y, Rule) :-
    Cell = cell(X-Y, _),
    xo_play(Mode, Cell, _-Rule),
    !.
% xo_play(Mode, PlayCell, RuleName-Rule)
% первый ход - случайный выбор из лучших позиций
xo_play(Mode, PlayCell, RuleName-Rule) :-
    \+ xo_cell(_, x),
    \+ xo_cell(_, o),
    RuleName = random_best_pos,
    xo_mode_valid_rule(Mode, RuleName, _WinLength, ModeLevel, _ModeGo),
    %check_point,
    Method = 0,
    findall( Method-rate(SolveQty, 0) / Coor,
             ( xo_cell_id(ID, Coor),
               once( xo_cell_solves(ID, SolveQty, _) )
             ),
             RateCoorList
    ),
    \+ RateCoorList = [],
    sort(RateCoorList, SortedRateCoorList),
    reverse(SortedRateCoorList, PlayCoorList),
    length(PlayCoorList, PlayLen),
    catch( PlayBest is PlayLen // 2 ^ (ModeLevel - 2), _, PlayBest = 1 ),
    catch( PlayIndex is random(PlayBest), _, PlayIndex = 0 ),
    nth0(PlayIndex, PlayCoorList, Method-Rate / _),
    findall( Coor,
             member(Method-Rate / Coor, PlayCoorList),
             PlayRateCoorList
    ),
    length(PlayRateCoorList, PlayRateLen),
    PlayRateIndex is random(PlayRateLen),
    nth0(PlayRateIndex, PlayRateCoorList, PlayCoor),
    PlayCell = cell(PlayCoor, _),
    Rule = rule(RuleName,method=Method,length=PlayLen/PlayRateLen,index=PlayIndex/PlayRateIndex),
    true.
% первый ход - случайный выбор
xo_play(Mode, PlayCell, RuleName-Rule) :-
    \+ xo_cell(_, x),
    \+ xo_cell(_, o),
    xo_random_free_cell(Mode, PlayCell, RuleName-Rule),
    true.
% выигрыш следующим ходом
xo_play(Mode, PlayCell, RuleName-Rule) :-
    RuleName = next_step_win,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, go(CompMark, UserMark)),
    plus(WinLength, -1, ToWinLength),
    %
    ( Mark = CompMark ; Mark = UserMark ),
    xo_has_chance(Mark, Solve_ID, ToWinLength),
    xo_solve(Solve_ID, Solve, _),
    %
    memberchk(cell(X-Y, n), Solve),
    PlayCell = cell(X-Y, Mark),
    Rule = rule(RuleName,Mark,X,Y),
    true.
% свободные края (выигрыш через ход)
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = echo,
    xo_mode_valid_rule(Mode, -free_border, WinLength, _ModeLevel, go(CompMark, UserMark)),
    plus(WinLength, -1, ToWinCut1), % 4
    plus(WinLength, -2, ToWinCut2), % 3
    plus(WinLength, -3, ToWinCut3), % 2
    ( ToWinCut = ToWinCut1, RuleName = free_border
    ; ToWinCut = ToWinCut2, RuleName = dash_mark
    ; ToWinCut = ToWinCut3, RuleName = side_by_side ),
    xo_threat_four_cells(ToWinCut2, ToWinCut, go(CompMark, UserMark), PlayList),
    %check_point,
    length(PlayList, PlayLength),
    PlayIndex is random(PlayLength),
    nth0(PlayIndex, PlayList, PlayCell),
    Rule = rule(RuleName-new, PlayCell-PlayList),
    true.
% свободные края (выигрыш через ход)
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = normal,
    RuleName = free_border,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, go(CompMark, UserMark)),
    plus(WinLength, -2, ToWinLength),
    %
    ( Mark = CompMark ; Mark = UserMark ),
    xo_has_chance(Mark, Solve_ID, ToWinLength),
    xo_solve(Solve_ID, Solve, _),
    %
    Solve = [First | Right],
    append(Left, [Last], Solve),
    %check_point
    ( % n nooon n
      First = cell(_, n),
      Last = cell(_, n),
      xo_has_chance(Mark, SolveBorder1_ID, ToWinLength),
      xo_solve(SolveBorder1_ID, SolveBorder1, _),
      append(Right, [cell(_, n)], SolveBorder1),
      xo_has_chance(Mark, SolveBorder2_ID, ToWinLength),
      xo_solve(SolveBorder2_ID, SolveBorder2, _),
      append([cell(_, n)], Left, SolveBorder2),
      PlayList = [First, Last]
    ; % _ nonoo n | _ nnooo n
      xo_has_chance(Mark, SolveBorder1_ID, ToWinLength),
      xo_solve(SolveBorder1_ID, SolveBorder1, _),
      append(Right, [cell(_, n)], SolveBorder1),
      FreeCell = cell(_, n),
      memberchk(FreeCell, Right),
      PlayList = [FreeCell]
    ; % n oonon _ | n ooonn _
      xo_has_chance(Mark, SolveBorder2_ID, ToWinLength),
      xo_solve(SolveBorder2_ID, SolveBorder2, _),
      append([cell(_, n)], Left, SolveBorder2),
      FreeCell = cell(_, n),
      memberchk(FreeCell, Left),
      PlayList = [FreeCell]
    ),
    %check_point,
    length(PlayList, PlayLength),
    PlayIndex is random(PlayLength),
    nth0(PlayIndex, PlayList, cell(X-Y, n)),
    PlayCell = cell(X-Y, Mark),
    Rule = rule(RuleName,Mark,X,Y),
    true.
% тире (выигрыш через ход)
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = normal,
    RuleName = dash_mark,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, go(CompMark, UserMark)),
    plus(WinLength, -2, ToWinCut2),
    plus(WinLength, -3, ToWinCut3),
    ( Mark = CompMark ; Mark = UserMark ),
    xo_has_chance(Mark, Solve_ID, ToWinCut3),
    xo_solve(Solve_ID, Solve, _),
    Solve = [First | Right],
    append(Left, [Last], Solve),
    %check_point,
    ( % o nonon o
      First = cell(_, n),
      Last = cell(_, n),
      xo_has_chance(Mark, SolveBorder1_ID, ToWinCut2),
      xo_solve(SolveBorder1_ID, SolveBorder1, _),
      append(Right, [cell(_, Mark)], SolveBorder1),
      xo_has_chance(Mark, SolveBorder2_ID, ToWinCut2),
      xo_solve(SolveBorder2_ID, SolveBorder2, _),
      append([cell(_, Mark)], Left, SolveBorder2),
      Left = [First | Middle],
      FreeCell = cell(_, n),
      memberchk(FreeCell, Middle),
      PlayList = [FreeCell]
    ),
    %check_point,
    length(PlayList, PlayLength),
    PlayIndex is random(PlayLength),
    nth0(PlayIndex, PlayList, cell(X-Y, n)),
    PlayCell = cell(X-Y, Mark),
    Rule = rule(RuleName,Mark,X,Y),
    true.
% вилка
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = echo,
    RuleName = fork,
    xo_fork_engine(Mode, RuleName, SortedForks),
    %check_point,
    % fork( 7-extra(0), 6-order(Priority, ForksQty, Flatness), 5-coef(CellCoef, AvgCellCoef), 4-profile(0), 2-Cell, 1-cross(Solve_IDs), 0-img(Marks, Lens, MoveTypes))
    SortedForks = [BestFork | TeilForkList],
    BestFork =.. BestForkArgs,
    BestFork = fork(Order, Extra, _, _, _, _, _),
    SimilarFork = fork(Order, Extra, _, _, _, _, _),
    findall( SimilarFork,
             member(SimilarFork, [BestFork | TeilForkList]),
    SimilarForks),
    length(SimilarForks, PlayLength),
    PlayIndex is random(PlayLength),
    nth0(PlayIndex, SimilarForks, PlayFork),
    %
    memberchk(2-PlayCell, BestForkArgs),
    Rule = rule(RuleName-new, PlayFork),
    !.
% вилка
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = normal,
    RuleName = fork,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, go(CompMark, UserMark)),
    xo_mode_go(Mode, go(Mark1, _Mark2), go(CompMark, UserMark)),
    plus(WinLength, -2, ToWinLength1),
    plus(WinLength, -3, ToWinLength2),
    MarkedQtyList = [ToWinLength1, ToWinLength2],
    OrderMarkList = [1-CompMark, 0-UserMark],
    %check_point,
    findall( MarkedQty-Order-Mark-Solve,
             ( member(MarkedQty, MarkedQtyList),
               member(Order-Mark, OrderMarkList),
               xo_has_chance(Mark, Solve_ID, MarkedQty),
               xo_solve(Solve_ID, Solve, _)
             ),
             MarkedSolveList
    ),
    \+ MarkedSolveList = [],
    sort(MarkedSolveList, SortedSolveList),
    reverse(SortedSolveList, ClaimForkList),
    %check_point,
    findall( Extra-Fork,
             ( xo_has_fork(ClaimForkList, Fork),
               xo_fork_extra(ModeLevel, Mark1, Fork, Extra, RuleName)
             ),
             ForkList
    ),
    \+ ForkList = [],
    sort(ForkList, SortedForkList),
    reverse(SortedForkList, [BestFork | TeilForkList]),
    BestFork = ForkExtra-fork(ForkHeight, ForkPower, ForkWidth, ForkOrder, _, _),
    PlayFork = ForkExtra-fork(ForkHeight, ForkPower, ForkWidth, ForkOrder, _, _),
    %check_point,
    findall( PlayFork,
             member(PlayFork, [BestFork | TeilForkList]),
             PlayForkList
    ),
    length(PlayForkList, PlayLength),
    PlayIndex is random(PlayLength),
    nth0(PlayIndex, PlayForkList, PlayFork),
    %check_point,
    PlayFork = Extra-fork(Height, Power, Width, Order, Mark, cell(X-Y, n)),
    PlayCell = cell(X-Y, Mark),
    Rule = rule(RuleName,extra=Extra,height=Height,power=Power,width=Width,order=Order,Mark,X,Y),
    true.
% случайный выбор из лучших шансов на выигрыш
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = echo,
    RuleName = random_best_chance,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, go(_CompMark, _UserMark)),
    %xo_mode_go(Mode, go(Mark1, _Mark2), go(CompMark, UserMark)),
    xo_limit_coor(WinLength, _LimitData),
    member(Cost, [2, 1]),
    xo_best_chance_engine(Mode, RuleName, Cost, PlayCellList),
    % turn(Extra, Profile, cell(Coor, Cell_ID))
    %check_point,
    length(PlayCellList, PlayLen),
    catch( PlayBest is PlayLen // 2 ^ (ModeLevel - 2), _, PlayBest = 1 ),
    catch( PlayIndex is random(PlayBest), _, PlayIndex = 0 ),
    nth0(PlayIndex, PlayCellList, ClaimCell),

    ClaimCell = turn(_-pf(sc(Score), cf(CenterFactor), _, _), Extra,  _),
    OfferCell = turn(_-pf(sc(Score), cf(CenterFactor), _, _), Extra, _),
    findall( OfferCell,
             member(OfferCell, PlayCellList),
             OfferCells
    ),
    length(OfferCells, OfferLen),
    OfferIndex is random(OfferLen),
    nth0(OfferIndex, OfferCells, CompoundCell),
    CompoundCell = turn(_, _, PlayCell),
    Rule = rule(RuleName-new,CompoundCell,cost=Cost,length=PlayLen/OfferLen,index=PlayIndex/OfferIndex),
    true.
% случайный выбор из лучших шансов на выигрыш
xo_play(Mode, PlayCell, RuleName-Rule) :-
    Mode = normal,
    RuleName = random_best_chance,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, go(CompMark, UserMark)),
    xo_mode_go(Mode, go(Mark1, _Mark2), go(CompMark, UserMark)),
    xo_limit_coor(WinLength, LimitData),
    member(Cost, [2, 1, 0]),
    %check_point,
    RateShape = [TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount],
    xo_rate_shape(RateShape, Method-Rate),
    findall( Extra-Method-Rate / Coor,
             ( xo_cell(Coor, n),
               xo_check_coor(Coor, LimitData),
               xo_rate(CompMark, Coor, Cost, CompGift-CompCount),
               xo_rate(UserMark, Coor, Cost, UserGift-UserCount),
               TotalCount0 is CompCount + UserCount,
               TotalCount0 > 0,
               TotalGift0 is CompGift + UserGift,
               to_currency(TotalCount0, TotalCount),
               to_currency(TotalGift0, TotalGift),
               xo_rate_extra(Cost, ModeLevel, CompMark, Mark1, Coor, Extra)
             ),
             RateCoorList
    ),
    \+ RateCoorList = [],
    sort(RateCoorList, SortedRateCoorList),
    reverse(SortedRateCoorList, PlayCoorList),
    length(PlayCoorList, PlayLen),
    catch( PlayBest is PlayLen // 2 ^ (ModeLevel - 2), _, PlayBest = 1 ),
    catch( PlayIndex is random(PlayBest), _, PlayIndex = 0 ),
    nth0(PlayIndex, PlayCoorList, Extra-Method-Rate / _),
    findall( Coor,
             member(Extra-Method-Rate / Coor, PlayCoorList),
             PlayRateCoorList
    ),
    length(PlayRateCoorList, PlayRateLen),
    PlayRateIndex is random(PlayRateLen),
    nth0(PlayRateIndex, PlayRateCoorList, PlayCoor),
    PlayCell = cell(PlayCoor, _),
    Rule = rule(RuleName,extra=Extra,method=Method,cost=Cost,length=PlayLen/PlayRateLen,index=PlayIndex/PlayRateIndex),
    true.
% случайный выбор из шансов на выигрыш
xo_play(Mode, PlayCell, RuleName-Rule) :-
    RuleName = random_chance,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, go(CompMark, UserMark)),
    xo_limit_coor(WinLength, LimitData),
    member(Cost, [1, 0]),
    ( Mark = CompMark ;  Mark = UserMark ),
    findall( Coor,
             ( xo_cell(Coor, n),
               xo_check_coor(Coor, LimitData),
               xo_rate(Mark, Coor, Cost, _-Count),
               \+ Count = 0
             ),
             FreeCoorList
    ),
    \+ FreeCoorList = [],
    length(FreeCoorList, Len),
    Index is random(Len),
    nth0(Index, FreeCoorList, Coor),
    PlayCell = cell(Coor, _),
    Rule = rule(RuleName,cost=Cost,length=Len,index=Index),
    true.
% случайный выбор свободной ячейки
xo_play(Mode, PlayCell, RuleName-Rule) :-
    xo_random_free_cell(Mode, PlayCell, RuleName-Rule),
    true.
% выбор первой свободной ячейки (заглушка)
xo_play(_, cell(Coor, n), none-rule(none)) :-
    xo_cell(Coor, n),
    true.

% xo_best_chance_engine(Mode, RuleName, Cost, SortedRateCells)
xo_best_chance_engine(Mode, RuleName, Cost, SortedCells) :-
    xo_mode_valid_rule(Mode, RuleName, _WinLength, ModeLevel, go(CompMark, UserMark)),
    % найти все ценные линии
    xo_line_solves(Cost, ExpSolves),
    \+ ExpSolves = [],
    % собрать свободные клетки
    findall( Cell,
             ( member(exp_solve(_, _, _, FreeCells), ExpSolves ),
               member(Cell, FreeCells)
             ),
    FreeCells0 ),
    \+ FreeCells0 = [],
    sort(FreeCells0, FreeCells),
    xo_rate_shape(_, Method-_),
    findall( turn(Profile, Extra, cell(Coor, Cell_ID)),
             ( member(cell(Coor, Cell_ID), FreeCells),
               xo_rate_cell_id(CompMark-UserMark-Cost-Method, Cell_ID, Profile),
               xo_extra_scene(CompMark-ModeLevel, Cell_ID, Extra),
               true
             ),
    RatedFreeCells),
    sort(0, @>, RatedFreeCells, SortedCells),
    %
    !.
    
% xo_fork_engine(Mode, RuleName, SortedForks)
xo_fork_engine(Mode, RuleName, SortedForks) :-
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, go(CompMark, UserMark)),
    % найти все тройки
    plus(WinLength, -2, ForkLen1),
    xo_line_solves(ForkLen1, ExpSolves1),
    % найти все двойки
    plus(WinLength, -3, ForkLen2),
    xo_line_solves(ForkLen2, ExpSolves2),
    \+ (ExpSolves1 = [], ExpSolves2 = []),
    % собрать из троек свободные клетки
    findall( Cell,
             ( member(exp_solve(_, _, _, FreeCells), ExpSolves1 ),
               member(Cell, FreeCells)
             ),
    FreeCells01 ),
    % собрать из двоек свободные клетки
    findall( Cell,
             ( member(exp_solve(_, _, _, FreeCells), ExpSolves2 ),
               member(Cell, FreeCells)
             ),
    FreeCells02 ),
    %
    \+ (FreeCells01 = [], FreeCells02 = []),
    sort(FreeCells01, FreeCells1),
    sort(FreeCells02, FreeCells2),
    %
    xo_fork_shape(CompMark-UserMark, ForkLen1-ForkLen2, ForkShapeList),
    Weigth is ForkLen1 * 2 + 1,
    %check_point,
    findall( ShapeForks,
             ( member(fork_shape(Marks, Len1-Len2, Priority), ForkShapeList),
               ( Len1 = ForkLen1 -> FreeCells11 = FreeCells1, ExpSolves11 = ExpSolves1
               ; Len1 = ForkLen2, FreeCells11 = FreeCells2, ExpSolves11 = ExpSolves2 ),
               ( Len2 = ForkLen1 -> FreeCells22 = FreeCells1, ExpSolves22 = ExpSolves1
               ; Len2 = ForkLen2, FreeCells22 = FreeCells2, ExpSolves22 = ExpSolves2 ),
               xo_cell_forks(FreeCells11, FreeCells22, ExpSolves11, ExpSolves22, ShapeForks, Marks, Len1-Len2, Priority, Weigth),
               true
             ),
    AllShapeForks),
    %
    flatten(AllShapeForks, ClaimForks),
    \+ ClaimForks = [],
    %check_point,
    sort(ClaimForks, SortedClaimForks),
    xo_forks_multi_cell(SortedClaimForks, OfferForks),
    xo_rate_shape(_, Method-_),
    xo_forks_cell_rate(OfferForks, RatedOfferForks, CompMark-UserMark-ForkLen1-ModeLevel-Method),
    %
    sort(0, @>, RatedOfferForks, SortedForks),
    !.

% xo_line_solves(MarkedQty, ExpSolves)
xo_line_solves(MarkedQty, ExpSolves) :-
    findall( exp_solve(Solve_ID, MoveType, Mark, FreeCells),
             ( xo_solve_cells(Solve_ID, SolveCells, MoveType),
               xo_solve_state_(Solve_ID, Mark, X, O, N, _, _),
               \+ Mark = n,
               xo_marked_qty(Mark, X, O, N, MarkedQty),
               findall( cell(Coor, Cell_ID),
                        ( member(cell(Coor, Cell_ID), SolveCells),
                          xo_cell_state_(Cell_ID, StateMark, _, _),
                          StateMark = n
                        ),
               FreeCells )
             ),
    ExpSolves ),
    !.

% xo_fork_shape(+Marks, +Lens, -ForkShapeList)
xo_fork_shape(CompMark-UserMark, ForkLen1-ForkLen2, ForkShapeList) :-
    ground(CompMark-UserMark),
    ground(ForkLen1-ForkLen2),
    ForkShapeList = [
        fork_shape(CompMark-CompMark, ForkLen1-ForkLen1, 12), % x4-x4
        fork_shape(UserMark-UserMark, ForkLen1-ForkLen1, 11), % o4-o4
        
        fork_shape(CompMark-UserMark, ForkLen1-ForkLen1, 10), % x4-o4
        %fork_shape(UserMark-CompMark, ForkLen1-ForkLen1, 9), % o4-x4
        
        fork_shape(CompMark-CompMark, ForkLen1-ForkLen2, 8), % x4-x3
        fork_shape(UserMark-UserMark, ForkLen1-ForkLen2, 7), % o4-o3

        fork_shape(CompMark-UserMark, ForkLen1-ForkLen2, 6), % x4-o3
        fork_shape(UserMark-CompMark, ForkLen1-ForkLen2, 5), % o4-x3
        
        fork_shape(CompMark-CompMark, ForkLen2-ForkLen2, 4), % x3-x3
        fork_shape(UserMark-UserMark, ForkLen2-ForkLen2, 3), % o3-o3
        
        fork_shape(CompMark-UserMark, ForkLen2-ForkLen2, 2), % x3-o3
        %fork_shape(UserMark-CompMark, ForkLen2-ForkLen2, 1), % o3-x3
        - ],
    !.

% fork( 7-extra(0), 6-order(Priority, ForksQty, Flatness), 5-coef(CellCoef, AvgCellCoef), 4-profile(0), 2-Cell, 1-cross(Solve_IDs), 0-img(Marks, Lens, MoveTypes) )
% xo_cell_forks(FreeCells1, FreeCells2, ExpSolves1, ExpSolves2, Forks, Marks, Lens, Priority, Weigth)
xo_cell_forks(FreeCells1, FreeCells2, ExpSolves1, ExpSolves2, Forks, Mark1-Mark2, Len1-Len2, Priority, Weigth) :-
    findall( fork( 7-extra(0), 6-order(Priority, 1, Flatness), 5-coef(CellCoef, CellCoef), 4-pf(0),
                   2-Cell, 1-cross(Solve_ID11-Solve_ID22), 0-img(Mark1-Mark2, Len1-Len2, MoveType1-MoveType2) ),
             ( % если определенная ячейка из 1-го списка свободных ячеек
               member(Cell, FreeCells1),
               % присутствует во 2-м списке свободных ячеек
               memberchk(Cell, FreeCells2),
               % и есть два разных решения по указанным отметкам
               member(exp_solve(Solve_ID1, MoveType1, Mark1, ExpFreeCells1), ExpSolves1),
               member(exp_solve(Solve_ID2, MoveType2, Mark2, ExpFreeCells2), ExpSolves2),
               \+ Solve_ID1 = Solve_ID2,
               % в двух плоскостях
               ( MoveType1 = MoveType2 -> Flatness = 1 ; Flatness = 2),
               % содержащие данную ячейку в списке свободных ячеек обоих решений
               memberchk(Cell, ExpFreeCells1),
               memberchk(Cell, ExpFreeCells2),
               % то это вилка!
               sort([Solve_ID1, Solve_ID2], [Solve_ID11, Solve_ID22]),
               CellCoef0 is (Len1 + Len2) / Weigth,
               to_currency(CellCoef0, CellCoef),
               true
             ),
    Forks ),
    !.

% xo_forks_multi_cell(ClaimForks, OfferForks)
xo_forks_multi_cell(ClaimForks, OfferForks) :-
    xo_forks_multi_cell_(ClaimForks, ClaimForks, OfferForks),
    !.
% fork( 7-extra(0), 6-order(Priority, ForksQty, Flatness), 5-coef(CellCoef, AvgCellCoef), 4-profile(0), 2-Cell, 1-cross(Solve_IDs), 0-img(Marks, Lens, MoveTypes))
xo_forks_multi_cell_([], _, []).
xo_forks_multi_cell_([ClaimFork | ClaimForks], ClaimForks0, [OfferFork | OfferForks]) :-
    ClaimFork =
        fork( Extra, 6-order(Priority, _ForksQty, Flatness), 5-coef(CellCoef, _), Profile,
              Cell, 1-cross(Solve_IDs), Img ),
    findall( CellCoef0,
             ( ClaimFork0 =
                   fork( _Extra, _Order, 5-coef(CellCoef0, _), _Profile,
                         Cell, 1-cross(Solve_IDs0), _Img ),
               member(ClaimFork0, ClaimForks0),
               % Solve_IDs sorted in xo_cell_forks
               \+ Solve_IDs0 = Solve_IDs,
               true
             ),
    CellCoefList),
    sumlist([CellCoef | CellCoefList], SumCellCoef),
    length([CellCoef | CellCoefList], ForksQty),
    AvgCellCoef0 is SumCellCoef / ForksQty,
    to_currency(AvgCellCoef0, AvgCellCoef),
    OfferFork =
        fork( Extra, 6-order(Priority, ForksQty, Flatness), 5-coef(CellCoef, AvgCellCoef),  Profile,
              Cell, 1-cross(Solve_IDs), Img ),
    !,
    xo_forks_multi_cell_(ClaimForks, ClaimForks0, OfferForks).

% xo_forks_cell_rate(OfferForks, RatedOfferForks, CompMark-UserMark-Cost-ModeLevel-Method)
xo_forks_cell_rate([], [], _).
xo_forks_cell_rate([OfferFork | OfferForks], [RatedOfferFork | RatedOfferForks], CompMark-UserMark-Cost-ModeLevel-Method) :-
    OfferFork =
        fork(_Extra, Order, Coef, _Profile, Cell, Cross, Img),
    Cell = _-cell(_Coor, Cell_ID),
    xo_rate_cell_id(CompMark-UserMark-Cost-Method, Cell_ID, Profile),
    xo_extra_scene(CompMark-ModeLevel, Cell_ID, Extra),
    RatedOfferFork0 =
        fork(Extra, Order, Coef, Profile, Cell, Cross, Img ),
    RatedOfferFork0 =.. [Head | ForkArgs],
    sort(0, @>, ForkArgs, SortedForkArgs),
    RatedOfferFork =.. [Head | SortedForkArgs],
    !,
    xo_forks_cell_rate(OfferForks, RatedOfferForks, CompMark-UserMark-Cost-ModeLevel-Method).

% xo_rate_cell_id(CompMark-UserMark-Cost-Method, Cell_ID, Profile)
xo_rate_cell_id(CompMark-UserMark-Cost-Method, Cell_ID, 4-Profile) :-
    xo_rate_cell_id_(CompMark, Cell_ID, Cost, CompGift, CompCount),
    xo_rate_cell_id_(UserMark, Cell_ID, Cost, UserGift, UserCount),
    TotalCount0 is CompCount + UserCount,
    TotalCount0 > 0,
    TotalGift0 is CompGift + UserGift,
    to_currency(TotalGift0, TotalGift),
    to_currency(TotalCount0, TotalCount),
    Shape = [TotalGift-tg, TotalCount-tc, CompGift-cg, UserGift-ug, CompCount-cc, UserCount-uc],
    xo_rate_profile(Cell_ID, Cost-Method, Shape, Profile),
    !.
xo_rate_cell_id(_, _, 4-pf(0)).

% xo_extra_scene(Mark-ModeLevel, ID, Extra)
xo_extra_scene(Mark-ModeLevel, ID, 3-Extra) :-
    xo_extra_scene_(Mark-ModeLevel, ID, Extra).
xo_extra_scene_(_-ModeLevel, _, extra(0)) :-
    ( ModeLevel < 8 ; xo_cell_state_sim(_, _, _, _) ),
    %( ModeLevel < 8 ; xo_cell_state_sim(_, _, _, _) ; true),
    !.
xo_extra_scene_(Mark-_, ID, Extra) :-
    ground([Mark, ID]),
    xo_get_cell(ID, _, n),
    xo_cell_solves(ID, _, CellSolves),
    %
    retractall( xo_cell_state_sim(_, _, _, _) ),
    retractall( xo_solve_state_sim(_, _, _, _, _, _, _) ),
    %
    xo_set_cell(ID, _, Mark, 1),
    forall( member(Solve_ID, CellSolves),
            ( xo_solve_state_(Solve_ID, _, X, O, N, _, _),
              ( xo_change_state(n, Mark, X, O, N, X1, O1, N1, H1)
               ->
                ChangedState = [x-X1, o-O1, n-N1] / H1,
                xo_set_solve_state(Solve_ID, ChangedState, 1)
              ; true )
            )
    ),
    %
    xo_next_step_win(Mark, WinQty),
    xo_best_fork(Mark, ForkQtyList),
    xo_best_cell(0, [3, 2], PowerCells),
    Scene =.. [extra, win_qty(WinQty), fork_qty(ForkQtyList) | PowerCells],
    Extra = Scene,
    %
    retractall( xo_cell_state_sim(_, _, _, _) ),
    retractall( xo_solve_state_sim(_, _, _, _, _, _, _) ),
    !.
xo_extra_scene_(_, _, extra(fail)).

xo_next_step_win(Mark, 1) :-
    xo_params(Params),
    memberchk(line(WinLength), Params),
    plus(WinLength, -1, ToWinNextStep),
    xo_has_chance(Mark, _Solve_ID, ToWinNextStep),
    !.
xo_next_step_win(_, 0).

xo_best_fork(_Mark, ForkQtyList) :-
    Mode = echo,
    RuleName = fork,
    xo_fork_engine(Mode, RuleName, SortedForks),
    %check_point,
    findall( Priority-ForkCoef,
             ( member(Priority, [12, 8, 4]),
               findall( ForkQty-1,
                        ( member(Fork, SortedForks),
                          Fork =.. ForkArgs,
                          memberchk(_-order(Priority, ForkQty, _), ForkArgs),
                          true
                        ),
               ForkPairList),
               sum_int_pairs(ForkPairList, ForkQtySum-ForkCount),
               ForkCoef0 is ForkQtySum / (ForkCount + 0.0001) + 0.0001,
               to_currency(ForkCoef0, ForkCoef, 2),
               true
             ),
    ForkQtyList),
    !.
xo_best_fork(_, []).

%pf(sc(Score), cf(CenterFactor), rate(Rate), Desc)
xo_best_cell(_, [], []).
xo_best_cell(0, [Cost | Costs], [avg_pf(Cost, CellScore) | BestCells]) :-
    Mode = echo,
    RuleName = random_best_chance,
    xo_best_chance_engine(Mode, RuleName, Cost, PlayCellList),
    %check_point,
    findall( Score-CenterFactor,
             ( member(Turn, PlayCellList),
               Turn =.. TurnArgs,
               memberchk(_-pf(sc(Score), cf(CenterFactor), _, _), TurnArgs),
               true
             ),
    ScoreList),
    %check_point,
    \+ ScoreList = [],
    length(ScoreList, ScoreLen),
    sum_int_pairs(ScoreList, SumScore-SumCenterFactor),
    AvgScore0 is SumScore / (ScoreLen + 0.0001) + 0.0001,
    AvgCenterFactor0 is SumCenterFactor / (ScoreLen + 0.0001) + 0.0001,
    to_currency(AvgScore0, AvgScore, 2),
    to_currency(AvgCenterFactor0, AvgCenterFactor, 2),
    CellScore = AvgScore-AvgCenterFactor,
    !,
    xo_best_cell(1, Costs, BestCells).
xo_best_cell(Mode, [Cost | Costs], [avg_pf(Cost, 0.0-0.0) | BestCells]) :-
    !,
    xo_best_cell(Mode, Costs, BestCells).


% xo_rate_profile(+Cell_ID, +Cost, +Shape, -Profile)
xo_rate_profile(Cell_ID, Cost-Method, Shape, Profile) :-
    ground([Cell_ID, Cost, Shape]),
    xo_rate_profile_(Cell_ID, Cost-Method, Shape, Profile),
    !.

xo_rate_profile_(Cell_ID, Cost-_Method, Shape, Profile) :-
    Cost > 1,
    Shape = [TotalGift-tg, TotalCount-tc, CompGift-cg, UserGift-ug, CompCount-cc, UserCount-uc],
    CompGift >= UserGift, CompCount >= UserCount,
    OrderRate = [6-TotalGift-tg, 5-TotalCount-tc, 4-CompGift-cg, 2-UserGift-ug, 3-CompCount-cc, 1-UserCount-uc],
    sort(0, @>, OrderRate, Rate),
    ( CompGift > UserGift, CompCount > UserCount -> Score = 80, Desc = desc(attack, total)
    ; CompGift > UserGift, CompCount = UserCount -> Score = 75, Desc = desc(attack, gift)
    ; CompGift = UserGift, CompCount > UserCount -> Score = 75, Desc = desc(attack, count)
    ; CompGift = UserGift, CompCount = UserCount -> Score = 60, Desc = desc(neutral, pressure)
    ),
    once( xo_cell_solves(Cell_ID, CenterFactor, _) ),
    Profile = pf(sc(Score), cf(CenterFactor), rate(Rate), Desc),
    !.
xo_rate_profile_(Cell_ID, Cost-_Method, Shape, Profile) :-
    Cost > 1,
    Shape = [TotalGift-tg, TotalCount-tc, CompGift-cg, UserGift-ug, CompCount-cc, UserCount-uc],
    CompGift =< UserGift, CompCount =< UserCount,
    OrderRate = [6-TotalGift-tg, 5-TotalCount-tc, 2-CompGift-cg, 4-UserGift-ug, 1-CompCount-cc, 3-UserCount-uc],
    sort(0, @>, OrderRate, Rate),
    ( CompGift < UserGift, CompCount < UserCount -> Score = 70, Desc = desc(defence, total)
    ; CompGift < UserGift, CompCount = UserCount -> Score = 65, Desc = desc(defence, gift)
    ; CompGift = UserGift, CompCount < UserCount -> Score = 65, Desc = desc(defence, count)
    ),
    once( xo_cell_solves(Cell_ID, CenterFactor, _) ),
    Profile = pf(sc(Score), cf(CenterFactor), rate(Rate), Desc),
    !.
xo_rate_profile_(Cell_ID, Cost-Method, Shape, Profile) :-
    Cost > 0,
    Shape = [TotalGift-tg, TotalCount-tc, CompGift-cg, UserGift-ug, CompCount-cc, UserCount-uc],
    RateShape = [TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount],
    xo_rate_shape(RateShape, Method-Rate),
    Score = 50, Desc = desc(method, Method),
    once( xo_cell_solves(Cell_ID, CenterFactor, _) ),
    Profile = pf(sc(Score), cf(CenterFactor), Rate, Desc),
    !.
xo_rate_profile_(Cell_ID, _, Shape, Profile) :-
    Shape = [TotalGift-tg, TotalCount-tc | _],
    AvgGift0 is TotalGift / 2,
    AvgCount0 is TotalCount / 2,
    to_currency(AvgGift0, AvgGift, 2),
    to_currency(AvgCount0, AvgCount, 2),
    OrderRate = [2-AvgGift-ag, 1-AvgCount-ac],
    sort(0, @>, OrderRate, Rate),
    Score = 30, Desc = desc(lowcost, average),
    once( xo_cell_solves(Cell_ID, CenterFactor, _) ),
    Profile = pf(sc(Score), cf(CenterFactor), rate(Rate), Desc),
    !.

% xo_rate_cell_id_(+Mark, +ID, +Cost, -GiftCoef, -CountCoef)
xo_rate_cell_id_(Mark, ID, Cost, GiftCoef, CountCoef) :-
    ground([Mark, ID, Cost]),
    xo_params(Params),
    memberchk(line(WinLength), Params),
    memberchk(max_solve_qty(MaxSolveQty), Params),
    findall( MarkedQty-1,
             ( once( xo_cell_solves(ID, _, CellSolves) ),
               member(Solve_ID, CellSolves),
               xo_has_chance(Mark, Solve_ID, MarkedQty),
               MarkedQty >= Cost
             ),
             MarkedQtyList
    ),
    sum_int_pairs(MarkedQtyList, Gift-Count),
    GiftCoef0 is Gift / MaxSolveQty * WinLength,
    CountCoef0 is Count / MaxSolveQty,
    to_currency(GiftCoef0, GiftCoef),
    to_currency(CountCoef0, CountCoef),
    !.

% to_currency(+NumIn, -NumOut)
to_currency(NumIn, NumOut) :-
    to_currency(NumIn, NumOut, 4),
    !.
% to_currency(+NumIn, -NumOut, +Round)
to_currency(NumIn, NumOut, Round) :-
    number(NumIn), integer(Round),
    NumOut is float( round( NumIn * (10 ** Round) ) / (10 ** Round) ),
    !.

% xo_rate_extra(Cost, ModeLevel, CompMark, NormalMark, Coor, Extra)
xo_rate_extra(2, ModeLevel, CompMark, NormalMark, X-Y, Extra) :-
    ModeLevel >= 9,
    ( CompMark = NormalMark -> ExtraMode = normal ; ExtraMode = echo ),
    xo_mark_cell(ExtraMode, X, Y),
    %
    findall( RuleName-Value,
             ( xo_rule(RuleName, RuleOpt),
               memberchk(kind(heuristic), RuleOpt),
               memberchk(value(Value), RuleOpt)
             ),
             Rules
           ),
    findall( Value-1,
             ( member(RuleName-Value, Rules),
               PlayCell = cell(_, CompMark),
               xo_play(ExtraMode, PlayCell, RuleName-_)
             ),
             ExtraList
           ),
    sum_int_pairs(ExtraList, Extra),
    %
    xo_unmark_cell(X, Y),
    !.
xo_rate_extra(_, _, _, _, _, 0-0).

% xo_fork_extra(ModeLevel, NormalMark, Fork, Extra, OwnRuleName)
xo_fork_extra(ModeLevel, NormalMark, Fork, Extra, OwnRuleName) :-
    ModeLevel >= 8,
    %
    FreeCell = cell(X-Y, n),
    Fork = fork(_, _, _, _, Mark, FreeCell),
    ( Mark = NormalMark -> ForkMode = normal ; ForkMode = echo ),
    xo_mark_cell(ForkMode, X, Y),
    %
    findall( RuleName-Value,
             ( xo_rule(RuleName, RuleOpt),
               memberchk(kind(heuristic), RuleOpt),
               \+ RuleName = OwnRuleName,
               memberchk(value(Value), RuleOpt)
             ),
             Rules
           ),
    findall( Value-1,
             ( member(RuleName-Value, Rules),
               PlayCell = cell(_, Mark),
               xo_play(ForkMode, PlayCell, RuleName-_)
             ),
             ExtraList
           ),
    sum_int_pairs(ExtraList, Extra),
    %
    xo_unmark_cell(X, Y),
    !.
xo_fork_extra(_, _, _, 0-0, _).

% xo_limit_coor(WinLength, LimitData)
xo_limit_coor(WinLength, LimitData) :-
    xo_limit_coor(WinLength, LimitData, _PlaySpace).
% xo_limit_coor(WinLength, LimitData, PlaySpace)
xo_limit_coor(WinLength, LimitData, PlaySpace) :-
    LimitData = [MinX, MaxX, MinY, MaxY],
    PlaySpace = [MinX0, MaxX0, MinY0, MaxY0],
    findall( X, ( xo_cell(X-_, Mark), \+ Mark = n ), XList ),
    findall( Y, ( xo_cell(_-Y, Mark), \+ Mark = n ), YList ),
    min_list(XList, MinX0),
    max_list(XList, MaxX0),
    min_list(YList, MinY0),
    max_list(YList, MaxY0),
    WinLength1 is -WinLength,
    plus(MinX0, WinLength1, MinX),
    plus(MaxX0, WinLength, MaxX),
    plus(MinY0, WinLength1, MinY),
    plus(MaxY0, WinLength, MaxY),
    !.

% xo_check_coor(Coor, LimitData)
xo_check_coor(Coor, LimitData) :-
    Coor = X-Y,
    LimitData = [MinX, MaxX, MinY, MaxY],
    between(MinX, MaxX, X),
    between(MinY, MaxY, Y),
    %X >= MinX, X =< MaxX,
    %Y >= MinY, Y =< MaxY,
    !.

% xo_random_free_cell(Mode, PlayCell, RuleName-Rule)
xo_random_free_cell(Mode, PlayCell, RuleName-Rule) :-
    RuleName = random_free_cell,
    xo_mode_valid_rule(Mode, RuleName, _WinLength, _ModeLevel, _ModeGo),
    findall( Coor,
             xo_cell(Coor, n),
             FreeCoorList
    ),
    \+ FreeCoorList = [],
    length(FreeCoorList, Len),
    Index is random(Len),
    nth0(Index, FreeCoorList, Coor),
    PlayCell = cell(Coor, _),
    Rule = rule(RuleName,length=Len,index=Index),
    true.

% шаблон ранга
% xo_rate_shape(RateShape, Method-Rate)
xo_rate_shape(RateShape, Method-Rate) :-
    RateShape = [TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount],
    List = [
        1-rate(TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount),
        2-rate(TotalGift, TotalCount, UserGift, CompGift, UserCount, CompCount),
        3-rate(TotalCount, TotalGift, CompCount, UserCount, CompGift, UserGift),
        4-rate(TotalCount, TotalGift, UserCount, CompCount, UserGift, CompGift),
        5-rate(TotalGift, TotalCount, CompCount, UserCount, CompGift, UserGift),
        6-rate(TotalGift, TotalCount, UserCount, CompCount, UserGift, CompGift),
        7-rate(TotalCount, TotalGift, CompGift, UserGift, CompCount, UserCount),
        8-rate(TotalCount, TotalGift, UserGift, CompGift, UserCount, CompCount),
        -
    ],
    ( ground(Method),
      memberchk(Method-Rate, List)
     -> true
    ; length(List, Len),
      catch( Index is random(Len - 1), _, Index = 0 ),
      nth0(Index, List, Method-Rate) ),
    !.
/* by Copilot Smart (GPT-5)
Method  Архетип‑роль 🎭 Смещение акцента    Типичная драматургия хода
1   «Главный бухгалтер» TotalGift вперёд, приоритет ценности трофея Методично усиливает материальный перевес, даже в ущерб темпу.
2   «Альтруист‑провокатор»  Смотрит на ценность, но глазами оппонента (UserGift вперёд) Способен подыграть, чтобы вынудить соперника раскрыться.
3   «Игрок в кости» TotalCount вперёд, ставка на количество, а не качество  Ловит удачу на множестве слабых угроз — шанс‑всплески.
4   «Зеркало темпа» Считает шаги оппонента прежде своих Играет в перехват ритма, иногда жертвуя выгодой ради сброса темпа.
5   «Полевой командир»  Дар/счёт соперника первыми, свой ответ вторым   Концентрируется на ответных ударах, играет от реакции.
6   «Зеркальный аналитик»   Ведёт сравнение счёта сторон (User→Comp) симметрично    Строит партии как шахматист, удерживающий баланс для контратаки.
7   «Контрразведчик»    Первым видит трофеи противника, но в свете своих счётов Предугадывает, где соперник будет усиливаться, и режет пути.
8   «Перехватчик шагов» Первым считает чужие ходы, затем оценивает свои Любит ломать комбо оппонента на ранней стадии цикла.
*/

% ранг ячейки
% xo_rate(Mark, X, Y, Cost, Gift, Count)
xo_rate(Mark, X, Y, Cost, Gift, Count) :-
    xo_rate(Mark, X-Y, Cost, Gift-Count).
% xo_rate(Mark, Coor, Cost, Rate)
xo_rate(Mark, Coor, Cost, GiftCoef-CountCoef) :-
    ground([Mark, Coor, Cost]),
    once( xo_cell_id(ID, Coor) ),
    xo_params(Params),
    memberchk(line(WinLength), Params),
    memberchk(max_solve_qty(MaxSolveQty), Params),
    findall( MarkedQty-1,
             ( once( xo_cell_solves(ID, _, CellSolves) ),
               member(Solve_ID, CellSolves),
               xo_has_chance(Mark, Solve_ID, MarkedQty),
               MarkedQty >= Cost
             ),
             MarkedQtyList
    ),
    sum_int_pairs(MarkedQtyList, Gift-Count),
    GiftCoef0 is Gift / MaxSolveQty * WinLength,
    CountCoef0 is Count / MaxSolveQty,
    to_currency(GiftCoef0, GiftCoef),
    to_currency(CountCoef0, CountCoef),
    !.

% sum_int_pairs(Pairs, SumPairs)
sum_int_pairs(Pairs, SumPairs) :-
    sum_int_pairs(Pairs, 0-0, SumPairs),
    !.
% sum_int_pairs(Pairs, SumPairs0, SumPairs)
sum_int_pairs([], SumPairs, SumPairs).
sum_int_pairs([X1-X2|Xs], Sum01-Sum02, SumPairs) :-
    plus(Sum01, X1, Sum11),
    plus(Sum02, X2, Sum12),
    sum_int_pairs(Xs, Sum11-Sum12, SumPairs).

% есть вилка
% xo_has_fork(MarkedSolveList, Fork)
xo_has_fork([MarkedQty-Order-Mark-Solve | TeilSolves], Fork) :-
    Fork = fork(ForkHeight, ForkPower, ForkWidth, ForkOrder, Mark, FreeCell),
    FreeCell = cell(_, n),
    %check_point,
    member(FreeCell, Solve),
    member(ClaimOrder, [1, 0]),
    findall( ForkMarkedQty,
             ( member(ForkMarkedQty-ClaimOrder-_-ForkSolve, TeilSolves),
               select(FreeCell, ForkSolve, ForkSolveRest),
               \+ ( member(ForkCell, ForkSolveRest),
                    memberchk(ForkCell, Solve)
                  )
             ),
             ForkMarkedQtyList
    ),
    \+ ForkMarkedQtyList = [],
    %check_point,
    max_list([MarkedQty | ForkMarkedQtyList], MaxMarkedQty),
    succ(MaxMarkedQty, ForkHeight),
    sum_list([MarkedQty | ForkMarkedQtyList], ForkPower),
    length([MarkedQty | ForkMarkedQtyList], ForkWidth),
    memberchk(Order-ClaimOrder-ForkOrder, [1-1-2, 0-0-1, 1-0-0, 0-1-0]).
xo_has_fork([_ | TeilSolves], Fork) :-
    xo_has_fork(TeilSolves, Fork).

% оценка ситуации
% xo_review(Mark, X, Y, Cost, OutMark, OutX, OutY)
xo_review(Mark, X, Y, Cost, OutMark, OutX, OutY) :-
    xo_has_chance(Mark, Solve_ID, MarkedQty),
    MarkedQty >= Cost,
    xo_solve(Solve_ID, Solve, _),
    selectchk(cell(X-Y, n), Solve, Review),
    member(cell(OutX-OutY, OutMark), Review).

% отметка ячейки
% xo_mark_cell(Mode, X, Y)
xo_mark_cell(Mode, X, Y) :-
    xo_params(Params),
    memberchk(go(CompMark, UserMark), Params),
    memberchk(Mode-Mark, [normal-CompMark, echo-UserMark]),
    %
    xo_mark_cell_ext(Mark, X, Y),
    %
    ( xo_step(_, Step, _, _) -> true ; Step = 0 ),
    succ(Step, Step1),
    asserta( xo_step(Mark, Step1, X, Y) ),
    !.

% очистка ячейки
% xo_unmark_cell(X, Y)
xo_unmark_cell(X, Y) :-
    Mark = n,
    %
    xo_mark_cell_ext(Mark, X, Y),
    %
    retract( xo_step(_Mark, _Step, X, Y) ),
    !.

xo_mark_cell_ext(Mark, X, Y) :-
    once( xo_cell_id(ID, X-Y) ),
    once( xo_cell_solves(ID, _, CellSolves) ),
    %
    forall( member(Solve_ID, CellSolves),
            ( xo_get_solve_state(Solve_ID, State),
              xo_solve(Solve_ID, Solve, State),
              xo_change_solve_state(Solve, X-Y, Mark, State, ChangedState),
              xo_set_solve_state(Solve_ID, ChangedState)
            )
    ),
    xo_set_cell(ID, X-Y, Mark),
    !.

% смена состояния для решения
% xo_change_solve_state(Solve, Cell, Mark, State, ChangedState)
xo_change_solve_state([cell(X-Y, OldMark) | _], X-Y, Mark, State, ChangedState) :-
    xo_change_state(OldMark, Mark, State, ChangedState),
    !.
xo_change_solve_state([_ | TeilSolve], Cell, Mark, State, ChangedState) :-
    !,
    xo_change_solve_state(TeilSolve, Cell, Mark, State, ChangedState).

% xo_change_state(OldMark, Mark, State, ChangedState)
xo_change_state(OldMark, Mark, State, ChangedState) :-
    State = [x-X, o-O, n-N] / _HasChanceMark,
    xo_change_state(OldMark, Mark, X, O, N, X1, O1, N1, HasChanceMark1),
    ChangedState = [x-X1, o-O1, n-N1] / HasChanceMark1,
    !.

xo_change_state(n, x, X, O, N, X1, O1, N1, H) :-
    succ(X, X1),
    plus(N, -1, N1),
    O1 = O,
    ( O1 =:= 0, H = x ; H = z ),
    !.
xo_change_state(n, o, X, O, N, X1, O1, N1, H) :-
    succ(O, O1),
    plus(N, -1, N1),
    X1 = X,
    ( X1 =:= 0, H = o ; H = z ),
    !.
xo_change_state(x, n, X, O, N, X1, O1, N1, H) :-
    succ(N, N1),
    plus(X, -1, X1),
    O1 = O,
    ( O1 =:= 0, \+ X1 =:= 0, H = x ; H = n ),
    !.
xo_change_state(o, n, X, O, N, X1, O1, N1, H) :-
    succ(N, N1),
    plus(O, -1, O1),
    X1 = X,
    ( X1 =:= 0, \+ O1 =:= 0, H = o ; H = n ),
    !.

% шаг назад
% xo_back
xo_back :-
    xo_back(_Mark, _Step, _X, _Y).
% xo_back(Mark, Step, X, Y)
xo_back(Mark, Step, X, Y) :-
    retract( xo_step(Mark, Step, X, Y) ),
    xo_unmark_cell(X, Y),
    asserta( xo_step_back(Mark, Step, X, Y) ),
    !.

% шаг вперед
% xo_forth
xo_forth :-
    xo_forth(_Mark, _Step, _X, _Y).
% xo_forth(Mark, Step, X, Y)
xo_forth(Mark, Step, X, Y) :-
    retract( xo_step_back(Mark, Step, X, Y) ),
    xo_params(Params),
    memberchk(go(CompMark, UserMark), Params),
    memberchk(Mode-Mark, [normal-CompMark, echo-UserMark]),
    xo_mark_cell(Mode, X, Y),
    !.

% взять параметры игры
% xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark)
xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark) :-
    xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark, _).
% xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark, ModeOptStr)
xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark, ModeOptStr) :-
    xo_params( [
        size(PosBegin, PosEnd),
        line(WinLength),
        level(Level),
        go(CompMark, UserMark),
        ModeOpt
    ] ),
    term_to_atom(ModeOpt, ModeOptAtom),
    atom_string(ModeOptAtom, ModeOptStr).

% установить параметры игры
% xo_set_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark)
xo_set_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark) :-
    xo_set_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark, "").
% xo_get_params(PosBegin, PosEnd, WinLength, Level, CompMark, ModeOptStr)
xo_set_params(PosBegin, PosEnd, WinLength, Level, CompMark, UserMark, ModeOptStr) :-
    ( ModeOptStr = ""
     ->
      ( xo_params([_, _, _, _, ModeOpt0]),
        ModeOpt = ModeOpt0
      ; ModeOpt = []
      )
    ; atom_string(ModeOptAtom, ModeOptStr),
      term_to_atom(ModeOpt, ModeOptAtom)
    ),
    retractall( xo_params(_) ),
    assertz(
        xo_params( [
            size(PosBegin, PosEnd),
            line(WinLength),
            level(Level),
            go(CompMark, UserMark),
            ModeOpt
        ] )
    ).

% инициализация
% xo_init
xo_init :-
    xo_make_cells,
    xo_make_solves,
    retractall( xo_step(_, _, _, _) ),
    retractall( xo_step_back(_, _, _, _) ),
    !.

% тест
% xo_test
xo_test :-
    Count = 10,
    xo_test(Count).
% xo_test(Count)
xo_test(Count) :-
    %set_prolog_flag(gc, false),
    between(1, Count, Value),
    time( xo_test(Result, Solve) ),
    %xo_test(Result, Solve),
    %xo_test(Result, Solve),
    once( xo_step(Mark, Step, _, _) ),
    writeln(game_over(Value, Result, Mark, Step, Solve)),
    Value = Count,
    !.
% xo_test(Result, Solve)
xo_test(Result, Solve) :-
    xo_init,
    xo_params(Params),
    memberchk(go(CompMark, UserMark), Params),
    memberchk(size(PosBegin, PosEnd), Params),
    %retractall( xo_step(_, _, _, _) ),
    PlayCell = cell(X-Y, _),
    %MaxStep = 35,
    MaxStep is round( float_integer_part( (PosEnd - PosBegin + 1) ^ 2 / 2 * sign(PosEnd - PosBegin + 1) ) ),
    between(1, MaxStep, _),
    member(Mode-Mark, [normal-CompMark, echo-UserMark]),
    ( xo_play_in(Mode-Mark, PlayCell, _-Rule)
     -> true
    ; time( once( xo_play(Mode, PlayCell, _-Rule) ) )
    %; once( xo_play(Mode, PlayCell, _-Rule) )
    ),
    PlayCell = cell(X-Y, _),
    %MarkCell = cell(X-Y, n),
    xo_mark_cell(Mode, X, Y),
    %
    once( xo_cell_id(ID, X-Y) ),
    once( xo_cell_state(ID, _, Ver, _) ),
    %
    once( xo_step(Mark, Step, X, Y) ),
    writeln(step(Step, Mark, X, Y, ver(Ver))-Rule),
    %statistics(localused, L),
    %statistics(globalused, G),
    %statistics(trailused, T),
    %writeln([localused-L, globalused-G, trailused-T]),
    %check_point,
    ( xo_win(Mode, Mark, Solve)
     ->
      Result = Mode
    ; xo_tie(Mode),
      Result = none,
      Solve = none
    ),
    !.

% ручной ввод координат
% xo_play_in(Mode-Mark, PlayCell, RuleName-Rule)
xo_play_in(Mode-Mark, cell(X-Y,_), input-rule(input)) :-
    fail, % disabled
    write(Mode-Mark),
    write(': '),
    read(In),
    In = X-Y.

% правила
% xo_rule(RuleName, RuleLevel, RuleKind, RuleDesc)
xo_rule(RuleName, RuleLevel, RuleKind, RuleDesc) :-
    xo_rule(RuleName, RuleOpt),
    memberchk(level(RuleLevel), RuleOpt),
    memberchk(kind(RuleKind), RuleOpt),
    memberchk(desc(RuleDesc), RuleOpt).
% xo_rule(RuleName, RuleOpt)
xo_rule(tie_by_chance, [
            level(3),
            kind(game),
            desc("ничья по шансам на выигрыш")
                       ]).
xo_rule(random_best_pos, [
            level(3),
            kind(rate),
            desc("случайный выбор из лучших позиций")
                     ]).
xo_rule(random_best_chance, [
            level(3),
            kind(rate),
            desc("случайный выбор из лучших шансов на выигрыш")
                     ]).
xo_rule(next_step_win, [
            level(1),
            kind(heuristic),
            value(8),
            desc("выигрыш следующим ходом")
                       ]).
xo_rule(free_border, [
            level(5),
            kind(heuristic),
            value(4),
            desc("свободные края (выигрыш через ход)")
                       ]).
xo_rule(dash_mark, [
            level(6),
            kind(heuristic),
            value(4),
            desc("тире (выигрыш через ход)")
                       ]).
xo_rule(fork, [
            level(7),
            kind(heuristic),
            value(1),
            desc("вилка")
                       ]).
xo_rule(random_chance, [
            level(2),
            kind(random),
            desc("случайный выбор из шансов на выигрыш")
                       ]).
xo_rule(random_free_cell, [
            level(0),
            kind(random),
            desc("случайный выбор свободной ячейки")
                       ]).

%
check_point.

%
:- if(\+ current_predicate(between/3)).
between(X, Y, Z) :-
    num_gen(X, Y, Z).
:- endif.

num_gen(X, _, X).
num_gen(X, Y, Z) :-
    X < Y,
    succ(X, X1),
    num_gen(X1, Y, Z).
 %
%%


