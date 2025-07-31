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
:- dynamic([xo_solve_cells/2, xo_solve_state/7, xo_cell_solves/3]).
:- dynamic([xo_step/4, xo_step_back/4 ]).

% параметры игры
% xo_params([Size, Line, Level, Go, ModeOpt])
%   Size = size(PosBegin, PosEnd) - размер игрового поля
%   Line = line(WinLength) - длина линии выигрыша
%   Level = level(Level) - уровень игры
%   Go = go(CompMark, UserMark) - отметки хода
%   ModeOpt - опции режима
xo_params( [
    size(0, 19),
    line(5),
    level(4),
    go(x, o),
    mode_opt([
              level(echo, +5),
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
                             free_border,
                             dash_mark,
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
    xo_cell_id(ID, X-Y),
    once( xo_cell_state(ID, StateMark, Ver, TimeStamp) ),
    Mark = StateMark,
    true.

xo_set_cell(ID, X-Y, Mark) :-
    ( ground(ID) -> true ; xo_cell_id(ID, X-Y) ),
    xo_cell_state(ID, _, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_cell_state(ID, Mark, Ver1, TimeStamp) ),
    !.

% пространство решений
% xo_solve(Solve, State/HasChanceMark)
%   Solve = [ cell(Coor, Mark) | _ ]
%   Coor = X-Y
%   Mark = {x ; o ; n}
%   State = [x-Qty1, o-Qty2, n-Qty3]
%   HasChanceMark
xo_solve(Solve, State) :-
    xo_solve(_Solve_ID, Solve, State).
xo_solve(Solve_ID, Solve, State) :-
    ( ground(Solve_ID),
      xo_solve_cells(Solve_ID, SolveCells)
     -> true
    ; xo_solve_cells(Solve_ID, SolveCells) ),
    findall( cell(X-Y, Mark),
             ( member(ID, SolveCells),
               once( xo_cell_id(ID, X-Y) ),
               once( xo_cell_state(ID, Mark, _, _) )
             ),
             Solve
    ),
    xo_get_solve_state(Solve_ID, State),
    true.

xo_get_solve_state(ID, State) :-
    xo_get_solve_state(ID, State, _Ver, _TimeStamp).
xo_get_solve_state(ID, State, Ver) :-
    xo_get_solve_state(ID, State, Ver, _TimeStamp).
xo_get_solve_state(ID, State, Ver, TimeStamp) :-
    once( xo_solve_state(ID, HasChanceMark, X, O, N, Ver, TimeStamp) ),
    State = [x-X1, o-O1, n-N1] / HasChanceMark1,
    HasChanceMark = HasChanceMark1, X = X1, O = O1, N = N1,
    !.

xo_set_solve_state(ID, State) :-
    State = [x-X, o-O, n-N] / HasChanceMark,
    xo_solve_state(ID, _HasChanceMark, _X, _O, _N, Ver, _TimeStamp),
    succ(Ver, Ver1),
    get_time(TimeStamp),
    asserta( xo_solve_state(ID, HasChanceMark, X, O, N, Ver1, TimeStamp) ),
    !.

% xo_cell_id(Cell_ID, X-Y)
% xo_cell_state(Cell_ID, Mark, Ver, TimeStamp)

% формирование пространства ячеек
% xo_make_cells
xo_make_cells :-
    xo_params(Params),
    memberchk(size(PosBegin, PosEnd), Params),
    xo_gen_cells(PosBegin, PosEnd).

xo_gen_cells(PosBegin, PosEnd) :-
    Ps = [xo_cell_id/2, xo_cell_state/4],
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
% xo_solve_moves(SolveMoves)
%   SolveMoves = [ move(DeltaX, DeltaY) | _ ]
xo_solve_moves([move(1, 0), move(-1, 0)]).    % горизонталь
xo_solve_moves([move(0, 1), move(0, -1)]).    % вертикаль
xo_solve_moves([move(1, 1), move(-1, -1)]).   % диагональ1
xo_solve_moves([move(1, -1), move(-1, 1)]).   % диагональ2

% xo_solve_cells(Solve_ID, [Cell_ID_1, ..., Cell_ID_WinLength])
% xo_solve_state(Solve_ID, HasChanceMark, X, O, N, Ver, TimeStamp)
% xo_cell_solves(Cell_ID, SolveQty, [Solve_ID_1, ..., Solve_ID_K])

% формирование пространства решений
% xo_make_solves
xo_make_solves :-
    Ps = [xo_solve_cells/2, xo_solve_state/7, xo_cell_solves/3],
    dynamic(Ps),
    forall( member(P, Ps), abolish(P) ),
    dynamic(Ps),
    xo_params(Params),
    memberchk(line(WinLength), Params),
    xo_cell_id(_Cell_ID, X-Y),
    xo_solve_moves(Moves),
    xo_collect_solve(Moves, X-Y, X-Y, WinLength, [X-Y], SolveCells),
    \+ xo_solve_cells(_, SolveCells),
    ( xo_solve_cells(ID, _), succ(ID, ID1)  -> true ; ID1 = 1),
    asserta( xo_solve_cells(ID1, SolveCells) ),
    get_time(TimeStamp),
    asserta( xo_solve_state(ID1, n, 0, 0, WinLength, 0, TimeStamp) ),
    fail.
xo_make_solves :-
    xo_cell_id(ID, _),
    findall( Solve_ID,
             ( xo_solve_cells(Solve_ID, SolveCells), memberchk(ID, SolveCells) ),
             CellSolves
    ),
    length(CellSolves, SolveQty),
    asserta( xo_cell_solves(ID, SolveQty, CellSolves) ),
    fail.
xo_make_solves :-
    once( xo_solve(_, _) ),
    Ps = [xo_solve_cells/2, xo_cell_solves/3],
    %compile_predicates(Ps),
    !.

% собрать решение по ячейке
% xo_collect_solve(Moves, BeginCoor, CurrentCoor, WinLength, SolveCoors, SolveCells)
xo_collect_solve(_, _, _, WinLength, SolveCoors, SolveCells) :-
    length(SolveCoors, WinLength),
    sort(SolveCoors, SortedSolveCoors),
    findall( ID,
             ( member(Coor, SortedSolveCoors), once( xo_cell_id(ID, Coor) ) ),
             SolveCells
    ),
    !.
xo_collect_solve([Move | Moves], BeginCoor, X-Y, WinLength, SolveCoors, SolveCells) :-
    Move = move(DeltaX, DeltaY),
    plus(X, DeltaX, X1),
    plus(Y, DeltaY, Y1),
    xo_cell(X1-Y1, _),
    SolveCoors1 = [X1-Y1 | SolveCoors],
    !,
    xo_collect_solve([Move | Moves], BeginCoor, X1-Y1, WinLength, SolveCoors1, SolveCells).
xo_collect_solve([_ | Moves], BeginCoor, _, WinLength, SolveCoors, SolveCells) :-
    !,
    xo_collect_solve(Moves, BeginCoor, BeginCoor, WinLength, SolveCoors, SolveCells).

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
    ( ground(Solve_ID) -> true ; xo_solve_cells(Solve_ID, _SolveCells) ),
    once( xo_solve_state(Solve_ID, HasChanceMark, X, O, N, _Ver, _TimeStamp) ),
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
               plus(CompCount, UserCount, TotalCount),
               TotalCount > 0,
               plus(CompGift, UserGift, TotalGift),
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
% xo_rate_shape(RateShape, MethodRate)
xo_rate_shape(RateShape, MethodRate) :-
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
    length(List, Len),
    catch( Index is random(Len - 1), _, Index = 0 ),
    nth0(Index, List, MethodRate),
    !.

% ранг ячейки
% xo_rate(Mark, X, Y, Cost, Gift, Count)
xo_rate(Mark, X, Y, Cost, Gift, Count) :-
    xo_rate(Mark, X-Y, Cost, Gift-Count).
% xo_rate(Mark, Coor, Cost, Rate)
xo_rate(Mark, Coor, Cost, Gift-Count) :-
    once( xo_cell_id(ID, Coor) ),
    once( xo_cell_solves(ID, _SolveQty, _) ),
    %WinLength = 5,
    findall( MarkedQty-1,
             ( once( xo_cell_solves(ID, _, CellSolves) ),
               member(Solve_ID, CellSolves),
               xo_has_chance(Mark, Solve_ID, MarkedQty),
               MarkedQty >= Cost
             ),
             MarkedQtyList
    ),
    sum_int_pairs(MarkedQtyList, Gift-Count),
    %GiftCoef is Gift / SolveQty * WinLength,
    %CountCoef is Count / SolveQty,
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
    ( X1 =:= 0, \+ O1 =:= 0, H = 0 ; H = n ),
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
    once( xo_step(Mark, Step, X, Y) ),
    writeln(step(Step, Mark, X, Y)-Rule),
    statistics(localused, L),
    statistics(globalused, G),
    statistics(trailused, T),
    writeln([localused-L, globalused-G, trailused-T]),
    once( xo_cell_id(ID, X-Y) ),
    once( xo_cell_state(ID, M, V, TS) ),
    writeln([ID, M, V, TS]),
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


