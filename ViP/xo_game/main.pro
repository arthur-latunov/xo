% Copyright AR
% xo_game

implement main
open core, list, math

constants
tie_by_chance : symbol = "tie_by_chance".
random_best_pos : symbol = "random_best_pos".
random_best_chance : symbol = "random_best_chance".
next_step_win : symbol = "next_step_win".
free_border : symbol = "free_border".
dash_mark : symbol = "dash_mark".
fork : symbol = "fork".
random_chance : symbol = "random_chance".
random_free_cell : symbol = "random_free_cell".
none : symbol = "none".
game : symbol = "game".
rate : symbol = "rate".
heuristic : symbol = "heuristic".
random : symbol = "random".

domains
go = go(symbol, symbol).
xo_param =
    size(integer, integer);
    line(integer);
    level(integer, integer);
    go(symbol, symbol);
    mode_opt(mode_opt*).
mode_opt =
    rules(symbol, symbol*);
    empty.

class facts
% параметры игры
% xo_params([Size, Line, Level, Go, ModeOpt])
%   Size = size(PosBegin, PosEnd) - размер игрового поля
%   Line = line(WinLength) - длина линии выигрыша
%   Level = level(NormalLevel, EchoLevel) - уровень игры
%   Go = go(MarkX, MarkO) - отметки хода
%   ModeOpt - опции режима
xo_params : (xo_param* Params).

clauses
xo_params([
            size(0, 19),
            line(5),
            level(4, 9),
            go(x, o),
            mode_opt(
                [
                    rules(normal,
                        [
                            tie_by_chance,
                            random_best_pos,
                            random_best_chance,
                            next_step_win,
                            free_border,
                            dash_mark,
                            fork,
                            random_chance,
                            random_free_cell,
                            none
                        ]),
                    rules(echo,
                        [
                            tie_by_chance,
                            random_best_pos,
                            random_best_chance,
                            next_step_win,
                            free_border,
                            dash_mark,
                            fork,
                            random_chance,
                            random_free_cell,
                            none
                        ]),
                    empty
                ])
        ]).

clauses
% взять размер игрового поля
% xo_get_size(PosBegin, PosEnd)
xo_get_size(PosBegin, PosEnd) :-
    xo_params(Params),
    size(PosBegin, PosEnd) in Params,
    !.
xo_get_size(0, 19).

% установить размер игрового поля
% xo_set_size(PosBegin, PosEnd)
xo_set_size(PosBegin, PosEnd) :-
    retract( xo_params(Params) ),
    size(PosBegin0, PosEnd0) in Params,
    select_param(size(PosBegin0, PosEnd0), Params, Params1),
    assertz( xo_params([size(PosBegin, PosEnd) | Params1]) ),
    !.

% взять уровни из параметров игры
% xo_get_levels(NormalLevel, EchoLevel)
xo_get_levels(NormalLevel, EchoLevel) :-
    xo_params(Params),
    level(NormalLevel, EchoLevel) in Params,
    !.
xo_get_levels(4, 9).

% установить уровни в параметрах игры
% xo_set_levels(NormalLevel, EchoLevel)
xo_set_levels(NormalLevel, EchoLevel) :-
    retract( xo_params(Params) ),
    level(NormalLevel0, EchoLevel0) in Params,
    select_param(level(NormalLevel0, EchoLevel0), Params, Params1),
    assertz( xo_params([level(NormalLevel, EchoLevel) | Params1]) ),
    !.

% взять данные по ячейке
xo_get_cell(Coor, Mark) :-
    xo_cell(Coor, Mark).

class facts
% пространство ячеек
% xo_cell(Coor, Mark)
%   Coor = coor(X, Y)
%   Mark = {x ; o ; n}
xo_cell : (coor Coor, symbol Mark).
% пространство решений
% xo_solve(Solve, State, HasChanceMark)
%   Solve = [ cell(Coor, Mark) | _ ]
%   Coor = coor(X, Y)
%   Mark = {x ; o ; n}
%   State = [state(x, Qty1), state(o, Qty2), state(n, Qty3)]
%   HasChanceMark = {x ; o ; n ; z}
%     x - only x and n
%     o - only o and n
%     n - none marks
%     z - zero chance (x and o marks exists in solve)
xo_solve : (cell* Solve, state* State, symbol HasChanceMark).
xo_step : (symbol Mark, integer Step, coor Coor).
xo_step_back : (symbol Mark, integer Step, coor Coor).

class predicates
append_solve : (cell* LeftPart, cell* RightPart, cell* Solve) nondeterm anyflow.
select_solve : (cell Cell, cell* Solve, cell* SolveRest) nondeterm (i,i,o).
select_param : (xo_param Param, xo_param* ParamList, xo_param* ParamListRest) nondeterm (i,i,o).

clauses
append_solve([],L,L).
append_solve([H|T],L,[H|R]):-
    append_solve(T,L,R).

select_solve(Elem, [Elem | Tail], Tail).
select_solve(Elem, [Head | Tail], [Head | Rest]) :-
    select_solve(Elem, Tail, Rest).

select_param(Elem, [Elem | Tail], Tail).
select_param(Elem, [Head | Tail], [Head | Rest]) :-
    select_param(Elem, Tail, Rest).

xo_step_once(Mark, Step, Coor) :-
    xo_step(Mark, Step, Coor),
    !.

xo_step_back_once(Mark, Step, Coor) :-
    xo_step_back(Mark, Step, Coor),
    !.

domains
move = move(integer DeltaX, integer DeltaY).

class predicates
% пространство движений для поиска решения
% xo_solve_moves(SolveMoves)
%   SolveMoves = [ move(DeltaX, DeltaY) | _ ]
xo_solve_moves : (move* SolveMoves [out]) multi.

clauses
xo_solve_moves([move(1, 0), move(-1, 0)]).    % горизонталь
xo_solve_moves([move(0, 1), move(0, -1)]).    % вертикаль
xo_solve_moves([move(1, 1), move(-1, -1)]).   % диагональ1
xo_solve_moves([move(1, -1), move(-1, 1)]).   % диагональ2

class predicates
xo_make_cell : () determ.
xo_make_solve : () determ.
xo_cell_solve :
    (move* Moves, coor BeginCoor, coor CurrentCoor, integer WinLength,
     cell* Solve0, cell* Solve [out])
    nondeterm.

clauses
% формирование пространства ячеек
% xo_make_cell
xo_make_cell() :-
    retractall( xo_cell(_, _) ),
    xo_params(Params),
    size(PosBegin, PosEnd) in Params,
    X = std::between(PosBegin, PosEnd),
    Y = std::between(PosBegin, PosEnd),
    assertz( xo_cell(coor(X, Y), n) ),
    fail.
xo_make_cell() :-
    !,
    xo_cell(_, _),
    !.

% формирование пространства решений
% xo_make_solve
xo_make_solve() :-
    retractall( xo_solve(_, _, _) ),
    xo_params(Params),
    line(WinLength) in Params,
    xo_cell(Coor, _),
    xo_solve_moves(Moves),
    xo_cell_solve(Moves, Coor, Coor, WinLength, [cell(Coor, n)], Solve),
    not( xo_solve(Solve, _, _) ),
    State = [state(x, 0), state(o, 0), state(n, WinLength)],
    assertz( xo_solve(Solve, State, n) ),
    fail.
xo_make_solve() :-
    !,
    xo_solve(_, _, _),
    !.

% шаблон решения по ячейке
% xo_cell_solve(Moves, BeginCoor, CurrentCoor, WinLength, Solve0, Solve)
xo_cell_solve(_, _, _, WinLength, Solve, SortedSolve) :-
    WinLength = length(Solve),
    SortedSolve = sort(Solve),
    !.
xo_cell_solve([Move | Moves], BeginCoor, coor(X, Y), WinLength, Solve0, Solve) :-
    Move = move(DeltaX, DeltaY),
    X1 = X + DeltaX,
    Y1 = Y + DeltaY,
    xo_cell(coor(X1, Y1), _),
    Solve1 = [cell(coor(X1, Y1), n) | Solve0],
    !,
    xo_cell_solve([Move | Moves], BeginCoor, coor(X1, Y1), WinLength, Solve1, Solve).
xo_cell_solve([_ | Moves], BeginCoor, _, WinLength, Solve0, Solve) :-
    !,
    xo_cell_solve(Moves, BeginCoor, BeginCoor, WinLength, Solve0, Solve).

class predicates
xo_mode_valid_rule :
    (symbol Mode, symbol RuleName, integer WinLength [out], integer ModeLevel [out], go ModeGo [out])
    determ.
xo_mode_level :
    (symbol Mode, integer NormalLevel, integer EchoLevel, integer ModeLevel [out])
    determ.
xo_mode_go :
    (symbol Mode, go Go, go ModeGo)
    determ anyflow.

clauses
% xo_mode_valid_rule(+Mode, +RuleName, -WinLength, -ModeLevel, -ModeGo)
xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, OutModeGo) :-
    xo_params(Params),
    mode_opt(ModeOpt) in Params,
    rules(Mode, Rules) in ModeOpt,
    RuleName in Rules,
    level(NormalLevel, EchoLevel) in Params,
    xo_mode_level(Mode, NormalLevel, EchoLevel, ModeLevel),
    xo_rule(RuleName, RuleOpt),
    level(RuleLevel) in RuleOpt,
    ModeLevel >= RuleLevel,
    line(WinLength) in Params,
    go(Mark1, Mark2) in Params,
    InModeGo = go(Mark1, Mark2),
    xo_mode_go(Mode, InModeGo, OutModeGo),
    !.

% xo_mode_level(Mode, NormalLevel, EchoLevel, ModeLevel)
xo_mode_level(normal, NormalLevel, _EchoLevel, NormalLevel).
xo_mode_level(echo, _NormalLevel, EchoLevel, EchoLevel).

% xo_mode_go(Mode, Go, ModeGo)
xo_mode_go(echo, InModeGo, OutModeGo) :-
    bound(InModeGo),
    InModeGo = go(Mark1, Mark2),
    OutModeGo = go(Mark2, Mark1),
    !.
xo_mode_go(echo, InModeGo, OutModeGo) :-
    bound(OutModeGo),
    OutModeGo = go(Mark2, Mark1),
    InModeGo = go(Mark1, Mark2),
    !.
xo_mode_go(normal, ModeGo, ModeGo).

class predicates
xo_has_chance : (symbol Mark, cell* Solve, integer MarkedQty) nondeterm (i,o,o) (i,o,i).

clauses
% есть шанс для выигрыша
% xo_has_chance(PlayMark, Solve, MarkedQty)
xo_has_chance(Mark, Solve, MarkedQty) :-
    SolveMark in [n, Mark],
    xo_solve(Solve, State, SolveMark),
    state(Mark, MarkedQty) in State.

% выигрыш
% xo_win(Solve)
xo_win(Solve) :-
    xo_params(Params),
    line(WinLength) in Params,
    go(MarkX, MarkO) in Params,
    Mark in [MarkX, MarkO],
    xo_solve(Solve, State, Mark),
    state(Mark, WinLength) in State,
    !.

% ничья
% xo_tie(Mode)
xo_tie(Mode) :-
    RuleName = tie_by_chance,
    xo_mode_valid_rule(Mode, RuleName, _WinLength, _ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    xo_solve(_, _, _),
    !,
    not( xo_has_chance(MarkX, _, _) ),
    not( xo_has_chance(MarkO, _, _) ).
xo_tie(_Mode) :-
    xo_cell(_, _),
    !,
    not( xo_cell(_, n) ).

class facts
gift : integer := 0.
count : integer := 0.

class predicates
xo_play :
    (symbol Mode, cell PlayCell, symbol RuleName, string Rule) nondeterm anyflow.

clauses
xo_play_once(Mode, PlayCell, RuleName, Rule) :-
    xo_play(Mode, PlayCell, RuleName, Rule),
    !.

% игра
% xo_play(Mode, PlayCell, RuleName, Rule)
% первый ход - случайный выбор из лучших позиций
xo_play(Mode, PlayCell, RuleName, Rule) :-
    not( xo_cell(_, x) ),
    not( xo_cell(_, o) ),
    RuleName = random_best_pos,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, _ModeGo),
    Method = 0,
    RateCoorList =
      [ tuple(Method, GiftCount, tuple(X, Y)) ||
        xo_cell(Coor, n),
        Coor = coor(X, Y),
        gift := 0,
        count := 0,
        foreach
            xo_solve(Solve, _, _),
            cell(Coor, n) in Solve
        do
            gift := gift + WinLength,
            count := count + 1
        end foreach,
        GiftCount = tuple(gift, count)
      ],
    not( RateCoorList = [] ),
    PlayCoorList = sort(RateCoorList, descending),
    PlayLen = length(PlayCoorList),
    try PlayBest = PlayLen div 2 ^ (ModeLevel - 2) catch _ do PlayBest = 1 end try,
    try PlayIndex = random(PlayBest) catch _ do PlayIndex = 0 end try,
    tuple(Method, tuple(Gift, Count), _) = nth(PlayIndex, PlayCoorList),
    PlayRateCoorList = [coor(X, Y) || tuple(Method, tuple(Gift, Count), tuple(X, Y)) in PlayCoorList],
    PlayRateLen = length(PlayRateCoorList),
    PlayRateIndex = random(PlayRateLen),
    PlayCoor = nth(PlayRateIndex, PlayRateCoorList),
    PlayCell = cell(PlayCoor, n),
    Rule = string::format("rule(%,method=%,length=%/%,index=%/%)", RuleName, Method, PlayLen, PlayRateLen, PlayIndex, PlayRateIndex),
    !.
% первый ход - случайный выбор
xo_play(Mode, PlayCell, RuleName, Rule) :-
    not( xo_cell(_, x) ),
    not( xo_cell(_, o) ),
    xo_random_free_cell(Mode, PlayCell, RuleName, Rule),
    !.
% выигрыш следующим ходом
xo_play(Mode, PlayCell, RuleName, Rule) :-
    RuleName = next_step_win,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    ToWinLength = WinLength - 1,
    %
    Mark in [MarkX, MarkO],
    xo_has_chance(Mark, Solve, ToWinLength),
    %
    cell(Coor, n) in Solve,
    PlayCell = cell(Coor, n),
    Rule = string::format("rule(%,%,%)", RuleName, Mark, Coor),
    succeed.
% свободные края (выигрыш через ход)
xo_play(Mode, PlayCell, RuleName, Rule) :-
    RuleName = free_border,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    ToWinLength = WinLength - 2,
    %
    Mark in [MarkX, MarkO],
    xo_has_chance(Mark, Solve, ToWinLength),
    %
    Solve = [First | Right],
    append_solve(Left, [Last], Solve),
    ( % n nooon n
      First = cell(_FirstCoor, n),
      Last = cell(_LastCoor, n),
      xo_has_chance(Mark, SolveBorder1, ToWinLength),
      append_solve(Right, [cell(_RightCoor, n)], SolveBorder1),
      xo_has_chance(Mark, SolveBorder2, ToWinLength),
      append_solve([cell(_LeftCoor, n)], Left, SolveBorder2),
      PlayList = [First, Last]
    ; % _ nonoo n | _ nnooo n
      xo_has_chance(Mark, SolveBorder1, ToWinLength),
      append_solve(Right, [cell(_RightCoor, n)], SolveBorder1),
      FreeCell in Right,
      FreeCell = cell(_FreeCellCoor, n),
      PlayList = [FreeCell]
    ; % n oonon _ | n ooonn _
      xo_has_chance(Mark, SolveBorder2, ToWinLength),
      append_solve([cell(_LeftCoor, n)], Left, SolveBorder2),
      FreeCell in Left,
      FreeCell = cell(_FreeCellCoor, n),
      PlayList = [FreeCell]
    ),
    PlayLength = length(PlayList),
    PlayIndex = random(PlayLength),
    PlayCell = nth(PlayIndex, PlayList),
    PlayCell = cell(PlayCoor, _),
    Rule = string::format("rule(%,%,%)", RuleName, Mark, PlayCoor),
    succeed.
% тире (выигрыш через ход)
xo_play(Mode, PlayCell, RuleName, Rule) :-
    RuleName = dash_mark,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    ToWinCut2 = WinLength - 2,
    ToWinCut3 = WinLength - 3,
    Mark in [MarkX, MarkO],
    xo_has_chance(Mark, Solve, ToWinCut3),
    Solve = [First | Right],
    append_solve(Left, [Last], Solve),
    %check_point,
    ( % o nonon o
      First = cell(_FirstCoor, n),
      Last = cell(_LastCoor, n),
      xo_has_chance(Mark, SolveBorder1, ToWinCut2),
      append_solve(Right, [cell(_RightCoor, n)], SolveBorder1),
      xo_has_chance(Mark, SolveBorder2, ToWinCut2),
      append_solve([cell(_LeftCoor, Mark)], Left, SolveBorder2),
      Left = [First | Middle],
      FreeCell in Middle,
      FreeCell = cell(_FreeCellCoor, n),
      PlayList = [FreeCell]
    ),
    PlayLength = length(PlayList),
    PlayIndex = random(PlayLength),
    PlayCell = nth(PlayIndex, PlayList),
    PlayCell = cell(PlayCoor, _),
    Rule = string::format("rule(%,%,%)", RuleName, Mark, PlayCoor),
    succeed.
% вилка
xo_play(Mode, PlayCell, RuleName, Rule) :-
    RuleName = fork,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    xo_mode_go(Mode, ModeGo1, ModeGo),
    ModeGo1 = go(Mark1, _Mark2),
    ToWinLength1 = WinLength - 2,
    ToWinLength2 = WinLength - 3,
    MarkedQtyList = [ToWinLength1, ToWinLength2],
    OrderMarkList = [tuple(1, MarkX), tuple(0, MarkO)],
    MarkedSolveList =
      [ tuple(MarkedQty, Order, Mark, Solve) ||
        MarkedQty in MarkedQtyList,
        tuple(Order, Mark) in OrderMarkList,
        xo_has_chance(Mark, Solve, MarkedQty)
      ],
    not( MarkedSolveList = [] ),
    ClaimForkList = sort(MarkedSolveList, descending),
    ForkList =
      [ tuple(Extra, Fork) ||
        xo_has_fork(ClaimForkList, Fork),
        xo_fork_extra(ModeLevel, Mark1, Fork, Extra, RuleName)
      ],
    not( ForkList = [] ),
    [BestFork | TeilForkList] = sort(ForkList, descending),
    BestFork = tuple(ForkExtra, tuple(ForkHeight, ForkPower, ForkWidth, ForkOrder, _, _)),
    PlayForkList =
      [ PlayFork ||
        PlayFork in [BestFork | TeilForkList],
        PlayFork = tuple(ForkExtra, tuple(ForkHeight, ForkPower, ForkWidth, ForkOrder, _, _))
      ],
    PlayLength = length(PlayForkList),
    PlayIndex = random(PlayLength),
    PlayFork = nth(PlayIndex, PlayForkList),
    PlayFork = tuple(Extra, tuple(Height, Power, Width, Order, Mark, PlayCell)),
    PlayCell = cell(PlayCoor, _),
    Rule = string::format("rule(%,extra=%,height=%,power=%,width=%,order=%,%,%)", RuleName, Extra, Height, Power, Width, Order, Mark, PlayCoor),
    succeed.
% случайный выбор из лучших шансов на выигрыш
xo_play(Mode, PlayCell, RuleName, Rule) :-
    RuleName = random_best_chance,
    xo_mode_valid_rule(Mode, RuleName, WinLength, ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    xo_mode_go(Mode, ModeGo1, ModeGo),
    ModeGo1 = go(Mark1, _Mark2),
    xo_limit_coor(WinLength, LimitData),
    ShapeMethod = random(8),
    Cost in [2, 1, 0],
    RateCoorList =
      [ tuple(Extra, MethodRate, tuple(X, Y)) ||
        xo_cell(Coor, n),
        Coor = coor(X, Y),
        xo_check_coor(Coor, LimitData),
        xo_rate(MarkX, Coor, Cost, CompGift, CompCount),
        xo_rate(MarkO, Coor, Cost, UserGift, UserCount),
        TotalCount = CompCount + UserCount,
        TotalCount > 0,
        TotalGift = CompGift + UserGift,
        xo_rate_extra(Cost, ModeLevel, MarkX, Mark1, Coor, Extra),
        RateShape = tuple(TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount),
        xo_rate_shape(ShapeMethod, RateShape, MethodRate)
      ],
    not( RateCoorList = [] ),
    PlayCoorList = sort(RateCoorList, descending),
    PlayLen = length(PlayCoorList),
    try PlayBest = PlayLen div 2 ^ (ModeLevel - 2) catch _ do PlayBest = 1 end try,
    try PlayIndex = random(PlayBest) catch _ do PlayIndex = 0 end try,
    tuple(Extra, MethodRate, _) = nth(PlayIndex, PlayCoorList),
    PlayRateCoorList =
      [ tuple(Extra, MethodRate, Coor) ||
        tuple(Extra, MethodRate, Coor) in PlayCoorList
      ],
    PlayRateLen = length(PlayRateCoorList),
    PlayRateIndex = random(PlayRateLen),
    tuple(Extra, tuple(Method, _Rate), tuple(X, Y)) = nth(PlayRateIndex, PlayRateCoorList),
    PlayCell = cell(coor(X, Y), n),
    Rule = string::format("rule(%,extra=%,method=%,cost=%,length=%/%,index=%/%)", RuleName, Extra, Method, Cost, PlayLen, PlayRateLen, PlayIndex, PlayRateIndex),
    !.
% случайный выбор из шансов на выигрыш
xo_play(Mode, PlayCell,  RuleName, Rule) :-
    RuleName = random_chance,
    xo_mode_valid_rule(Mode, RuleName, WinLength, _ModeLevel, ModeGo),
    ModeGo = go(MarkX, MarkO),
    xo_limit_coor(WinLength, LimitData),
    Cost in [1, 0],
    Mark in [MarkX, MarkO],
    FreeCoorList =
      [ Coor ||
        xo_cell(Coor, n),
        xo_check_coor(Coor, LimitData),
        xo_rate(Mark, Coor, Cost, _, Count),
        Count <> 0
      ],
    not( FreeCoorList = [] ),
    Len = length(FreeCoorList),
    Index = random(Len),
    Coor = nth(Index, FreeCoorList),
    PlayCell = cell(Coor, n),
    Rule = string::format("rule(%,cost=%,length=%,index=%)", RuleName, Cost, Len, Index),
    !.
% случайный выбор свободной ячейки
xo_play(Mode, PlayCell, RuleName, Rule) :-
    xo_random_free_cell(Mode, PlayCell, RuleName, Rule),
    !.
% выбор первой свободной ячейки (заглушка)
xo_play(_, cell(Coor, n), none, "") :-
    xo_cell(Coor, n),
    !.

class predicates
xo_has_fork :
    (tuple{integer MarkedQty, integer Order, symbol Mark, cell* Solve}*,
     tuple{integer ForkHeight, integer ForkPower,
            integer ForkWidth, integer ForkOrder, symbol Mark, cell FreeCell} Fork
    ) nondeterm (i,o).

xo_fork_extra :
    (integer ModeLevel, symbol NormalMark,
     tuple{integer ForkHeight, integer ForkPower,
            integer ForkWidth, integer ForkOrder, symbol Mark, cell FreeCell} Fork,
     tuple{integer,integer} Extra [out], string OwnRuleName
    ).


clauses
% есть вилка
% xo_has_fork(MarkedSolveList, Fork)
xo_has_fork([ClaimFork | TeilSolves], Fork) :-
    ClaimFork = tuple(MarkedQty, Order, Mark, Solve),
    FreeCell in Solve,
    FreeCell = cell(_, n),
    ClaimOrder in [1, 0],
    ForkMarkedQtyList =
      [ ForkMarkedQty ||
        tuple(ForkMarkedQty, ClaimOrder, _, ForkSolve) in TeilSolves,
        select_solve(FreeCell, ForkSolve, ForkSolveRest),
        not ( (ForkCell in ForkSolveRest, ForkCell in Solve) )
      ],
    not( ForkMarkedQtyList = [] ),
    MaxMarkedQty = maximum([MarkedQty | ForkMarkedQtyList]),
    ForkHeight = MaxMarkedQty + 1,
    ForkPower = sum([MarkedQty | ForkMarkedQtyList]),
    ForkWidth = length([MarkedQty | ForkMarkedQtyList]),
    tuple(Order, ClaimOrder, ForkOrder) in
      [tuple(1, 1, 2), tuple(0, 0, 1), tuple(1, 0, 0), tuple(0, 1, 0)],
    Fork = tuple(ForkHeight, ForkPower, ForkWidth, ForkOrder, Mark, FreeCell).
xo_has_fork([_ | TeilSolves], Fork) :-
    xo_has_fork(TeilSolves, Fork).

% xo_fork_extra(ModeLevel, NormalMark, Fork, Extra, OwnRuleName)
xo_fork_extra(ModeLevel, NormalMark, Fork, Extra, OwnRuleName) :-
    ModeLevel >= 8,
    Fork = tuple(_, _, _, _, Mark, FreeCell),
    if Mark = NormalMark then ExtraMode = normal else ExtraMode = echo end if,
    FreeCell = cell(Coor, n),
    xo_mark_cell(ExtraMode, FreeCell),
    xo_cell(Coor, NewMark),
    CellMarked = cell(Coor, NewMark),
    %
    Rules =
      [ tuple(RuleName, Value) ||
             xo_rule(RuleName, RuleOpt),
             kind(heuristic) in RuleOpt,
             not( RuleName = OwnRuleName ),
             value(Value) in RuleOpt
      ],
    gift := 0,
    count := 0,
    foreach
        tuple(RuleName, Value) in Rules,
        xo_play(ExtraMode, PlayCell, RuleName, _Rule),
        PlayCell = cell(_, Mark)
    do
        gift := gift + Value,
        count := count + 1
    end foreach,
    Extra = tuple(gift, count),
    %
    xo_unmark_cell(CellMarked),
    !.
xo_fork_extra(_, _, _, tuple(0, 0), _).

class predicates
xo_rate_extra :
    (integer Cost, integer ModeLevel, symbol MarkX, symbol NormalMark, coor Coor,
     tuple{integer,integer} Extra [out]).

clauses
% xo_rate_extra(Cost, ModeLevel, MarkX, NormalMark, Coor, Extra)
xo_rate_extra(2, ModeLevel, MarkX, NormalMark, Coor, Extra) :-
    ModeLevel >= 9,
    if MarkX = NormalMark then ExtraMode = normal else ExtraMode = echo end if,
    FreeCell = cell(Coor, n),
    xo_mark_cell(ExtraMode, FreeCell),
    xo_cell(Coor, NewMark),
    CellMarked = cell(Coor, NewMark),
    %
    Rules =
      [ tuple(RuleName, Value) ||
             xo_rule(RuleName, RuleOpt),
             kind(heuristic) in RuleOpt,
             value(Value) in RuleOpt
      ],
    gift := 0,
    count := 0,
    foreach
        tuple(RuleName, Value) in Rules,
        xo_play(ExtraMode, _PlayCell, RuleName, _Rule)
    do
        gift := gift + Value,
        count := count + 1
    end foreach,
    Extra = tuple(gift, count),
    %
    xo_unmark_cell(CellMarked),
    !.
xo_rate_extra(_, _, _, _, _, tuple(0, 0)).

class predicates
xo_limit_coor : (integer WinLength, tuple{integer, integer, integer, integer} LimitData [out]).
xo_limit_coor_ :
    (integer WinLength,
     tuple{integer, integer, integer, integer} LimitData [out],
     tuple{integer, integer, integer, integer} PlaySpace [out]).
xo_check_coor : (coor Coor, tuple{integer, integer, integer, integer} LimitData) determ.
xo_random_free_cell :
    (symbol Mode, cell PlayCell, symbol RuleName, string Rule) determ anyflow.

clauses
% xo_limit_coor(WinLength, LimitData)
xo_limit_coor(WinLength, LimitData) :-
    xo_limit_coor_(WinLength, LimitData, _PlaySpace).
% xo_limit_coor(WinLength, LimitData, PlaySpace)
xo_limit_coor_(WinLength, LimitData, PlaySpace) :-
    XList = [X || xo_cell(coor(X, _), Mark), Mark <> n],
    YList = [Y || xo_cell(coor(_, Y), Mark), Mark <> n],
    MinX0 = minimum(XList), MinX = MinX0 - WinLength,
    MaxX0 = maximum(XList), MaxX = MaxX0 + WinLength,
    MinY0 = minimum(YList), MinY = MinY0 - WinLength,
    MaxY0 = maximum(YList), MaxY = MaxY0 + WinLength,
    LimitData = tuple(MinX, MaxX, MinY, MaxY),
    PlaySpace = tuple(MinX0, MaxX0, MinY0, MaxY0),
    !.

% xo_check_coor(Coor, LimitData)
xo_check_coor(Coor, LimitData) :-
    Coor = coor(X, Y),
    LimitData = tuple(MinX, MaxX, MinY, MaxY),
    X >= MinX, X <= MaxX,
    Y >= MinY, Y <= MaxY,
    !.

% xo_random_free_cell(Mode, PlayCell, RuleName, Rule)
xo_random_free_cell(Mode, PlayCell, RuleName, Rule) :-
    RuleName = random_free_cell,
    xo_mode_valid_rule(Mode, RuleName, _WinLength, _ModeLevel, _ModeGo),
    FreeCoorList = [Coor || xo_cell(Coor, n)],
    not( FreeCoorList = [] ),
    Len = length(FreeCoorList),
    Index = random(Len),
    Coor = nth(Index, FreeCoorList),
    PlayCell = cell(Coor, n),
    Rule = string::format("rule(%,length=%,index=%)", RuleName, Len, Index),
    !.

class predicates
xo_rate_shape :
    (positive, tuple{integer, integer, integer, integer, integer, integer} RateShape,
     tuple{integer, tuple{integer, integer, integer, integer, integer, integer}} MethodRate [out]).
xo_rate : (symbol Mark, coor Coor, integer Cost, integer Gift [out], integer Count [out]).

clauses
% шаблон ранга
% xo_rate_shape(Method, RateShape, MethodRate)
xo_rate_shape(Method, RateShape, MethodRate) :-
    RateShape = tuple(TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount),
    List = [
        tuple(1, tuple(TotalGift, TotalCount, CompGift, UserGift, CompCount, UserCount)),
        tuple(2, tuple(TotalGift, TotalCount, UserGift, CompGift, UserCount, CompCount)),
        tuple(3, tuple(TotalCount, TotalGift, CompCount, UserCount, CompGift, UserGift)),
        tuple(4, tuple(TotalCount, TotalGift, UserCount, CompCount, UserGift, CompGift)),
        tuple(5, tuple(TotalGift, TotalCount, CompCount, UserCount, CompGift, UserGift)),
        tuple(6, tuple(TotalGift, TotalCount, UserCount, CompCount, UserGift, CompGift)),
        tuple(7, tuple(TotalCount, TotalGift, CompGift, UserGift, CompCount, UserCount)),
        tuple(8, tuple(TotalCount, TotalGift, UserGift, CompGift, UserCount, CompCount))
    ],
    MethodRate = nth(Method, List),
    !.

% ранг ячейки
% xo_rate(Mark, Coor, Cost, Rate)
xo_rate(Mark, Coor, Cost, Gift, Count) :-
    gift := 0,
    count := 0,
    foreach
        xo_has_chance(Mark, Solve, MarkedQty),
        MarkedQty >= Cost,
        cell(Coor, n) in Solve
    do
        gift := gift + MarkedQty,
        count := count + 1
    end foreach,
    Gift = gift,
    Count = count,
    !.

class predicates
xo_change_solve :
    (cell* Solve, cell Cell, symbol Mark, cell* ChangedSolve,
     state* State, state* ChangedState, symbol HasChanceMark)
    determ
    (i,i,i,o,i,o,o).
xo_change_chance : (state* State, symbol HasChanceMark) determ (i,o).

clauses
% отметка ячейки
% xo_mark_cell(Mode, Cell)
xo_mark_cell(Mode, Cell) :-
    xo_params(Params),
    go(MarkX, MarkO) in Params,
    tuple(Mode, Mark) in [tuple(normal, MarkX), tuple(echo, MarkO)],
    xo_solve(Solve, State, HasChanceMark),
    Cell in Solve,
    xo_change_solve(Solve, Cell, Mark, ChangedSolve, State, ChangedState, ChangedHasChanceMark),
    retract( xo_solve(Solve, State, HasChanceMark) ),
    asserta( xo_solve(ChangedSolve, ChangedState, ChangedHasChanceMark) ),
    fail.
xo_mark_cell(Mode, Cell) :-
    xo_params(Params),
    go(MarkX, MarkO) in Params,
    tuple(Mode, Mark) in [tuple(normal, MarkX), tuple(echo, MarkO)],
    Cell = cell(Coor, n),
    retract( xo_cell(Coor, n) ),
    asserta( xo_cell(Coor, Mark) ),
    ( xo_step(_, Step, _) ; Step = 0 ),
    Step1 = Step + 1,
    asserta( xo_step(Mark, Step1, Coor) ),
    !.

% очистка ячейки
% xo_unmark_cell(Cell)
xo_unmark_cell(Cell) :-
    xo_solve(Solve, State, HasChanceMark),
    Cell in Solve,
    xo_change_solve(Solve, Cell, n, ChangedSolve, State, ChangedState, ChangedHasChanceMark),
    retract( xo_solve(Solve, State, HasChanceMark) ),
    asserta( xo_solve(ChangedSolve, ChangedState, ChangedHasChanceMark) ),
    fail.
xo_unmark_cell(Cell) :-
    Cell = cell(Coor, _),
    retract( xo_cell(Coor, _) ),
    asserta( xo_cell(Coor, n) ),
    retract( xo_step(_Mark, _Step, Coor) ),
    !.

% шаг назад
% xo_back()
xo_back() :-
    xo_step(Mark, Step, Coor),
    Cell = cell(Coor, Mark),
    xo_unmark_cell(Cell),
    asserta( xo_step_back(Mark, Step, Coor) ),
    !.

% шаг вперед
% xo_forth()
xo_forth() :-
    retract( xo_step_back(Mark, _Step, Coor) ),
    xo_params(Params),
    go(MarkX, _MarkO) in Params,
    if Mark = MarkX then Mode = normal else Mode = echo end if,
    Cell = cell(Coor, n),
    xo_mark_cell(Mode, Cell),
    !.

xo_forth_clear() :-
    retractall( xo_step_back(_, _, _) ).

% смена состояния для решения
% xo_change_solve(Solve, Cell, Mark, ChangedSolve, State, ChangedState, ChangedHasChanceMark)
xo_change_solve([Cell|TeilSolve], Cell, Mark, [ChangedCell|TeilSolve], State, ChangedState, ChangedHasChanceMark) :-
    Cell = cell(Coor, OldMark),
    ChangedCell = cell(Coor, Mark),
    state(Mark, Qty) in State,
    state(OldMark, OldQty) in State,
    state(Mark3, Qty3) in State,
    not( Mark3 in [Mark, OldMark] ),
    ChangedState = [state(Mark3, Qty3), state(Mark, Qty + 1), state(OldMark, OldQty - 1)],
    xo_change_chance(ChangedState, ChangedHasChanceMark),
    !.
xo_change_solve([SafeCell|TeilSolve], Cell, Mark, [SafeCell|RestSolve], State, ChangedState, ChangedHasChanceMark) :-
    !,
    xo_change_solve(TeilSolve, Cell, Mark, RestSolve, State, ChangedState, ChangedHasChanceMark).

% xo_change_chance(State, HasChanceMark)
xo_change_chance(State, HasChanceMark) :-
    xo_params(Params),
    go(MarkX, MarkO) in Params,
    ( state(MarkX, 0) in State,
      state(MarkO, 0) in State,
      HasChanceMark = n
    ; state(MarkO, 0) in State,
      state(MarkX, MarkXedQty) in State,
      MarkXedQty > 0,
      HasChanceMark = MarkX
    ; state(MarkX, 0) in State,
      state(MarkO, MarkOedQty) in State,
      MarkOedQty > 0,
      HasChanceMark = MarkO
    ; HasChanceMark = z
    ),
    !.

domains
rule_opt =
    level(integer);
    kind(symbol);
    value(integer);
    desc(string).

class predicates
xo_rule : (symbol RuleName, rule_opt* RuleOpt) determ (i, o).
xo_rule : (symbol RuleName, rule_opt* RuleOpt) multi (o,o).

clauses
% правила
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

class predicates

clauses
% инициализация
% xo_init
xo_init() :-
    xo_make_cell,
    xo_make_solve,
    retractall( xo_step(_, _, _) ),
    retractall( xo_step_back(_, _, _) ),
    !.

xo_clear() :-
    retractall( xo_cell(_, _) ),
    retractall( xo_solve(_, _, _) ),
    retractall( xo_step(_, _, _) ),
    retractall( xo_step_back(_, _, _) ),
    !.

clauses
    run() :-
        _ = mainForm::display(gui::getScreenWindow()),
        messageLoop::run().

end implement main

goal
    formWindow::run(main::run).
