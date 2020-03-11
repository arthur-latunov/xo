% Copyright AR

class main
    open core

predicates
    run : core::runnable.

constants
x : symbol = "x".
o : symbol = "o".
n : symbol = "n".
z : symbol = "z".
normal : symbol = "normal".
echo : symbol = "echo".

domains
coor = coor(integer X, integer Y).
cell = cell(coor Coor, symbol Mark).
state = state(symbol Mark, integer Qty).

predicates
xo_get_size : (integer PosBegin [out], integer PosEnd [out]).
xo_set_size : (integer PosBegin, integer PosEnd) determ.
xo_get_levels : (integer [out], integer [out]).
xo_set_levels : (integer, integer) determ.
xo_get_cell : (coor Coor, symbol Mark) nondeterm (o,o) (i,o) (i,i).
xo_step_once : (symbol Mark, integer Step, coor Coor) determ (i,o,o) (o,o,o).
xo_play_once :
    (symbol Mode, cell PlayCell, symbol RuleName, string Rule) determ (i,o,o,o).
xo_mark_cell : (symbol Mode, cell Cell) determ.
xo_unmark_cell : (cell Cell) determ.
xo_win : (cell* Solve [out]) determ.
xo_tie : (symbol Mode) determ.
xo_init : () determ.
xo_clear : ().

end class main