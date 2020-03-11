% Copyright AR

implement mainForm inherits formWindow
    open core, vpiDomains, vpi, resourceIdentifiers, main

clauses
    display(Parent) = Form :-
        Form = new(Parent),
        Form:show().

constructors
    new : (window Parent).
clauses
    new(Parent) :-
        formWindow::new(Parent),
        generatedInitialize(),
        navigationOverlay::registerSDI(This).

predicates
    onIconMouseDbl : window::mouseDblListener.
clauses
    onIconMouseDbl(Source, _Point, _ShiftControlAlt, _Button) :-
        _AboutDialog = aboutDialog::display(Source).

constants
app_name : string = "Крестики-нолики (пять в ряд)".
app_font_name : string = "Verdana".
game_levels : string* = ["1", "2", "3", "4", "5", "6", "7", "8", "9"].

facts
win_gdi : windowGDI := getWindowGDI().
hwnd : windowHandle := winGetActiveWindow().
pic_n : picture := pictGetFromRes(bmp_n).
pic_x : picture := pictGetFromRes(bmp_x).
pic_o : picture := pictGetFromRes(bmp_o).
pic_z : picture := pictGetFromRes(bmp_z).
pic_nc : picture := pictGetFromRes(bmp_nc).
pic_xc : picture := pictGetFromRes(bmp_xc).
pic_oc : picture := pictGetFromRes(bmp_oc).
pic_zc : picture := pictGetFromRes(bmp_zc).
pic_ng : picture := pictGetFromRes(bmp_ng).
pic_xg : picture := pictGetFromRes(bmp_xg).
pic_og : picture := pictGetFromRes(bmp_og).
pic_zg : picture := pictGetFromRes(bmp_zg).
pic_ny : picture := pictGetFromRes(bmp_ny).
pic_xy : picture := pictGetFromRes(bmp_xy).
pic_oy : picture := pictGetFromRes(bmp_oy).
pic_zy : picture := pictGetFromRes(bmp_zy).
/*
pic_nm : picture := pictGetFromRes(bmp_nm).
pic_xm : picture := pictGetFromRes(bmp_xm).
pic_om : picture := pictGetFromRes(bmp_om).
pic_zm : picture := pictGetFromRes(bmp_zm).
pic_zd : picture := pictGetFromRes(bmp_zd).
pic_zr : picture := pictGetFromRes(bmp_zr).
pic_zb : picture := pictGetFromRes(bmp_zb).
*/

facts
win_font : font := vpi::fontCreateByName(app_font_name, 10).
win_bold_font : font := vpi::fontSetAttrs(win_font, [fs_Bold], 10).
%win_underline_font : font := vpi::fontSetAttrs(win_font, [fs_UnderLine], 10).
win_small_font : font := vpi::fontSetAttrs(win_font, [], 8).
win_small_bold_font : font := vpi::fontSetAttrs(win_font, [fs_Bold], 8).
%win_small_underline_font : font := vpi::fontSetAttrs(win_font, [fs_UnderLine], 8).
app_splash : integer := 0.

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data) :-
        win_init(),
        succeed.

constants
cell_size : integer = 22.

facts
field_size : integer := 0.
pos_begin : integer := 0.
pos_end : integer := 0.
pos_offset : integer := 0.

facts
field_left : integer := 0.
field_top : integer := 0.
field_right : integer := 0.
field_bottom : integer := 0.

facts
box_left : integer := 0.
box_top : integer := 0.
box_right : integer := 0.
box_bottom : integer := 0.

predicates
win_init : ().

clauses
win_init() :-
    %winSetForeColor(hwnd, color_black),
    %winSetBackColor(hwnd, color_WhiteSmoke),
    %winClear(hwnd, color_WhiteSmoke),
    setText(app_name),
    home_pushButton_ctl:setVisible(false),
    back_pushButton_ctl:setVisible(false),
    force_pushButton_ctl:setVisible(false),
    end_pushButton_ctl:setVisible(false),
    info_StaticText_ctl:setVisible(false),
    gameOpt_ctl:setFont(win_small_font),
    x_listButton_ctl:addList(game_levels),
    o_listButton_ctl:addList(game_levels),
    play_pushButton_ctl:setFont(win_bold_font),
    stop_pushButton_ctl:setFont(win_bold_font),
    home_pushButton_ctl:setFont(win_bold_font),
    back_pushButton_ctl:setFont(win_bold_font),
    force_pushButton_ctl:setFont(win_bold_font),
    end_pushButton_ctl:setFont(win_bold_font),
    icon_ctl:addMouseDblListener(onIconMouseDbl),
    %
    win_gdi:setFont(win_small_font),
    win_gdi:setPen(pen(1, ps_Solid, color_Gray)),
    win_gdi:setBrush(brush(pat_Solid, color_WhiteSmoke)),
    rct(Lx, Tx, _Rx, Bx) = x_Coor_ctl:getOuterRect(),
    rct(Ly, _Ty, Ry, _By) = y_Coor_ctl:getOuterRect(),
    xo_get_size(PosBegin, PosEnd),
    pos_begin := PosBegin,
    pos_end := PosEnd,
    pos_offset := 1 - pos_begin,
    field_size := PosEnd - PosBegin + 1,
    field_left := Lx + 1,
    field_top := Bx,
    field_right := field_left + field_size * cell_size + 2,
    field_bottom := field_top + field_size * cell_size + 2,
    box_left := Ly + 1,
    box_top := Tx,
    box_right := Ry + 1,
    box_bottom := Bx,
    %
    xo_get_levels(NormalLevel, EchoLevel),
    x_listButton_ctl:selectAt(NormalLevel-1, true),
    o_listButton_ctl:selectAt(EchoLevel-1, true),
    !.

predicates
    onPaint : window::paintResponder.
clauses
    onPaint(_Source, _Rectangle, _GDI) :-
        if app_splash = 1 then
            fill_cells([1], 0),
            fill_cells([2,3,4,5,6], 1),
            fill_cells([1], 1),
            app_splash := 0
        else
            fill_cells([1], 0)
        end if,
        succeed.

predicates
fill_cells : (integer* Flips, integer SmallPic).

clauses
fill_cells(Flips, SmallPic) :-
    rct(Li, Ti, Ri, Bi) = info_StaticText_ctl:getOuterRect(),
    win_gdi:drawRect(rct(Li-1, Ti-3, Ri+1, Bi+1)),
    win_gdi:setPen(pen(1, ps_Solid, color_Gray)),
    win_gdi:setBrush(brush(pat_Solid, color_WhiteSmoke)),
    win_gdi:drawRect(rct(box_left, box_top, box_right, box_bottom)),
    foreach
        Flip in Flips
    do
        foreach
            I = std::between(pos_begin, pos_end),
            J = std::between(pos_begin, pos_end)
        do
            if I = pos_begin, Flip = 1, SmallPic = 0 then
                draw_coor(I, J, coor_norm, 1)
            end if,
            if J = pos_begin, Flip = 1, SmallPic = 0 then
                draw_coor(I, J, coor_norm, 1)
            end if,
            draw_cell(I, J, cell_space, SmallPic),
            %
            if Flip <> 1 then
                N = math::random(3),
                Xf = field_left + (I + pos_offset - 1) * cell_size + 4,
                Yf = field_top + (J + pos_offset - 1) * cell_size + 4,
                PicFlash = list::nth(N, [pic_z, pic_x, pic_o]),
                win_gdi:pictDraw(PicFlash, pnt(Xf, Yf), rop_SrcCopy)
            end if
        end foreach,
        if Flip > 1 then
            programControl::sleep(250)
        end if
    end foreach,
    !.

facts
cell_space : integer := 0.
cell_flash : integer := 1.
cell_force : integer := 2.
cell_back : integer := 3.

predicates
draw_cell : (integer I, integer J, integer Attr, integer SmallPic).

clauses
draw_cell(I, J, Attr, SmallPic) :-
    X = field_left + (I + pos_offset - 1) * cell_size,
    Y = field_top + (J + pos_offset - 1) * cell_size,
    tuple(Attr, SmallPic, PicField) in
        [ tuple(cell_space, 0, pic_n),  tuple(cell_space, 1, pic_z),
          tuple(cell_flash, 0, pic_ny), tuple(cell_flash, 1, pic_zy),
          tuple(cell_force, 0, pic_ng), tuple(cell_force, 1, pic_zg),
          tuple(cell_back, 0, pic_nc), tuple(cell_back, 1, pic_zc)
        ],
    win_gdi:pictDraw(PicField, pnt(X+4*SmallPic, Y+4*SmallPic), rop_SrcCopy),
    %
    xo_get_cell(coor(I, J), Mark),
    tuple(Mark, Attr, PicMark) in
        [ tuple(x, cell_space, pic_x),  tuple(o, cell_space, pic_o),
          tuple(x, cell_flash, pic_xy), tuple(o, cell_flash, pic_oy),
          tuple(x, cell_force, pic_xg), tuple(o, cell_force, pic_og),
          tuple(x, cell_back, pic_xc), tuple(o, cell_back, pic_oc)
        ],
    win_gdi:pictDraw(PicMark, pnt(X+4, Y+4), rop_SrcCopy),
    !.
draw_cell(_I, _J, _Attr, _SmallPic).

predicates
    onPlay_pushButtonClick : button::clickResponder.
    onStop_pushButtonClick : button::clickResponder.

clauses
    onPlay_pushButtonClick(_Source) = button::defaultAction :-
        play_pushButton_ctl:setEnabled(false),
        stop_pushButton_ctl:setEnabled(),
        xo_init(),
        fill_cells([1], 1),
        play_loop(),
        !.
    onPlay_pushButtonClick(_Source) = button::defaultAction.

    onStop_pushButtonClick(_Source) = button::defaultAction :-
        stop_pushButton_ctl:setEnabled(false),
        play_pushButton_ctl:setEnabled(),
        xo_clear(),
        fill_cells([1], 1),
        !.

constants
comp_play : integer = 1.
user_play : integer = 2.

predicates
xo_cell_exists : () determ.
play_check : (integer SidePlay, symbol Mark [out]) determ.
play_check_ : (integer SidePlay, symbol Mark) determ.
play_loop : () determ.
play_auto : (symbol Mark) determ.
play_end :(integer DrawSolve) determ (i).

clauses
xo_cell_exists() :-
    xo_get_cell(_, _),
    !.

play_check(SidePlay, Mark) :-
    ( xo_step_once(Mark, _Step, _Coor)
    ; not( xo_step_once(_Mark, _Step, _Coor) ),
      Mark = o
    ),
    play_check_(SidePlay, Mark),
    !.

play_check_(comp_play, x) :-
    o_Comp_ctl:getRadioState() = radioButton::checked,
    !.
play_check_(comp_play, o) :-
    x_Comp_ctl:getRadioState() = radioButton::checked,
    !.
play_check_(user_play, x) :-
    o_User_ctl:getRadioState() = radioButton::checked,
    !.
play_check_(user_play, o) :-
    x_User_ctl:getRadioState() = radioButton::checked,
    !.

play_loop() :-
    xo_cell_exists(),
    not( play_end(0) ),
    play_check(comp_play, Mark),
    play_auto(Mark),
    play_end(1),
    !.
play_loop() :-
    xo_cell_exists(),
    not( play_end(0) ),
    play_check(comp_play, _Mark),
    _IsSuccessful = vpi::processEvents(),
    !,
    play_loop().
play_loop().

play_auto(Mark) :-
    tuple(Mark, Mode) in [tuple(o, normal), tuple(x, echo)],
    xo_play_once(Mode, PlayCell, _RuleName, _Rule),
    xo_mark_cell(Mode, PlayCell),
    PlayCell = cell(coor(I, J), _),
    draw_cell(I, J, cell_flash, 1),
    programControl::sleep(250),
    draw_cell(I, J, cell_space, 1),
    !.

play_end(DrawSolve) :-
    xo_win(Solve),
    if DrawSolve = 1 then
        foreach
            cell(coor(I, J), _) in Solve
        do
            draw_cell(I, J, cell_force, 1),
            programControl::sleep(100)
        end foreach,
        programControl::sleep(1000),
        fill_cells([1], 1)
    end if,
    !.
play_end(_DrawSolve) :-
    xo_step_once(Mark, _Step, _Coor),
    tuple(Mark, Mode) in [tuple(x, normal), tuple(o, echo)],
    xo_tie(Mode),
    !.

predicates
    onMouseDown : window::mouseDownListener.
clauses
    onMouseDown(_Source, Point, _ShiftControlAlt, Button) :-
        xo_cell_exists(),
        Point = pnt(X, Y),
        X = std::between(field_left, field_right),
        Y = std::between(field_top, field_bottom),
        Button = 0,
        user_turn(X, Y),
        !.
    onMouseDown(_Source, Point, _ShiftControlAlt, Button) :-
        xo_cell_exists(),
        Point = pnt(X, Y),
        X = std::between(box_left, box_right),
        Y = std::between(box_top, box_bottom),
        Button = 0,
        last_turn(),
        !.
    onMouseDown(_Source, _Point, _ShiftControlAlt, _Button).

predicates
user_turn : (integer X, integer Y) determ.
last_turn : ().

clauses
user_turn(X, Y) :-
    xo_cell_exists(),
    not( play_end(0) ),
    I = (X - field_left) div cell_size - pos_offset + 1,
    J = (Y - field_top) div cell_size - pos_offset + 1,
    Coor = coor(I, J),
    xo_get_cell(Coor, n),
    play_check(user_play, Mark),
    tuple(Mark, Mode) in [tuple(o, normal), tuple(x, echo)],
    PlayCell = cell(Coor, n),
    xo_mark_cell(Mode, PlayCell),
    draw_cell(I, J, cell_flash, 1),
    programControl::sleep(250),
    draw_cell(I, J, cell_space, 1),
    play_end(1),
    !.
user_turn(_X, _Y) :-
    xo_cell_exists(),
    not( play_end(0) ),
    play_check(comp_play, _Mark),
    _IsSuccessful = vpi::processEvents(),
    !,
    play_loop().
user_turn(_X, _Y).

last_turn() :-
    win_gdi:setPen(pen(1, ps_Solid, color_Gray)),
    win_gdi:setBrush(brush(pat_Solid, color_lightYellow)),
    win_gdi:drawRect(rct(box_left, box_top, box_right, box_bottom)),
    programControl::sleep(250),
    win_gdi:setBrush(brush(pat_Solid, color_WhiteSmoke)),
    win_gdi:drawRect(rct(box_left, box_top, box_right, box_bottom)),
    %
    xo_step_once(_Mark, _Step, Coor),
    Coor = coor(I, J),
    foreach
        _Loop = std::cIterate(3)
    do
        draw_cell(I, J, cell_flash, 1),
        programControl::sleep(125),
        draw_cell(I, J, cell_space, 1),
        programControl::sleep(125)
    end foreach,
    %
    play_end(1),
    !.
last_turn().

predicates
    onX_listButtonSelectionChanged : listControl::selectionChangedListener.
clauses
    onX_listButtonSelectionChanged(_Source) :-
        xo_get_levels(NormalLevel, EchoLevel),
        not( x_listButton_ctl:isSelectedIndex(NormalLevel-1) ),
        NormalLevel1 = x_listButton_ctl:tryGetSelectedIndex(),
        xo_set_levels(NormalLevel1+1, EchoLevel),
        !.
    onX_listButtonSelectionChanged(_Source).

predicates
    onO_listButtonSelectionChanged : listControl::selectionChangedListener.
clauses
    onO_listButtonSelectionChanged(_Source) :-
        xo_get_levels(NormalLevel, EchoLevel),
        not( o_listButton_ctl:isSelectedIndex(EchoLevel-1) ),
        EchoLevel1 = o_listButton_ctl:tryGetSelectedIndex(),
        xo_set_levels(NormalLevel, EchoLevel1+1),
        !.
    onO_listButtonSelectionChanged(_Source).

facts
coor_norm : integer := 0.
coor_sign : integer := 1.
coor_i : integer := -1.
coor_j : integer := -1.
coor_i_state : integer := -1.
coor_j_state : integer := -1.

predicates
    onMouseMove : window::mouseMoveListener.
clauses
    onMouseMove(_Source, Point, _ShiftControlAlt, _Buttons) :-
        Point = pnt(X, Y),
        X = std::between(field_left, field_right),
        Y = std::between(field_top, field_bottom),
        I = (X - field_left) div cell_size - pos_offset + 1,
        J = (Y - field_top) div cell_size - pos_offset + 1,
        if tuple(I, J) <> tuple(coor_i, coor_j) then
            draw_coor(coor_i, coor_j, coor_norm, 0)
        end if,
        draw_coor(I, J, coor_sign, 0),
        draw_coor_desk(I, J, 1),
        !.
    onMouseMove(_Source, _Point, _ShiftControlAlt, _Buttons) :-
        draw_coor(coor_i, coor_j, coor_norm, 0),
        draw_coor_desk(coor_i, coor_j, 0),
        !.
facts
    coor_pic_i : (integer I, integer State, picture Picture).
    coor_pic_j : (integer J, integer State, picture Picture).
predicates
    draw_coor : (integer I, integer J, integer State, integer Force) procedure.
    draw_coor_desk : (integer I, integer J, integer Visible) procedure.
clauses
    draw_coor(I, J, State, Force) :-
        if xo_cell_exists() or Force = 1 then succeed else fail end if,
        I = std::between(pos_begin, pos_end),
        J = std::between(pos_begin, pos_end),
        tuple(State, SignFont) in [tuple(coor_norm, win_small_font), tuple(coor_sign, win_small_bold_font)],
        tuple(State, PenColor) in [tuple(coor_norm, color_Gray), tuple(coor_sign, color_black)],
        tuple(State, BrushColor) in [tuple(coor_norm, color_whiteSmoke), tuple(coor_sign, color_lightYellow)],
        win_gdi:setFont(SignFont),
        win_gdi:setPen(pen(1, ps_Solid, PenColor)),
        win_gdi:setBrush(brush(pat_Solid, BrushColor)),
        if not(I = coor_i) or not(State = coor_i_state) then
            Xb = box_left + (I + pos_offset) * cell_size,
            RectIBox = rct(Xb-1, box_top, Xb+cell_size-2, box_bottom),
            RectIText = rct(Xb-1, box_top+3, Xb+cell_size-2, box_bottom),
            if coor_pic_i(I, State, IPicture) then
                win_gdi:pictDraw(IPicture, pnt(Xb-1, box_top), rop_SrcCopy)
            else
                win_gdi:drawRect(RectIBox),
                win_gdi:drawTextInRect(RectIText, toString(I), [dtext_center, dtext_Noclip]),
                IPicture = pictGetFromWin(hwnd, RectIBox),
                assertz( coor_pic_i(I, State, IPicture) )
            end if,
            coor_i := I,
            coor_i_state := State
        end if,
        if not(J = coor_j) or not(State = coor_j_state) then
            Yb = box_top + (J + pos_offset) * cell_size,
            RectJBox = rct(box_left, Yb-1, box_right, Yb+cell_size-2),
            RectJText = rct(box_left, Yb+2, box_right, Yb+cell_size-2),
            if coor_pic_j(J, State, JPicture) then
                win_gdi:pictDraw(JPicture, pnt(box_left, Yb-1), rop_SrcCopy)
            else
                win_gdi:drawRect(RectJBox),
                win_gdi:drawTextInRect(RectJText, toString(J), [dtext_center, dtext_Noclip]),
                JPicture = pictGetFromWin(hwnd, RectJBox),
                assertz( coor_pic_j(J, State, JPicture) )
            end if,
            coor_j := J,
            coor_j_state := State
        end if,
        !.
    draw_coor(_I, _J, _State, _Force).

draw_coor_desk(I, J, Visible) :-
    xo_cell_exists(),
    tuple(Visible, Rop) in [tuple(0, rop_SrcCopy), tuple(1, rop_NotSrcCopy)],
    coor_pic_i(I, coor_norm, INormPicture),
    win_gdi:pictDraw(INormPicture,
        pnt(field_right-cell_size-(cell_size div 2), field_bottom),
        Rop),
    coor_pic_i(J, coor_norm, JNormPicture),
    win_gdi:pictDraw(JNormPicture,
        pnt(field_right-(cell_size div 2)-2, field_bottom),
        Rop),
    !.
draw_coor_desk(_I, _J, _Visible) :-
    win_gdi:setPen(pen(1, ps_Solid, color_WhiteSmoke)),
    win_gdi:setBrush(brush(pat_Solid, color_WhiteSmoke)),
    Left = field_right-cell_size-(cell_size div 2),
    Top = field_bottom,
    Right = Left + cell_size*2,
    Bottom = Top + cell_size,
    win_gdi:drawRect(rct(Left, Top, Right, Bottom)),
    !.

% This code is maintained automatically, do not update it manually.
%  14:13:58-10.3.2020

facts
    gameOpt_ctl : groupBox.
    x_Box_ctl : groupBox.
    x_Comp_ctl : radioButton.
    x_User_ctl : radioButton.
    x_listButton_ctl : listButton.
    o_Box_ctl : groupBox.
    o_Comp_ctl : radioButton.
    o_User_ctl : radioButton.
    o_listButton_ctl : listButton.
    icon_ctl : iconControl.
    play_pushButton_ctl : button.
    stop_pushButton_ctl : button.
    x_Coor_ctl : textControl.
    y_Coor_ctl : textControl.
    home_pushButton_ctl : button.
    back_pushButton_ctl : button.
    force_pushButton_ctl : button.
    end_pushButton_ctl : button.
    info_StaticText_ctl : textControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setFont(vpi::fontCreateByName("Verdana", 10)),
        setText("xo_game"),
        setRect(rct(10, 60, 256, 350)),
        setDecoration(titlebar([closeButton, minimizeButton])),
        setBorder(thinBorder()),
        setState([wsf_ClipSiblings, wsf_ClipChildren]),
        menuSet(noMenu),
        addMouseDownListener(onMouseDown),
        addMouseMoveListener(onMouseMove),
        addShowListener(onShow),
        setPaintResponder(onPaint),
        gameOpt_ctl := groupBox::new(This),
        gameOpt_ctl:setText("Настройки"),
        gameOpt_ctl:setPosition(4, 2),
        gameOpt_ctl:setSize(160, 45),
        gameOpt_ctl:setAnchors([]),
        gameOpt_ctl:setBorderStyle(groupbox::simpleBorder()),
        gameOpt_ctl:setBorderStyle(groupBox::simpleBorder),
        x_Box_ctl := groupBox::new(gameOpt_ctl),
        x_Box_ctl:setText("x"),
        x_Box_ctl:setPosition(26, 1),
        x_Box_ctl:setSize(131, 24),
        x_Box_ctl:setAnchors([]),
        x_Box_ctl:setBorderStyle(groupBox::noBorder),
        x_Box_ctl:setTransparent(true),
        x_Comp_ctl := radioButton::new(x_Box_ctl),
        x_Comp_ctl:setText("Компьютер"),
        x_Comp_ctl:setPosition(0, 0),
        x_Comp_ctl:setWidth(50),
        x_Comp_ctl:setAnchors([]),
        x_Comp_ctl:setRadioState(radioButton::checked),
        x_User_ctl := radioButton::new(x_Box_ctl),
        x_User_ctl:setText("Человек"),
        x_User_ctl:setPosition(55, 0),
        x_User_ctl:setWidth(50),
        x_User_ctl:setAnchors([]),
        x_User_ctl:setRadioState(radioButton::unchecked),
        x_listButton_ctl := listButton::new(x_Box_ctl),
        x_listButton_ctl:setPosition(110, 0),
        x_listButton_ctl:setWidth(16),
        x_listButton_ctl:setMaxDropDownRows(9),
        x_listButton_ctl:setAnchors([]),
        x_listButton_ctl:addSelectionChangedListener(onX_listButtonSelectionChanged),
        o_Box_ctl := groupBox::new(gameOpt_ctl),
        o_Box_ctl:setText("o"),
        o_Box_ctl:setPosition(26, 18),
        o_Box_ctl:setSize(131, 24),
        o_Box_ctl:setAnchors([]),
        o_Box_ctl:setBorderStyle(groupBox::noBorder),
        o_Comp_ctl := radioButton::new(o_Box_ctl),
        o_Comp_ctl:setText("Компьютер"),
        o_Comp_ctl:setPosition(0, 0),
        o_Comp_ctl:setWidth(50),
        o_Comp_ctl:setAnchors([]),
        o_Comp_ctl:setRadioState(radioButton::unchecked),
        o_User_ctl := radioButton::new(o_Box_ctl),
        o_User_ctl:setText("Человек"),
        o_User_ctl:setPosition(55, 0),
        o_User_ctl:setWidth(50),
        o_User_ctl:setAnchors([]),
        o_User_ctl:setRadioState(radioButton::checked),
        o_listButton_ctl := listButton::new(o_Box_ctl),
        o_listButton_ctl:setPosition(110, 0),
        o_listButton_ctl:setWidth(16),
        o_listButton_ctl:setMaxDropDownRows(9),
        o_listButton_ctl:setAnchors([]),
        o_listButton_ctl:addSelectionChangedListener(onO_listButtonSelectionChanged),
        icon_ctl := iconControl::new(gameOpt_ctl),
        icon_ctl:setIcon(application_icon),
        icon_ctl:setPosition(3, 7),
        play_pushButton_ctl := button::new(This),
        play_pushButton_ctl:setText("Старт"),
        play_pushButton_ctl:setPosition(170, 5),
        play_pushButton_ctl:setWidth(33),
        play_pushButton_ctl:defaultHeight := true,
        play_pushButton_ctl:setAnchors([]),
        play_pushButton_ctl:setClickResponder(onPlay_pushButtonClick),
        stop_pushButton_ctl := button::new(This),
        stop_pushButton_ctl:setText("Стоп"),
        stop_pushButton_ctl:setPosition(203, 5),
        stop_pushButton_ctl:setWidth(33),
        stop_pushButton_ctl:defaultHeight := true,
        stop_pushButton_ctl:setAnchors([]),
        stop_pushButton_ctl:setEnabled(false),
        stop_pushButton_ctl:setClickResponder(onStop_pushButtonClick),
        x_Coor_ctl := textControl::new(This),
        x_Coor_ctl:setText("0"),
        x_Coor_ctl:setPosition(14, 51),
        x_Coor_ctl:setSize(10, 10),
        x_Coor_ctl:setAnchors([]),
        x_Coor_ctl:setVisible(false),
        y_Coor_ctl := textControl::new(This),
        y_Coor_ctl:setText("0"),
        y_Coor_ctl:setPosition(4, 62),
        y_Coor_ctl:setSize(10, 10),
        y_Coor_ctl:setAnchors([]),
        y_Coor_ctl:setVisible(false),
        home_pushButton_ctl := button::new(This),
        home_pushButton_ctl:setText("|<"),
        home_pushButton_ctl:setPosition(170, 21),
        home_pushButton_ctl:setWidth(17),
        home_pushButton_ctl:defaultHeight := true,
        home_pushButton_ctl:setAnchors([]),
        home_pushButton_ctl:setEnabled(false),
        back_pushButton_ctl := button::new(This),
        back_pushButton_ctl:setText("<"),
        back_pushButton_ctl:setPosition(187, 20),
        back_pushButton_ctl:setSize(16, 12),
        back_pushButton_ctl:defaultHeight := false,
        back_pushButton_ctl:setAnchors([]),
        back_pushButton_ctl:setEnabled(false),
        force_pushButton_ctl := button::new(This),
        force_pushButton_ctl:setText(">"),
        force_pushButton_ctl:setPosition(203, 21),
        force_pushButton_ctl:setSize(16, 12),
        force_pushButton_ctl:defaultHeight := false,
        force_pushButton_ctl:setAnchors([]),
        force_pushButton_ctl:setEnabled(false),
        end_pushButton_ctl := button::new(This),
        end_pushButton_ctl:setText(">|"),
        end_pushButton_ctl:setPosition(219, 21),
        end_pushButton_ctl:setSize(17, 12),
        end_pushButton_ctl:defaultHeight := false,
        end_pushButton_ctl:setAnchors([]),
        end_pushButton_ctl:setEnabled(false),
        info_StaticText_ctl := textControl::new(This),
        info_StaticText_ctl:setText("Hовая игра"),
        info_StaticText_ctl:setPosition(171, 37),
        info_StaticText_ctl:setSize(64, 10),
        info_StaticText_ctl:setAnchors([]),
        info_StaticText_ctl:setAlignment(alignCenter).
    % end of automatic code

end implement mainForm
