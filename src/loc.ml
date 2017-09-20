

type t = {
        _start : Lexing.position;
        _end : Lexing.position;
    }

let dummy = {
        _start = Lexing.dummy_pos;
        _end = Lexing.dummy_pos;
    }

let check { _start; _end } =
    assert (_start.pos_fname = _end.pos_fname);
    assert (_start.pos_lnum <= _end.pos_lnum);
    if _start.pos_lnum = _end.pos_lnum then
        assert (_start.pos_cnum <= _end.pos_cnum)

let to_string ({ _start; _end } as loc) =
    check loc;
    let file = _start.pos_fname
    and start_line = _start.pos_lnum
    and end_line = _end.pos_lnum
    and start_col = _start.pos_cnum - _start.pos_bol + 1
    and end_col = _end.pos_cnum - _end.pos_bol + 1
    in
    if start_line = end_line then begin
        assert (start_col <= end_col);
        Printf.sprintf "file %s, line %d, col %d-%d"
            file start_line start_col end_col
    end else begin
        assert (start_line < end_line);
        Printf.sprintf "file %s, line %d to %d"
            file start_line end_line
    end

let join { _start } { _end} =
    let l = { _start; _end } in
    check l;
    l
