
uses
  SysUtils, Math;

function StartPosition(): string;
  
  type
    TIntegerArray = array of integer;
  
  function EmptySquares(const aRow: string; const iStart, iEnd: integer): TIntegerArray;
  var
    i: integer;
  begin
    SetLength(result, 0);
    for i := iStart to iEnd do
      if aRow[i] = '.' then
      begin
        SetLength(result, Succ(Length(result)));
        result[High(result)] := i;
      end;
  end;
  
  function CastlingRights(const iRook1, iRook2: integer): string;
  begin
    result := Concat(
      Chr(Ord('A') + Pred(iRook1)),
      Chr(Ord('A') + Pred(iRook2)),
      Chr(Ord('a') + Pred(iRook1)),
      Chr(Ord('a') + Pred(iRook2))
    );
  end;
  
var
  i, iRook1, iRook2: integer;
  arr: TIntegerArray;
  blackpieces: string;
begin
  result := StringOfChar('.', 8);
  
  result[2 * Random(4) + 1] := 'B';
  result[2 * Random(4) + 2] := 'B';
  
  arr := EmptySquares(result, 1, 8);
  i := arr[Random(4) + 1];
  result[i] := 'K';
  
  iRook1 := RandomFrom(EmptySquares(result, 1, Pred(i)));
  result[iRook1] := 'R';
  iRook2 := RandomFrom(EmptySquares(result, Succ(i), 8));
  result[iRook2] := 'R';
  
  result[RandomFrom(EmptySquares(result, 1, 8))] := 'Q';
  
  for i := 1 to 8 do
    if result[i] = '.' then
      result[i] := 'N';
  
  blackpieces := LowerCase(result);
  
  result := Concat(blackpieces, '/pppppppp/8/8/8/8/PPPPPPPP/', result, ' w ', CastlingRights(iRook2, iRook1), ' - 0 1');
end;

begin
  Randomize;
  WriteLn(StartPosition());
end.
