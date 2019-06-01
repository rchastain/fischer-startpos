
unit Fischerandom;

interface

{ Positions de départ aux échecs de Fischer, par la méthode de Hans Bodlaender. }
function StartPosition(a, b, c, d, e: integer; const AFullFEN: boolean = FALSE; const ASchredderFEN: boolean = FALSE): string; overload;
function StartPosition(i: integer): string; overload;
function StartPosition(): string; overload;

{ Numérotation des positions de départ aux échecs de Fischer, par la méthode de Reinhard Scharnagl. }
function PositionNumber(const AWhitePieces: string): integer;

implementation

function StartPosition(a, b, c, d, e: integer; const AFullFEN: boolean; const ASchredderFEN: boolean): string; overload;

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
  castling: string;
begin
  if (a < 1) or (a > 4)
  or (b < 1) or (b > 4)
  or (c < 1) or (c > 6)
  or (d < 1) or (d > 5)
  or (e < 1) or (e > 4) then
    exit('');
  
  result := StringOfChar('.', 8);
  
  result[2 * a - 1] := 'B';
  result[2 * b] := 'B';
  
  i := 1; while result[i] <> '.' do Inc(i);
  while c > 1 do
  begin
    Inc(i); while result[i] <> '.' do Inc(i);
    Dec(c);
  end;
  result[i] := 'Q';
  
  i := 1; while result[i] <> '.' do Inc(i);
  while d > 1 do
  begin
    Inc(i); while result[i] <> '.' do Inc(i);
    Dec(d);
  end;
  result[i] := 'N';
  i := 1;
  while result[i] <> '.' do Inc(i);
  while e > 1 do
  begin
    Inc(i);
    while result[i] <> '.' do Inc(i);
    Dec(e);
  end;
  result[i] := 'N';
  
  i := 1;
  while result[i] <> '.' do Inc(i);
  result[i] := 'R';
  iRook1 := i;
  while result[i] <> '.' do Inc(i);
  result[i] := 'K';
  while result[i] <> '.' do Inc(i);
  result[i] := 'R';
  iRook2 := i;
  
  if AFullFEN then
  begin
    if ASchredderFEN then
      castling := CastlingRights(iRook2, iRook1)
    else
      castling := 'KQkq';
    result := Concat(
      LowerCase(result),
      '/pppppppp/8/8/8/8/PPPPPPPP/',
      result,
      ' w ',
      castling,
      ' - 0 1'
    );
  end;
end;

function StartPosition(i: integer): string; overload;
var
  a, b, c, d, e, f: integer;
begin
  if (i < 1) or (i > 1920) then
    exit('');
  
  f := i - 1;
  a := (f div 480) + 1;
  f := f mod 480;
  b := (f div 120) + 1;
  f := f mod 120;
  c := (f div 20) + 1;
  f := f mod 20;
  d := (f div 4) + 1;
  f := f mod 4;
  e := f + 1;
  
  result := StartPosition(a, b, c, d, e);
end;

function StartPosition(): string; overload;
begin
  {
  result := StartPosition(
    Random(4) + 1,
    Random(4) + 1,
    Random(6) + 1,
    Random(5) + 1,
    Random(4) + 1,
    aShredderFEN
  );
  }
  result := StartPosition(Random(1920) + 1);
end;

function PositionNumber(const AWhitePieces: string): integer;
const
  CNotFound = -1;
var
  whiteSquareBishopPos: CNotFound..3;
  blackSquareBishopPos: CNotFound..3;
  queenPos: CNotFound..5;
  krnCode: 0..9;
  i, x: integer;
  s: string; 
begin
  Assert(Length(AWhitePieces) = 8);
  
  whiteSquareBishopPos := CNotFound;
  blackSquareBishopPos := CNotFound;
  queenPos := CNotFound;
  
  i := 0;
  while (i < 4) and (whiteSquareBishopPos = CNotFound) do
    if AWhitePieces[2 * i + 2] = 'B' then
      whiteSquareBishopPos := i
    else
      Inc(i);
  
  i := 0;
  while (i < 4) and (blackSquareBishopPos = CNotFound) do
    if AWhitePieces[2 * i + 1] = 'B' then
      blackSquareBishopPos := i
    else
      Inc(i);
  
  Assert(whiteSquareBishopPos > CNotFound);
  Assert(blackSquareBishopPos > CNotFound);
  
  i := 0;
  x := 1;
  while (x < 9) and (queenPos = CNotFound) do
  begin
    if AWhitePieces[x] = 'Q' then
      queenPos := i
    else
    begin
      Inc(x);
      if AWhitePieces[Pred(x)] <> 'B' then
        Inc(i);
    end;
  end;
  
  s := '';
  x := 1;
  repeat
    if AWhitePieces[x] in ['N', 'R', 'K'] then
      s := s + AWhitePieces[x];
    Inc(x)
  until x > 8;
  
  if s = 'NNRKR' then krnCode := 0 else
  if s = 'NRNKR' then krnCode := 1 else
  if s = 'NRKNR' then krnCode := 2 else
  if s = 'NRKRN' then krnCode := 3 else
  if s = 'RNNKR' then krnCode := 4 else
  if s = 'RNKNR' then krnCode := 5 else
  if s = 'RNKRN' then krnCode := 6 else
  if s = 'RKNNR' then krnCode := 7 else
  if s = 'RKNRN' then krnCode := 8 else
  if s = 'RKRNN' then krnCode := 9 else
    Assert(FALSE);
  
  result := whiteSquareBishopPos + 4 * blackSquareBishopPos + 16 * queenPos + 96 * krnCode;
end;

end.
