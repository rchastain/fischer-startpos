
{ Programme utilisé pour produire le fichier final. Les chaînes sont rangées dans l'ordre proposé par Reinhard Scharnagl. }

uses
  SysUtils, Fischerandom;

var
  f: textfile;
  s: string;
  a: array[0..959] of string;
  i: integer;
  
begin
  Assert(PositionNumber('RNBQKBNR') = 518);
  
  AssignFile(f, 'alphabet.fen');
  Reset(f);
  Initialize(a);
  while not EOF(f) do
  begin
    ReadLn(f, s);
    i := PositionNumber(Copy(s, 36, 8));
    Assert((i >= 0) and (i <= 959));
    Assert(a[i] = '');
    a[i] := s;
  end;
  CloseFile(f);
  
  AssignFile(f, 'fischerandom.fen');
  Rewrite(f);
  for i := 0 to 959 do
    WriteLn(f, a[i]);
  CloseFile(f);
end.
