
uses
  SysUtils, Classes, Fischerandom;

var
  i, j, k, l, m: integer;
  positions: TStringList;
  
begin
  Randomize;
  
  { Une position au hasard. }
  WriteLn(StartPosition());
  
  { La position de départ traditionnelle. }
  WriteLn(StartPosition(2, 3, 3, 2, 3));
  WriteLn(StartPosition(767));
  
  { Toutes les positions possibles. }
  positions := TStringList.Create;
  positions.Sorted := TRUE;
  positions.Duplicates := dupIgnore;
  for i := 1 to 4 do { Position du premier fou. }
  for j := 1 to 4 do { Position du second fou. }
  for k := 1 to 6 do { Position de la dame. }
  for l := 1 to 5 do { Position du premier cavalier. }
  for m := 1 to 4 do { Position du second cavalier. }
    positions.Append(StartPosition(i, j, k, l, m, true, true));
  Assert(positions.Count = 960);
  positions.SaveToFile('alphabet.fen');
  positions.Free;
end.
