
uses
  SysUtils, Cairo;

procedure CreateImage(const aPlacement: string; const aFileName: string; const aScale: integer);
  
  function ChessAlphaSymbol(const aPieceSymbol: char; const aDarkSquare: boolean): char;
  const
    T1 = 'PNBRQKpnbrqk ';
    T2 = 'phbrqkojntwl ';
    T3 = 'PHBRQKOJNTWL+';
  var
    i: integer;
  begin
    i := Pos(aPieceSymbol, T1);
    if i = 0 then
      result := aPieceSymbol
    else
      if aDarkSquare then
        result := T3[i]
      else
        result := T2[i];
  end;

var
  cr: pcairo_t;
  sf: pcairo_surface_t;
  x, y: integer;
begin
  sf := cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 10 * aScale, 10 * aScale);
  cr := cairo_create(sf);
  cairo_select_font_face(cr, 'Chess Alpha', CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr, aScale);
  cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
  for x := 1 to 10 do
    for y := 1 to 10 do
    begin
      cairo_move_to(cr, aScale * (x - 1), aScale * (11 - y));
      cairo_show_text(cr, pchar(string(ChessAlphaSymbol(aPlacement[x + 10 * Pred(y)], not Odd(x + y)))));
    end;
  cairo_surface_write_to_png(sf, pchar(aFileName));
  cairo_destroy(cr);
  cairo_surface_destroy(sf);
end;

const
  PLACEMENT =
    '6777777778' +
    '4%s5' +
    '4PPPPPPPP5' +
    '4        5' +
    '4        5' +
    '4        5' +
    '4        5' +
    '4pppppppp5' +
    '4%s5' +
    '1222222223';
  
begin
  CreateImage(Format(PLACEMENT, [ParamStr(1), LowerCase(ParamStr(1))]), ParamStr(2), StrToInt(ParamStr(3)));
end.
