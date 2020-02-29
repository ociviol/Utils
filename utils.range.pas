unit utils.range;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRange }

  TRange = Class
  public
    Class function EncodePoint(const aPoint : TPoint):String;
    class function DecodePoint(const aVal : String):TPoint;
  end;

implementation

{ TRange }

class function TRange.EncodePoint(const aPoint: TPoint): String;
begin

end;

class function TRange.DecodePoint(const aVal: String): TPoint;
const
  CS_LETTERS = ['A'..'Z'];
  CS_NUMBERS = ['0'..'9'];

  function ColumnLetterToColumnIndex(const cell : String):Integer;
  var
    i : integer;
  begin
    Result := 0;

    for i := 1 to Length(cell) do
    begin
      Result := Result * 26;
      Inc(Result, ord(cell[i]) - ord('A'));
    end;
  end;

  function GetChar(var Line : String):String; inline;
  begin
    result := Line[1];
    Line := Copy(Line, 2, Length(Line));
  end;

  function GetCellCol(Cell : String):Integer;
  var
    s : string;
  begin
    result := 0;
    s := '';
    while length(Cell) > 0 do
    begin
      if (Cell[1] in CS_LETTERS) then
        s := s + GetChar(Cell)
      else
        break;
    end;
    result := ColumnLetterToColumnIndex(s);
  end;

  function GetCellRow(Cell : String):Integer;
  var
   s : String;
  begin
   result := 0;
   s := '';
   while length(Cell) > 0 do
   begin
     if (Cell[1] in CS_NUMBERS) then
       s := s + GetChar(Cell)
     else
       Cell := Copy(Cell, 2, Length(Cell));
   end;
   result := StrToInt(s)-1;
  end;

  function MakePoint:TPoint;
  var
    c : String;
  begin
    c := aVal;
    Result.x := GetCellCol(c);
    Result.y := GetCellRow(c);
  end;
begin
  result := MakePoint;
end;

end.

