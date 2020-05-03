unit utils.vcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

function FindForm(aClass : TClass):TForm;

implementation

function FindForm(aClass: TClass): TForm;
var
  i : integer;
begin
  for i:=0 to Screen.FormCount-1 do
    if Screen.Forms[i].ClassType = aClass then
      exit(Screen.Forms[i]);
  result := nil;
end;

end.

