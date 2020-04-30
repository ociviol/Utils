unit utils.treeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

type

  { TTreeNodeEx }

  TTreeNodeEx = Class Helper for TTreeNode
  private
    function GetPath: String;
  protected
    property Path: String read GetPath;
  End;

  { TTreeviewEx }

  TTreeviewEx = Class helper for TTreeView
  public
    procedure AddFilePath(const aFilename : String; aObject : TObject = nil);
  end;

implementation

{ TTreeviewEx }

procedure TTreeviewEx.AddFilePath(const aFilename: String; aObject: TObject);
var
  ar : TStringArray;
  i : integer;
  n : TTreeNode;
begin
  ar := aFilename.Split([PathDelim]);

  n := Items.FindNodeWithText(ar[0]);
  if not assigned(n) then
    n := Items.AddChild(nil, ar[0]);

  for i := low(ar)+1 to High(ar) do
  begin
    n := n.FindNode(ar[i]);
    if not assigned(n) then
      n := Items.AddChild(n, ar[i]);
  end;
end;

{ TTreeNode }

function TTreeNodeEx.GetPath: String;
var
  n: TTreeNode;
begin
  n := Self;
  Result := '';
  while Assigned(n) do
  begin
    if Result = '' then
      Result := Text
    else
      Result := n.Text + PathDelim + Result;
    n := n.Parent;
  end;
end;


end.

