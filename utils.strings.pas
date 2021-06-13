unit Utils.Strings;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils.NaturalSortStringList;

type

  { TThreadStringList }

  TThreadStringList = Class(TThreadList)
  private
    FStringlist : TNaturalSortStringList;
    function Get(Index: Integer): string;
    function GetCount: Integer;
    function GetObject(Index: Integer): TObject;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    procedure Put(Index: Integer; AValue: string);
    procedure PutObject(Index: Integer; AObject: TObject);
    procedure SetOnChanging(AValue: TNotifyEvent);
    procedure SetSorted(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function HasObject(aObject: TObject): Boolean;
    procedure Clear;
    function  LockList: TStringList;  reintroduce;
    procedure Delete(index : integer); overload;
    procedure UnlockList;  reintroduce;
    procedure AddObject(const AVAlue : STring; AObject : TObject);
    function IndexOf(const AValue : String):Integer;
    procedure Sort;

    property Count : Integer read GetCount;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Sorted : Boolean read GetSorted write SetSorted;
    property OnChanging : TNotifyEvent read GetOnChanging Write SetOnChanging;
  end;


//Functions

implementation

{ TThreadStringList }

constructor TThreadStringList.Create;
begin
  FStringlist := TNaturalSortStringList.Create;
  inherited Create;
end;

destructor TThreadStringList.Destroy;
begin
  FStringlist.Free;
  inherited Destroy;
end;

function TThreadStringList.GetObject(Index: Integer): TObject;
begin
  with LockList do
  try
    if index < FStringlist.Count then
      result := FStringlist.Objects[index]
    else
    result := nil;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.PutObject(Index: Integer; AObject: TObject);
begin
  with LockList do
  try
    FStringlist.Objects[index] := AObject;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetSorted(AValue: Boolean);
begin
  with LockList do
  try
    FStringlist.Sorted := AVAlue;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Put(Index: Integer; AValue: string);
begin
  with LockList do
  try
    FStringlist[index] := aValue;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetOnChanging(AValue: TNotifyEvent);
begin
  with LockList do
  try
    FStringlist.OnChanging := AValue;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetSorted: Boolean;
begin
  with LockList do
  try
    result := FStringlist.Sorted;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCount: Integer;
begin
 with LockList do
  try
    result := FStringlist.Count;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Get(Index: Integer): string;
begin
  with LockList do
  try
    if index < FStringlist.Count then
      result := FStringlist[Index]
    else
      result := '';
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetOnChanging: TNotifyEvent;
begin
  with LockList do
  try
    result := FStringlist.OnChanging;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.HasObject(aObject: TObject): Boolean;
var
  i : integer;
begin
  with LockList do
  try
    for i := 0 to FStringlist.Count - 1 do
      if FStringlist.Objects[i] = aObject then
        exit(True);
    result := false;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Clear;
begin
  with LockList do
  try
    FStringlist.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.LockList: TStringList;
begin
  inherited LockList;
  result := FStringlist;
end;

procedure TThreadStringList.Delete(index: integer);
begin
  with LockList do
  try
    FStringlist.Delete(index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.UnlockList;
begin
  inherited UnlockList;
end;

procedure TThreadStringList.AddObject(const AVAlue : STring; AObject : TObject);
begin
  with LockList do
  try
    FStringlist.AddObject(AValue, AObject);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.IndexOf(const AValue: String): Integer;
begin
  with LockList do
  try
    result := FStringlist.IndexOf(AValue);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Sort;
begin
  with LockList do
  try
    FStringlist.Sort;
  finally
    UnlockList;
  end;
end;

{
function AnsiNaturalCompareStrings(str1, str2: string; vCaseSensitive: boolean = True): integer;
var
  l1, l2: integer; //Str length
  n1, n2: integer; //numrical part
  i1, i2: integer; //index in Str
  d: integer;
begin
  if not vCaseSensitive then
  begin
    str1 := UpperCase(str1);
    str2 := UpperCase(str2);
  end;

  l1 := Length(str1);
  l2 := Length(str2);

  i1 := 1;
  i2 := 1;
  while i1 <= l1 do
  begin
    //Compare non-numbers
    d := Ord(str1[i1]) - Ord(str2[i2]);
    if not (str1[i1] in ['0'..'9']) then
    begin
      if (d <> 0) then
      begin
        Result := d;
        exit;
      end;
    end
    else
    begin
      //Convert a section of str1 to a number
      n1 := 0;
      repeat
        n1 := 10 * n1 + Ord(str1[i1]) - Ord('0');
        Inc(i1);
      until (i1 > l1) or not (str1[i1] in ['0'..'9']);

      //Convert a section of str2 to a number
      n2 := 0;
      repeat
        n2 := 10 * n2 + Ord(str2[i2]) - Ord('0');
        Inc(i2);
      until (i2 > l2) or not (str2[i2] in ['0'..'9']);

      //Compare numbers naturally
      d := n1 - n2;
      if d <> 0 then
      begin
        Result := d;
        exit;
      end
      else
        Continue;
    end;
    Inc(i1);
    Inc(i2);
  end;
  Result := (i1 - l1) - (i2 - l2);
end;
}
end.

