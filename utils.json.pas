unit utils.json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TJsonObject }

  TJsonObject = Class(TPersistent)
  public
    class function Load(const aFileName : String; aClass : TClass):TObject;
    procedure Save(const aFileName : String);
  end;

implementation

uses
  fpjson, fpjsonrtti;

{ TJsonObject }

class function TJsonObject.Load(const aFileName: String; aClass : TClass): Tobject;
var
  DeStreamer: TJSONDeStreamer;
  t : TStringList;
begin
  result := aClass.Create;
  try
    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      if FileExists(aFileName) then
      begin
        t := TStringList.Create;
        try
          t.LoadFromFile(aFileName);
          DeStreamer.JSONToObject(t[0], result);
        finally
          t.Free;
        end
      end;
    finally
      DeStreamer.Free;
    end;
  except
  end;
end;

procedure TJsonObject.Save(const aFileName: String);
var
  Streamer: TJSONStreamer;
  s : String;
  t : TStringList;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray]; // Save strings as JSON array
    // JSON convert and output
    s := Streamer.ObjectToJSONString(Self);
    t := TStringList.Create;
    try
      t.add(s);
      t.SaveToFile(aFileName);
    finally
      t.free;
    end;
  finally
    Streamer.Free;
  end;
end;

end.

