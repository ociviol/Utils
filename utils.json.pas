unit utils.json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TJsonObject }

  TJsonObject = Class(TPersistent)
  public
    class function Load(const aFileName : String; aObject : TObject):TObject;
    procedure Save(const aFileName: String);
  end;

implementation

uses
  fpjson, fpjsonrtti;

{ TJsonObject }

class function TJsonObject.Load(const aFileName: String; aObject : TObject): TObject;
var
  DeStreamer: TJSONDeStreamer;
  t : TStringList;
begin
  result := aObject;
  try
    if FileExists(aFileName) then
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        t := TStringList.Create;
        try
          t.LoadFromFile(aFileName);
          DeStreamer.JSONToObject(t[0], result);
        finally
          t.Free;
        end
      finally
        DeStreamer.Free;
      end;
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
  if not DirectoryExists(ExtractFilePath(aFileName)) then
    ForceDirectories(ExtractFilePath(aFileName));

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

