unit utils.zipstream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils.zipfile;

type

  { TZipStream }
  TZipFileStream = Class(TStream)
  end;

  TZipStream = Class(TObject)
  private
    FFilename : String;
    FZipFile : TZipFile;
    function GetCount: Integer;
    function GetReadStream(Index: Integer): TStream;
    function GetWriteStream(Index: Integer): TStream;
  public
    constructor Create(const Filename : String);
    destructor Destroy; override;

    procedure CreateFile(const Filename : String);
    function  BeginWrite(Index : Integer):TStream;
    procedure EndWrite(Index : Integer; aStream : TStream);
    property Count:Integer read GetCount;
    property ReadStream[Index:Integer]:TStream read GetReadStream;
    property WriteStream[Index:Integer]:TStream read GetWriteStream;
  end;

implementation

{ TZipStream }


constructor TZipStream.Create(const Filename: String);
begin
  inherited Create;
  FZipFile := TZipFile.Create;
end;

destructor TZipStream.Destroy;
begin
  FZipFile.Free;
  inherited Destroy;
end;

procedure TZipStream.CreateFile(const Filename: String);
begin

end;

function TZipStream.BeginWrite(Index: Integer): TStream;
begin

end;

procedure TZipStream.EndWrite(Index: Integer; aStream: TStream);
begin

end;

function TZipStream.GetCount: Integer;
begin
  result := FZipFile.FileCount;
end;

function TZipStream.GetReadStream(Index: Integer): TStream;
begin
  result := FZipFile.GetFileStream(Index);
end;

function TZipStream.GetWriteStream(Index: Integer): TStream;
begin

end;


procedure TZipStream.AddFile(const Filename: String);
begin

end;

end.

