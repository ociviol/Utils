unit Utils.Files;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils.Logger, utils.Searchfiles;

type
  TCopyProgress = procedure(aCur, aMax : int64; const aMsg : String) of object;

function GetFileSize(const FileName: string): int64;
procedure CopyFile(const aSrc, aDest: string; FLog: ILog = nil; aProgress : TCopyProgress = nil);
procedure KillFolder(const aFolder: string);
function FileCount(const aFolder: string):Integer;

implementation

uses
  math;

function GetFileSize(const FileName: string): int64;
var
  sr: TRawByteSearchRec;
begin
  if FindFirst(Filename, faAnyFile, sr) = 0 then
  begin
    FindClose(sr);
    Result := sr.Size;
  end
  else
    Result := 0;
end;

procedure CopyFile(const aSrc, aDest: string; FLog: ILog = nil; aProgress : TCopyProgress = nil);
var
  sin, sout: TFileStream;

  procedure SafeOpenSourceFile;
  var
    retry: integer;
    done: boolean;
  begin
    done := False;
    retry := 0;
    sin := nil;
    repeat
      try
        sin := TFileStream.Create(aSrc, fmOpenRead);
        done := True;
      except
        on e: Exception do
        begin
          if Assigned(Flog) then
            FLog.Log('CopyFile:Error:' + e.Message);
          Inc(retry);
          sleep(50);
        end;
      end;
    until (retry >= 20) or done;
  end;

const
  blksz = (1024 * 1024) * 2;
var
  sz, cp : int64;
begin
  SafeOpenSourceFile;
  if not Assigned(sin) then
    raise Exception.Create('Utils.Files.CopyFile : Cannot open source file : ' + aSrc);
  try
    if not DirectoryExists(ExtractFilePath(aDest)) then
      ForceDirectories(ExtractFilePath(aDest));

    sz := sin.size;
    cp := blksz;
    sout := TFileStream.Create(aDest, fmCreate);
    try
      while(sz > 0) do
      begin
        cp := sout.CopyFrom(sin, ifthen(sz < blksz, sz, blksz));
        dec(sz, cp);
        if Assigned(aProgress) then
          aProgress(sin.size - sz, sin.size, 'Copying : ' + ExtractFileName(aSrc));
      end;
    finally
      sout.Free;
    end;
  finally
    sin.Free;
  end;
end;

procedure KillFolder(const aFolder: string);
var
  files: TStringList;
  s : string;
  i : integer;
begin
  try
    Files := TStringList.Create;
    try
      GetFiles(aFolder, ['*'], Files);
      for s in files do
        DeleteFile(s);
      Files.Clear;
      GetDirectories(aFolder, Files);
      for i := Files.Count - 1 downto 0 do
        RemoveDir(Files[i]);
    finally
      Files.Free;
    end;

  except
  end;
end;

function FileCount(const aFolder: string): Integer;
var
  files: TStringList;
begin
  if DirectoryExists(aFolder) then
  begin
    Files := TStringList.Create;
    try
      GetFiles(aFolder, ['*'], Files);
      result := Files.Count;
    finally
      Files.Free;
    end;
  end;
end;

end.
