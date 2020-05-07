unit Utils.Files;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils.Logger;

function GetFileSize(const FileName : string) : Int64;
procedure CopyFile(const aSrc, aDest : String; FLog : ILog = nil);

implementation

function GetFileSize(const FileName : string) : Int64;
var
  sr : TRawByteSearchRec;
begin
  if FindFirst (Filename, faAnyFile, sr) = 0 then
  begin
    FindClose(sr);
    result := sr.Size;
  end
  else
    result := 0;
end;

procedure CopyFile(const aSrc, aDest: String; FLog : ILog = nil);
var
  sin, sout : TFileStream;
  procedure SafeOpenSourceFile;
  var
    retry : integer;
    done : boolean;
  begin
    done := false;
    retry := 0;
    sin := nil;
    repeat
      try
        sin := TFileStream.Create(aSrc, fmOpenRead);
        done := true;
      except
        on e: exception do
        begin
          if Assigned(Flog) then
            FLog.Log('CopyFile:Error:'+e.Message);
          inc(retry);
          sleep(50);
        end;
      end;
    until (retry >=20) or done;
  end;

begin
  SafeOpenSourceFile;
   if not Assigned(sin) then
     raise Exception.Create('Utils.Files.CopyFile : Cannot open source file : ' + aSrc);
  try
    if not DirectoryExists(ExtractFilePath(aDest)) then
      ForceDirectories(ExtractFilePath(aDest));
    sout := TFileStream.Create(aDest, fmCreate);
    try
       sout.CopyFrom(sin, sin.Size);
    finally
      sout.free;
    end;
  finally
    sin.Free;
  end;
end;

end.

