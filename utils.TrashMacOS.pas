unit utils.TrashMacOS;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

function MoveToTrashMac(const AFile: string): Boolean;

implementation

uses
  SysUtils,
  Classes,
  Process,
  CocoaAll;

function MoveToTrashMac(const AFile: string): Boolean;
var
  FileManager: NSFileManager;
  FileURL, ResultURL: NSURL;
  Error: NSError;
  Proc: TProcess;
begin
  Result := False;

  if not FileExists(AFile) then Exit;

  // --- 1. Native Cocoa attempt ---
  FileManager := NSFileManager.defaultManager;
  FileURL := NSURL.fileURLWithPath(NSSTR(AFile));
  ResultURL := nil;
  Error := nil;

  if FileManager.trashItemAtURL_resultingItemURL_error(
       FileURL,
       @ResultURL,
       @Error
     ) then
  begin
    Exit(True);
  end;

  // Optional: log native error
  if Assigned(Error) then
    WriteLn('Cocoa trash failed: ',
      UTF8Encode(Error.localizedDescription.UTF8String));

  // --- 2. Fallback: Finder via AppleScript ---
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := '/usr/bin/osascript';
    Proc.Parameters.Add('-e');
    Proc.Parameters.Add(
      'tell application "Finder" to delete POSIX file "' +
      StringReplace(AFile, '"', '\"', [rfReplaceAll]) +
      '"'
    );
    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Result := (Proc.ExitStatus = 0);
  finally
    Proc.Free;
  end;
end;

end.