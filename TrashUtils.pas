unit TrashUtils;

{$mode objfpc}{$H+}

interface

function MoveToTrash(const AFile: string): Boolean;

implementation

uses
  Classes, SysUtils, Process
  {$IFDEF Windows}, Windows, ShellApi{$ENDIF}
  {$IFDEF Darwin}, BaseUnix{$ENDIF};

function MoveToTrash(const AFile: string): Boolean;
{$IFDEF Windows}
var
  FileOp: TSHFileOpStruct;
  FromBuf: array[0..MAX_PATH] of Char;
{$ENDIF}
{$IFDEF UNIX}
var
  Proc: TProcess;
{$ENDIF}
begin
  Result := False;

  if not FileExists(AFile) then Exit;

  {$IFDEF Windows}
  // Use SHFileOperation (sends to Recycle Bin)
  FillChar(FileOp, SizeOf(FileOp), 0);
  FillChar(FromBuf, SizeOf(FromBuf), 0);

  StrPCopy(FromBuf, AFile);

  FileOp.Wnd := 0;
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := @FromBuf;
  FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;

  Result := (SHFileOperation(FileOp) = 0);
  {$ENDIF}

  {$IFDEF Darwin}
  // macOS: use Finder via AppleScript (reliable trash behavior)
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := '/usr/bin/osascript';
    Proc.Parameters.Add('-e');
    Proc.Parameters.Add(
      'tell application "Finder" to delete POSIX file "' + AFile + '"'
    );
    Proc.Options := [poWaitOnExit];
    Proc.Execute;
    Result := (Proc.ExitStatus = 0);
  finally
    Proc.Free;
  end;
  {$ENDIF}

  {$IFDEF Linux}
  // Linux: use gio trash (freedesktop standard)
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'gio';
    Proc.Parameters.Add('trash');
    Proc.Parameters.Add(AFile);
    Proc.Options := [poWaitOnExit];
    Proc.Execute;
    Result := (Proc.ExitStatus = 0);
  finally
    Proc.Free;
  end;
  {$ENDIF}
end;

end.
