unit Utils.Searchfiles;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls
{$if defined(Linux) or defined(Darwin)}
  , cthreads
{$endif}
  ;

type
  TFilterPredicate = function(const Path: string; const SearchRec: TSearchRec): Boolean;
  TFoundFileCallback = function(const Filename : String; IsFolder : Boolean = False):TTreenode of object;
  TProgressEvent = procedure(Sender : TObject; const ProgressID : QWord; const Pos, Max : integer; const Msg : String = '') of object;
  TSearchFileOption = (sfoRecurse, sfoNoFiles, sfoFolders);
  TSearchFileOptions = set of TSearchFileOption;

  { TCancellableThread }

  TCancellableThread = Class(TThread)
  private
    FLockList : TThreadList;
  protected
    FCanceled : Boolean;
  public
    constructor Create(bCreateSuspended : Boolean); reintroduce;
    destructor Destroy; override;
    procedure Cancel(Sender : TObject);
  end;


function ThreadedSearchFiles(const Path : String;
                             const Masks : array of String;
                             CallBack : TFoundFileCallback;
                             Terminate : TNotifyEvent;
                             OnProgress : TProgressEvent = nil;
                             const str_scanning : string = '';
                             SearchFileOptions : TSearchFileOptions = [sfoRecurse]):TThread;

procedure GetDirectories(const Path : String; Dirs : TStringList);
procedure GetFiles(const Path : string; Masks : Array of String; Files : TStringList);

implementation

uses
  Utils.Masks, Utils.NaturalSortStringList, LazUTF8;

type
  TThreadSearchFiles = Class(TCancellableThread)
  private
    FPath : String;
    FMasks : array of string;
    Fstr_scanning : String;
    FCallBack : TFoundFileCallback;
    FOnProgress : TProgressEvent;
    FOptions : TSearchFileOptions;
    ProgressID : QWord;
    FCur, FMax : Integer;
    FMsg : String;
    FFile : String;
    procedure DoProgress;
    procedure DoCallBackTrue;
    procedure DoCallBackFalse;
    procedure GetDirectories(const Path : String);
    procedure GetFiles(const Path : string);
  public
    constructor Create(const Path : String;
                       const Masks : array of string;
                       CallBack : TFoundFileCallback;
                       WhenTerminate : TNotifyEvent;
                       OnProgress : TProgressEvent = nil;
                       const str_scanning : string = '';
                       SearchFileOptions : TSearchFileOptions = [sfoRecurse]);
    procedure Execute; override;
  End;

procedure GetDirectories(const Path : String; Dirs : TStringList);
var
  sr : TRawByteSearchRec;
  sPath : String;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          GetDirectories(IncludeTrailingPathDelimiter(Path) + sr.Name, Dirs);
          Dirs.Add(SysToUTF8(IncludeTrailingPathDelimiter(Path) + sr.Name));
        end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

procedure GetDirectories(const Path : String; var Dirs : TStringArray);
var
  sr : TRawByteSearchRec;
  sPath : String;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          GetDirectories(IncludeTrailingPathDelimiter(Path) + sr.Name, Dirs);
          SetLength(Dirs, length(Dirs)+1);
          Dirs[length(Dirs)-1] := SysToUTF8(IncludeTrailingPathDelimiter(Path) + sr.Name);
        end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

procedure GetFiles(const Path : string; Masks : Array of String; Files : TStringList);
var
  sr : TRawByteSearchRec;
  spath : string;
  s : string;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
      begin
         if (sr.Name <> '.') and (sr.Name <> '..') then
            GetFiles(IncludeTrailingPathDelimiter(Path) + sr.Name, Masks, Files);
      end
      else
        for s in Masks do
        begin
          if MatchesMask(sr.Name, s) then
          begin
            Files.Add(SysToUTF8(IncludeTrailingPathDelimiter(Path) + sr.Name));
            break;
          end;
        end;

    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

{ TCancellableThread }

constructor TCancellableThread.Create(bCreateSuspended: Boolean);
begin
  FLockList := TThreadList.Create;
  inherited Create(bCreateSuspended);
end;

destructor TCancellableThread.Destroy;
begin
  FLockList.Free;
  inherited Destroy;
end;

procedure TCancellableThread.Cancel(Sender: TObject);
begin
  FLockList.Locklist;
  try
    FCanceled := True;
  finally
    FLockList.UnlockList;
  end;
end;

function ThreadedSearchFiles(const Path : String;
                             const Masks : array of String;
                             CallBack : TFoundFileCallback;
                             Terminate : TNotifyEvent;
                             OnProgress : TProgressEvent = nil;
                             const str_scanning : string = '';
                             SearchFileOptions : TSearchFileOptions = [sfoRecurse]):TThread;
begin
  result := TThreadSearchFiles.Create(Path, Masks, CallBack, Terminate, OnProgress, str_scanning, SearchFileOptions);
end;

{ TThreadSearchFiles }

constructor TThreadSearchFiles.Create(const Path : String;
                                      const Masks : array of string;
                                      CallBack : TFoundFileCallback;
                                      WhenTerminate : TNotifyEvent;
                                      OnProgress : TProgressEvent = nil;
                                      const str_scanning : string = '';
                                      SearchFileOptions : TSearchFileOptions = [sfoRecurse]);
var
  i : integer;
begin
  FPath := Path;
  SetLength(FMasks, Length(Masks));
  for i := low(MAsks) to High(Masks) do
    FMAsks[i] := Masks[i];
  FOnProgress := OnProgress;
  FCallBack := CallBack;
  OnTerminate := WhenTerminate;
  FreeOnTerminate := True;
  Fstr_scanning := str_scanning;
  FOptions := SearchFileOptions;
  inherited Create(False);
end;


procedure TThreadSearchFiles.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressID, FCur, FMax, FMsg);
end;

procedure TThreadSearchFiles.DoCallBackTrue;
begin
  FCallBack(FFile, True);
end;

procedure TThreadSearchFiles.DoCallBackFalse;
begin
  FCallBack(FFile);
end;

procedure TThreadSearchFiles.GetDirectories(const Path : string);
var
  sr : TRawByteSearchRec;
  sPath : String;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          GetDirectories(IncludeTrailingPathDelimiter(Path) + sr.Name);

          if Fcanceled then Terminate;
          if Terminated then
            Exit;

          FMsg := Fstr_scanning + IncludeTrailingPathDelimiter(Path) + SysToUTF8(sr.Name);
          FFile := SysToUTF8(IncludeTrailingPathDelimiter(Path) + sr.Name);
          Synchronize(@DoCallBackTrue);
          Synchronize(@DoProgress);
          Sleep(10);
        end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

procedure TThreadSearchFiles.GetFiles(const Path : string);
var
  sr : TRawByteSearchRec;
  spath : string;
  s : string;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
      begin
         if (sr.Name <> '.') and (sr.Name <> '..') then
            GetFiles(IncludeTrailingPathDelimiter(Path) + sr.Name);
      end
      else
        for s in FMasks do
        begin
          if MatchesMask(sr.Name, s) then
          begin
            if Fcanceled then Terminate;
            if Terminated then
              Exit;

            try
              FMsg := Fstr_scanning + IncludeTrailingPathDelimiter(Path) + sr.Name;
              FFile := SysToUTF8(IncludeTrailingPathDelimiter(Path) + sr.Name);
              Synchronize(@DoCallBackFalse);
              Synchronize(@DoProgress);
              Sleep(10);
            except
            end;
            break;
          end;
        end;

    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

procedure TThreadSearchFiles.Execute;
begin
  while not Terminated do
  try
    try
      ProgressID := QWord(ThreadID) + GetTickCount64;

      FCur := 0;
      FMax := 1;
      FMsg := Fstr_scanning + '...';

      if Assigned(FOnProgress) then
        Synchronize(@DoProgress);

      if sfoFolders in FOptions then
        GetDirectories(FPath);

      if Terminated then
        Exit;

      if not (sfoNoFiles in FOptions) then
        GetFiles(FPath);

    finally
      FCur := 0;
      FMax := 0;

      if Assigned(FOnProgress) then
        Synchronize(@&DoProgress);
    end;

    if not Terminated then
      Terminate;
  except
    if not Terminated then
      Terminate;
  end;
end;

end.

