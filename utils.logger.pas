unit Utils.Logger;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Forms
{$if defined(Linux) or defined(Darwin)}
  ,cthreads
{$endif}
  ;

type
  TOnFlush = procedure(const aStream : TStringStream) of object;

  ILog = interface
  ['{36573377-D6D3-42F0-BD07-5ED2806D392E}']
    procedure Log(const msg : string);
    procedure SetActive(const bState : boolean; aOwner : TForm);
    function ArchivePath:String;
    //procedure AttachLog(aOwner : TForm);
    //procedure DetachLog;
  end;

function GetIlog(const Filename : string; Activate : Boolean = True; MaxLogSizeMb : integer = 10; aUDP : Boolean = false):ILog;
function MakeLong(aHighWord, aLowWord : word):longint; inline;
function LowWord(Value : longint):Integer; inline;
function HighWord(Value : longint):Integer; inline;

implementation

uses
{$if defined(Linux) or defined(Darwin)}
  unix,
{$endif}
  Controls,
  StdCtrls,
  ExtCtrls,
  DateUtils,
  Utils.Files,
  Utils.Searchfiles,
  UTils.Zipfile
  //blcksock,
  //uXmlDoc
  ;

type
  { TLogList }

  TLogList = Class(TThreadList)
  private
    //FUDP : TUDP;
    FBuffer : TStringStream;
    FClientList : TStringlist;

    //procedure OnServerReceive(const Data : String; Socket : TUDPBlockSocket);
  public
    constructor Create(aUDP : Boolean = false);
    destructor Destroy; override;
    function GetSize:Integer;
    procedure AddItem(const Msg : string);
    procedure Dump(aDest : TStream);
  end;

  TLogThread = Class(TThread)
  private
    FDayStarted : integer;
    FMaxLogSize : integer;
    FOriginalFilename,
    FFilename : String;
    FList : TLogList;
    FStartedDate : TDateTime;

    procedure ZipLog(const aFilename : string);
    function GetDay:Integer;
    function MakeFilename(const Filename: String): String;
    function GetArchivePath:String; inline;
    procedure Log(const msg: string);
  public
    constructor Create(const Filename : String; List : TLogList; MaxLogSizeMb : integer);
    procedure Execute; override;
    procedure Dump;
  End;

  { TLog }

  TLog = Class(TInterfacedObject, Ilog)
  private
    FFilename : String;
    FList : TLogList;
    FLogThread : TLogThread;
    FActive : Boolean;
    FMaxLogSizeMb : Integer;
    FLogpanel,
    FLogToolPanel: TPanel;
    FLogList : TListbox;
    FLock : TThreadList;
    //FOnFlush : TOnFlush;

    procedure StopThread;
    procedure ZipLogs;
    procedure DefBtnClick(Sender : Tobject);
  public
    constructor Create(const Filename : String; Activate : Boolean; MaxLogSizeMb : integer; aUDP : Boolean = False);
    destructor Destroy; override;
    procedure Log(const msg : string);
    procedure Dump;
    procedure SetActive(const bState : boolean; {%H-}aOwner : TForm = nil);
    function ArchivePath:String;
    //procedure AttachLog(aOwner : TForm);
    //procedure DetachLog;
  End;


function GetIlog(const Filename: string; Activate: Boolean;
  MaxLogSizeMb: integer; aUDP: Boolean): ILog;
begin
  result := TLog.Create(Filename, Activate, MaxLogSizeMb, aUDP) as ILog;
end;

function MakeLong(aHighWord, aLowWord : word):longint; inline;
begin
  result := aHighWord shl 16;
  inc(result, aLowWord);
end;

function LowWord(Value : longint):Integer; inline;
begin
  result := (Value shl 16);
  result := result shr 16;
end;

function HighWord(Value : longint):Integer; inline;
begin
  Result := (Value shr 16);
end;

function SafeOpen(const Filename : string; Mode : Word):TFileStream;
var
  retry : integer;
begin
  retry := 0;
  result := nil;
  repeat
    try
      result := TFileStream.Create(Filename, Mode);
      retry := -1;
    except
      inc(retry);
      sleep(100);
    end;
  until (retry < 0) or (retry > 4);
end;

function SafeDelete(const Filename : string):Boolean;
var
  retry : integer;
begin
  retry := 0;
  repeat
    try
      DeleteFile(Filename);
      retry := -1;
    except
      inc(retry);
      sleep(100);
    end;
  until (retry < 0) or (retry > 4);
  result := retry < 0;
end;

procedure CheckArchivedLogs(const Filename, ArchivePath : String);
var
  files : TStringList;
  mask : string;
begin
  mask := '*' + ChangeFileExt(ExtractFilename(Filename), '.log');
  files := TStringlist.Create;
  try
    GetFiles(ArchivePath, mask, Files);
    while files.count > 50 do
    begin
      if not SafeDelete(files[0]) then
        Exit;
      Files.Clear;
      GetFiles(ArchivePath, mask, Files);
    end;
  finally
    Files.Free;
  end;
end;

{ TLogList }

procedure TLogList.AddItem(const Msg: string);
begin
  with LockList do
  try
    FBuffer.WriteString(Msg + sLineBreak);
  finally
    UnlockList;
  end;
end;

{
procedure TLogList.OnServerReceive(const Data: String; Socket: TUDPBlockSocket);
var
  ln, i : integer;
  ip : string;
  Doc : TXmlDoc;
begin
  Doc := TXmlDoc.Create;
  try
    Doc.AsString := Data;
    with Doc.DocumentElement do
      for i := 0 to NbElements - 1 do
        if Elements[i].Text = 'register' then
        begin
          ip := Elements[i].GetAttribute('IP');
          FClientList.Add(ip);
          //FClientList.Add(Elements[i].GetAttribute('IP') + '=0');
        end;
  finally
    Doc.Free;
  end;
end;
}

constructor TLogList.Create(aUDP: Boolean);
begin
  FClientList := TStringlist.Create;
  FBuffer := TStringStream.Create('', TEncoding.UTF8);
  {
  if aUDP then
  begin
    FUDP := TUDP.Create(@OnServerReceive);
    FUDP.StartServer;
  end;
  }
  inherited Create;
end;

destructor TLogList.Destroy;
begin
  FBuffer.Free;
  FClientList.Free;
  //if Assigned(FUDP) then
  //  FUDP.Free;
  inherited;
end;

procedure TLogList.Dump(aDest: TStream);
//var
//  Doc : TXmlDoc;
//  Docs,
//  s : string;
begin
  with LockList do
  try
    try
      aDest.Seek(0, soEnd);
      FBuffer.Position := 0;
      {
      if Assigned(FUDP) then
      begin
        Doc := TXmlDoc.Create;
        try
          with doc.CreateNewDocumentElement('data') do
            Text := FBuffer.DataString;
          Docs := Doc.AsString;

          for s in FClientList do
          begin
            FUdp.ConnectToServer(s);
            FUDP.Send(Docs);
            FUDP.Disconnect;
          end;
        finally
          Doc.Free;
        end;
        FBuffer.Position:=0;
      end;
      }
      FBuffer.SaveToStream(aDest);
 //     aDest.CopyFrom(FBuffer, FBuffer.Size);
      FBuffer.Size := 0;
    except
    end;
  finally
    UnlockList;
  end;
end;

function TLogList.GetSize: Integer;
begin
  with LockList do
  try
    result := FBuffer.Size;
  finally
    UnlockList;
  end;
end;

{ TLog }

procedure TLog.Log(const msg: string);
var
  s : string;
begin
  if FActive then
  begin
    s := Format('%s Thread ID : %.8d : %s',
                [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now),
                QWord(GetCurrentThreadId()), Msg]);

    FLock.lockList;
    try
      try
        if Assigned(FLogList) then
          FLogList.Items.Add(s);
      except
      end;
    finally
      Flock.Unlocklist;
    end;

    FList.AddItem(s);
  end;
end;

constructor TLog.Create(const Filename: String; Activate : Boolean; MaxLogSizeMb : integer; aUDP : Boolean);
begin
  inherited Create;
  FFilename := Filename;
  FActive := Activate;
  FList := TLogList.Create(aUDP);
  FMaxLogSizeMb := MaxLogSizeMb;
  FLogpanel := nil;
  FLogToolPanel := nil;
  FLogList  := nil;
  FLock := TThreadList.Create;
  if FActive then
    FLogThread := TLogThread.Create(Filename, FList, MaxLogSizeMb)
  else
    FLogThread := nil;
  Log('Logger created.');
end;

destructor TLog.Destroy;
begin
  Log('Logger destroying.');
  StopThread;
  FList.Free;
  ZipLogs;
  CheckArchivedLogs(FFilename, ArchivePath);
  FLock.Free;
  inherited;
end;

procedure TLog.SetActive(const bState: boolean; aOwner : TForm = nil);
begin
  if Factive = bState then
    Exit;

  if Factive and not bState then
  begin
    Log('Logger deactivated.');
    StopThread;
    if Assigned(FLogpanel) then
    begin
      FLogPanel.Visible := False;
    end;
  end;

  if not Factive and bState then
  begin
    //if Assigned(aOwner) then
    //  AttachLog(aOwner);

    Log('Logger activated.');
    FLogThread := TLogThread.Create(FFilename, FList, FMaxLogSizeMb);
  end;

  FActive := bState;
end;

procedure TLog.Dump;
begin
  if Assigned(FLogThread) then
    FLogThread.Dump;
end;

function TLog.ArchivePath: String;
begin
  result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(FFilename)) + 'Archives');
  if not DirectoryExists(result) then
    ForceDirectories(result);
end;

(*
procedure TLog.AttachLog(aOwner: TForm);
begin
  if Assigned(FLogpanel) then
    FLogpanel.Visible := True
  else
  begin
    Log('Logger attaching to : ' + aOwner.Name);
    FLogpanel := TPanel.Create(aOwner);
    with FLogpanel do
    begin
      name := 'LoggerPnl';
      FLogpanel.Align:=alBottom;
      FLogpanel.Height:=25;
      Caption := '';
    end;

    FLogToolPanel := TPanel.Create(aOwner);
    with FLogToolPanel do
    begin
      name := 'LoggerPnlTool';
      Align:=alTop;
      Height := 22;
      Caption := '';
      Parent := FLogPanel;
    end;
    with tButton.Create(aOwner) do
    begin
      align := alRight;
      name := 'LoggerPnlToolBtn';
      Caption := 'Show';
      Parent := FLogToolPanel;
      OnCLick := @DefBtnClick;
    end;

    FLogList := TListbox.Create(aOwner);
    with FLogList do
    begin
      Name:='LoggerList'+IntToStr(GetTickCount64);
      Align:=alClient;
      Parent := FLogpanel;
    end;
    FlogPanel.Visible := True;
    FlogPanel.Parent := aOwner;
  end;
end;

procedure TLog.DetachLog;
begin
  FlogPanel := nil;
  FLogList := nil;
  Log('Logger detached from host window.');
end;
*)

procedure TLog.StopThread;
begin
  if Assigned(FLogThread) then
  begin
    with FLogThread do
    begin
      Terminate;
      WaitFor;
    end;
    FreeAndNil(FLogThread);
  end;
end;

procedure TLog.ZipLogs;
var
  f : string;
  files : TStringList;
  st : TFileStream;
begin

  files := TStringlist.Create;
  try
    GetFiles(ExtractFilePath(FFilename), ['*.log'], Files);
    if files.Count > 0 then
      for f in files do
        if f.Contains(ExtractFileName(FFilename)) then
        try
          RenameFile(f, ArchivePath + ExtractFileName(f));
          //CopyFile(f, ArchivePath + ExtractFileName(f));
        except
        end;
      {
        begin
          st := SafeOpen(f, fmOpenRead or fmShareDenyWrite);
          if Assigned(st) then
          try
            AppendStream(st, ExtractFileName(f), now);
          finally
            st.Free;
            SafeDelete(f);
          end;
        end;
      }
  finally
    Files.Free;
  end;
end;

procedure TLog.DefBtnClick(Sender: Tobject);
begin
  if Assigned(FLogPanel) then
    with FLogPanel do
      if Height = 200 then
      begin
        Height := 25;
        Tbutton(Sender).Caption := 'Show';
      end
      else
      begin
        Height := 200;
        Tbutton(Sender).Caption := 'Hide';
      end;
end;

{ TLogThread }

constructor TLogThread.Create(const Filename: String; List : TLogList;
                              MaxLogSizeMb : Integer);
begin
  FStartedDate := now;
  FDayStarted := GetDay;
  FMaxLogSize := MaxLogSizeMb * 1024 * 1024;
  FOriginalFilename := Filename;
  FFilename := MakeFilename(Filename);
  Flist := List;
  if not ForceDirectories(ExtractFilePath(FFilename)) then
    raise Exception.Create('TLogThread.Create: Cannot create path : ' + ExtractFilePath(FFilename));
  inherited Create(False);
  Priority := tpLower;
end;

function TLogThread.MakeFilename(const Filename: String): String;
begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Filename)) +
            FormatDateTime('yyyy-mm-dd-hh-nn-ss-', FStartedDate) +
            ExtractFileName(Filename);
end;

function TLogThread.GetArchivePath: String;
begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(FOriginalFilename)) + 'Archives';
  if not DirectoryExists(result) then
    ForceDirectories(result);
end;

function TLogThread.GetDay:Integer;
var
  y, m, d : word;
begin
  DecodeDate(now, y, m, d);
  result := d;
end;

procedure TLogThread.Log(const msg: string);
begin
  FList.AddItem(Format('%s Thread ID : %.8d : %s',
                         [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now),
                          QWord(GetCurrentThreadId()), Msg]));
end;

procedure TLogThread.ZipLog(const aFilename : string);
var
  st : TFileStream;
  z : TZipFile;
begin
  exit;

  if FileExists(aFilename) then
  begin
    z := TZipFile.Create;
    with z do
    try
      FileName := GetArchivePath +
                  FormatDateTime('yyyy-mm-dd - ', now) +
                  ChangeFileExt(ExtractFileName(FOriginalFilename), '.zip');

      try
        Active := true;
        try
          st := SafeOpen(aFilename, fmOpenRead or fmShareDenyWrite);
          if Assigned(st) then
          try
            AppendStream(st, ExtractFileName(aFilename), now);
          finally
            st.Free;
            SafeDelete(aFilename);
          end;
        finally
          Active := False;
        end;
      except
      end;
    finally
      z.Free;
    end;
  end;
end;

const
  UTF8Bom : array[0..2] of Byte = (239, 187, 191);

procedure TLogThread.Dump;
var
  f : TFileStream;
begin
  if Flist.GetSize > 0 then
  begin
    if FileExists(FFilename) then
      f := SafeOpen(FFilename, fmOpenReadWrite or fmShareDenyWrite)
    else
    begin
      f := SafeOpen(FFilename, fmCreate or fmShareDenyWrite);
      {$if Defined(MsWindows)}
      f.WriteBuffer(UTF8Bom, 3);
      {$endif}
    end;

    if Assigned(f) then
    try
      FList.Dump(f);
    finally
      f.Free;
    end;
  end;
end;

procedure TLogThread.Execute;
const
  FiftyKB = 50 * 1024;
var
  LastCheck : TDateTime;
begin
  Log('Logger started.');

  LastCheck := Now;
  while not Terminated do
  begin
    if (Flist.GetSize > FiftyKB) or (SecondsBetween(now, LastCheck) > 5) then
    try
      if (GetFileSize(FFilename) > FMaxLogSize) or (GetDay <> FDayStarted) then
      begin
        ZipLog(FFilename);
        FStartedDate := now;
        FDayStarted := GetDay;
        FFilename := MakeFilename(FOriginalFilename);
        CheckArchivedLogs(FOriginalFilename, GetArchivePath);
      end;

      Dump;
      LastCheck := now;
    except
    end
    else
      Sleep(1000);
  end;

  Log('Logger ended.');
  Dump;
  CheckArchivedLogs(FOriginalFilename, GetArchivePath);
end;


end.

