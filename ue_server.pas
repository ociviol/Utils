unit UE_Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$if defined(Darwin) or defined(Linux)}
    cthreads,
  {$endif}
  blcksock;

type
  TUEOnReceive = procedure(const Data : String; Socket : TUDPBlockSocket) of object;

  { TUEServerThread }

  TUEServerThread = class(TThread)
  private
    FSocket: TUDPBlockSocket;
    FBuffer: string;
    FUEOnReceive : TUEOnReceive;
    procedure DoOnReceive;
  protected
    constructor Create(aUEOnReceive : TUEOnReceive);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TUEServer = class
  private
    FUEOnReceive : TUEOnReceive;
    FUEServerThread: TUEServerThread;
    function GetRunning: Boolean;
    procedure SetEnabled(AValue: Boolean);
  public
    constructor Create(aUEOnReceive : TUEOnReceive);
    destructor Destroy; override;
    procedure Stop;
    procedure Start;
    property Running: Boolean read GetRunning;
    property Enabled : Boolean read GetRunning write SetEnabled;
  end;

implementation

{ TUEServer }

function TUEServer.GetRunning: Boolean;
begin
  Result := FUEServerThread <> nil;
end;

procedure TUEServer.SetEnabled(AValue: Boolean);
begin
  if Assigned(FUEServerThread) then
    Stop
  else
    Start;
end;

constructor TUEServer.Create(aUEOnReceive: TUEOnReceive);
begin
  FUEOnReceive:=aUEOnReceive;
  inherited Create;
end;

destructor TUEServer.Destroy;
begin
  if Assigned(FUEServerThread) then
  begin
    FUEServerThread.Terminate;
    FUEServerThread.Waitfor;
    FUEServerThread.Free;
  end;
  inherited Destroy;
end;

procedure TUEServer.Start;
begin
  FUEServerThread := TUEServerThread.Create(FUEOnReceive);
end;

procedure TUEServer.Stop;
begin
  if FUEServerThread <> nil then
  begin
    FUEServerThread.Terminate;
    FUEServerThread.WaitFor;
    FreeAndNil(FUEServerThread);
  end;
end;

{ TUEServerThread }

constructor TUEServerThread.Create(aUEOnReceive: TUEOnReceive);
begin
  FUEOnReceive := aUEOnReceive;
  inherited Create(False);
end;

destructor TUEServerThread.Destroy;
begin
  inherited Destroy;
end;


procedure TUEServerThread.DoOnReceive;
begin
  FUEOnReceive(FBuffer, FSocket);
end;

procedure TUEServerThread.Execute;
var
  i : Integer;
begin
  FSocket := TUDPBlockSocket.Create;
  try
    FSocket.Bind('10.211.55.35', '8090');
    try
      if FSocket.LastError <> 0 then
      begin
        raise Exception.CreateFmt('Bind failed with error code %d', [FSocket.LastError]);
        Exit;
      end;

      while not Terminated do
      begin
        // wait one second for new packet
        FBuffer := FSocket.RecvPacket(1000);

        if FSocket.LastError = 0 then
        begin
          if Assigned(FUEOnReceive) then
            Synchronize(@DoOnReceive);
        end;

        // minimal sleep
        if FBuffer = '' then
          Sleep(10);
      end;

    finally
      FSocket.CloseSocket;
    end;
  finally
    FSocket.Free;
  end;
end;

end.
