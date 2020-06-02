unit UE_Client;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}Classes, SysUtils, DateUtils,

  // synapse
  blcksock;

const
  cReceiveTimeout = 2000;
  cBatchSize = 100;

type
  { TUEClient }

  TUEClient = class
  private
    FConnected : Boolean;
    FSocket: TUDPBlockSocket;
    FResponseTime: Int64;
    function GetConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect;
    function Connect(const Address: string): Boolean;
    function Send(const Message: string): string;
    property ReponseTime: Int64 read FResponseTime;
    property Connected : Boolean read GetConnected;
  end;

  { TUEAnalyzer }

  { TUEAnalyzerThread }

  TUEAnalyzerThread = class(TThread)
  private
    FAddress: string;
    FBatchDelay: Cardinal;
    FDropedPackets: Cardinal;
    FAverageResponse: Extended;
    FCriticalSection: TRTLCriticalSection;
    function GetAverageResponse: Extended;
    function GetDropedPackets: Cardinal;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
    constructor Create(const Address: string; const BatchDelay: Cardinal);
    property DropedPackets: Cardinal read GetDropedPackets;
    property AverageResponse: Extended read GetAverageResponse;
  end;

  TUEAnalyzer = class
  private
    FAddress: string;
    FBatchDelay: Cardinal;
    FAnalyzerThread: TUEAnalyzerThread;
    function GetAverageResponse: Extended;
    function GetDropedPackets: Cardinal;
    function GetRunning: Boolean;
  public
    procedure StopAnalyzer;
    procedure StartAnalyzer;
    property Running: Boolean read GetRunning;
    property Address: string read FAddress write FAddress;
    property DropedPackets: Cardinal read GetDropedPackets;
    property AverageResponse: Extended read GetAverageResponse;
    property BatchDelay: Cardinal read FBatchDelay write FBatchDelay;
  end;

implementation

{ TUEAnalyzerThread }

function TUEAnalyzerThread.GetAverageResponse: Extended;
begin
  EnterCriticalsection(FCriticalSection);
  try
    Result := FAverageResponse;
  finally
    LeaveCriticalsection(FCriticalSection);
  end;
end;

function TUEAnalyzerThread.GetDropedPackets: Cardinal;
begin
  EnterCriticalsection(FCriticalSection);
  try
    Result := FDropedPackets;
  finally
    LeaveCriticalsection(FCriticalSection);
  end;
end;

procedure TUEAnalyzerThread.Execute;
var
  UEClient: TUEClient;
  Connected: Boolean;
  SendString: string;
  SendCounter: Int64;
  SumResponse: Cardinal;
  SumDropedPackets: Cardinal;
begin
  UEClient := TUEClient.Create;
  try
    Connected := UEClient.Connect(FAddress);
    try
      if not Connected then
      begin
        raise Exception.CreateFmt('Could not connect UPD client to address %s', [FAddress]);
        Exit;
      end;

      SumDropedPackets := 0;
      FAverageResponse := 0;
      FDropedPackets := 0;
      SumResponse := 0;
      SendCounter := 1;

      while not Terminated do
      begin
        SendString := IntToStr(SendCounter);

        if not (UEClient.Send(SendString) = SendString) then
          Inc(SumDropedPackets);

        Inc(SumResponse, UEClient.ReponseTime);
        Inc(SendCounter);

        if (SendCounter mod cBatchSize) = 0 then
        begin
          EnterCriticalsection(FCriticalSection);
          try
            FAverageResponse := SumResponse / cBatchSize;
            FDropedPackets := SumDropedPackets;
          finally
            LeaveCriticalsection(FCriticalSection);
          end;

          // sleep for specified batch time
          Sleep(FBatchDelay * 1000);
          SumDropedPackets := 0;
          SumResponse := 0;
        end;

        // minimal sleep
        Sleep(10);
      end;
    finally
      UEClient.Disconnect;
    end;
  finally
    UEClient.Free;
  end;
end;

destructor TUEAnalyzerThread.Destroy;
begin
  {$IFDEF MSWINDOWS}
    DeleteCriticalSection(FCriticalSection)
  {$ELSE}
    DoneCriticalSection(FCriticalSection)
  {$ENDIF};

  inherited Destroy;
end;

constructor TUEAnalyzerThread.Create(const Address: string; const BatchDelay: Cardinal);
begin
  {$IFDEF MSWINDOWS}
    InitializeCriticalSection(FCriticalSection)
  {$ELSE}
    InitCriticalSection(FCriticalSection)
  {$ENDIF};

  FBatchDelay := BatchDelay;
  FreeOnTerminate := True;
  FAddress := Address;

  inherited Create(False);
end;

{ TUEAnalyzer }

procedure TUEAnalyzer.StartAnalyzer;
begin
  FAnalyzerThread := TUEAnalyzerThread.Create(FAddress, FBatchDelay);
end;

function TUEAnalyzer.GetRunning: Boolean;
begin
  Result := FAnalyzerThread <> nil;
end;

function TUEAnalyzer.GetAverageResponse: Extended;
begin
  Result := FAnalyzerThread.AverageResponse;
end;

function TUEAnalyzer.GetDropedPackets: Cardinal;
begin
  Result := FAnalyzerThread.DropedPackets;
end;

procedure TUEAnalyzer.StopAnalyzer;
begin
  if Running then
  begin
    FAnalyzerThread.Terminate;
    FAnalyzerThread := nil;
  end;
end;

{ TUEClient }

function TUEClient.GetConnected: Boolean;
begin
  result := FConnected;
end;

constructor TUEClient.Create;
begin
  FSocket := TUDPBlockSocket.Create;
end;

destructor TUEClient.Destroy;
begin
  FreeAndNil(FSocket);

  inherited Destroy;
end;

procedure TUEClient.Disconnect;
begin
  FSocket.CloseSocket;
  FConnected := false;
end;

function TUEClient.Connect(const Address: string): Boolean;
begin
  FSocket.Connect(Address, '7');
  Result := FSocket.LastError = 0;
  FConnected := result;
end;

function TUEClient.Send(const Message: string): string;
var
  StartTime: TDateTime;
begin
  Result := '';
  StartTime := Now;
  FSocket.SendString(Message);

  if FSocket.LastError = 0 then
  begin
    Result := FSocket.RecvPacket(cReceiveTimeout);
    FResponseTime := MilliSecondsBetween(Now, StartTime);

    if FSocket.LastError <> 0 then
    begin
      FResponseTime := -1;
      Result := '';
    end;
  end;
end;

end.
