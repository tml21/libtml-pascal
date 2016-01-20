(*
 *  libTML:  A BEEP based Messaging Suite
 *  Copyright (C) 2015 wobe-systems GmbH
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *  
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to develop
 *  proprietary applications using this library without any royalty or
 *  fee but returning back any change, improvement or addition in the
 *  form of source code, project image, documentation patches, etc.
 *
 *  Homepage:
 *    http://www.libtml.org
 *
 *  For professional support contact us:
 *
 *    wobe-systems GmbH
 *    support@libtml.org
 *)

unit thrDataProcessing;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, SyncObjs, Forms,
  uTMLClasses, uTMLCore, uTMLTypes, cSidexDocument;

//------------------------------------------------------------------------------

const
  CMD_FINDER_SAY_HELLO        =  1;
  CMD_FINDER_SAY_GOODBYE      =  2;
  CMD_FINDER_ASK_FOR_FRIENDS  =  3;
  CMD_FINDER_DO_CLOSE         =  4;
  CMD_FINDER_ALIVE_CHECK      =  5;
  CMD_FINDER_CHANGING_NAME    =  6;

  CMD_DEMO_SLIDER             = 10;
  CMD_DEMO_DATA_PROCESSING    = 11;
  CMD_DEMO_SEND_MESSAGE       = 12;
  CMD_DEMO_LIST_FILES         = 13;
  CMD_DEMO_GET_STREAM_ID      = 14;
  CMD_DEMO_RELEASE_STREAM_ID  = 15;

const
  ASYNC_LIST_FRIENDS          =  1;
  ASYNC_ASK_FOR_FRIENDS       =  2;
  ASYNC_ASK_TO_BE_MY_FRIEND   =  3;
  ASYNC_DO_CLOSE              =  4;
  ASYNC_ALIVE_CHECK           =  5;
  ASYNC_BEGIN_DATA_PROCESSING =  6;
  ASYNC_END_DATA_PROCESSING   =  7;
  ASYNC_SHOW_MESSAGE          =  8;
  ASYNC_UPDATE_PROGRESS       =  9;
  ASYNC_APPEND_MESSAGE        = 10;
  ASYNC_SYNC_DATA_PROCESS     = 11;
  ASYNC_REFRESH_DOWNLOADS     = 12;

const
  GROUP_NAME_MYSELF           = 'Myself';
  GROUP_NAME_QUERY            = 'Query';
  GROUP_NAME_ANSWER           = 'Answer';
  GROUP_NAME_SLIDER           = 'Slider';
  GROUP_NAME_PROGRESS         = 'Progress';

  PARAM_NAME_HOST_NAME        = 'HostName';
  PARAM_NAME_HOST_IP          = 'HostIP';
  PARAM_NAME_LISTENER_IP      = 'ListenerIP';
  PARAM_NAME_LISTENER_PORT    = 'ListenerPort';
  PARAM_NAME_MY_NAME          = 'MyName';

  PARAM_NAME_IP               = 'IP';
  PARAM_NAME_PORT             = 'Port';
  PARAM_NAME_HOST             = 'Host';
  PARAM_NAME_NAME             = 'Name';
  PARAM_NAME_ACCEPT           = 'Accept';
  PARAM_NAME_MESSAGE          = 'Message';
  PARAM_NAME_VALUE            = 'Value';
  PARAM_NAME_I_PORTION        = 'iPortion';
  PARAM_NAME_N_PORTIONS       = 'nPortions';
  PARAM_NAME_DURATION         = 'Duration';
  PARAM_NAME_N_PROGRESSES     = 'nProgresses';
  PARAM_NAME_LIST             = 'List';
  PARAM_NAME_FILENAME         = 'FileName';
  PARAM_NAME_ID               = 'ID';

  TABLE_NAME_FRIENDS          = 'Friends';

  TABLE_COLUMN_IP             = 'IP';
  TABLE_COLUMN_PORT           = 'Port';
  TABLE_COLUMN_HOST           = 'Host';
  TABLE_COLUMN_NAME           = 'Name';
  TABLE_COLUMN_ID             = 'ID';

//------------------------------------------------------------------------------

type
  TFriendInfo = record
    ID:   Integer;
    IP:   string;
    Port: string;
    Host: string;
    Name: string;
  end;

//------------------------------------------------------------------------------

type
  TAsyncCallData = class(TObject)
  public
    ID:         Integer;
    CommandUID: Cardinal;
    Progress:   Integer;
    Text:       string;
    iParam:     Array[0..4] of Integer;
    Info:       TFriendInfo;
  end;

//------------------------------------------------------------------------------

type
  TProgressData = class(TObject)
  public
    CommandUID: Cardinal;
    Progress:   Integer;
  end;

  TDownloadData = class(TObject)
  public
    StreamID:  TML_STREAM_ID;
    Progress:  Integer;
    ErrorCode: TML_INT32;
    Finished:  Boolean;
    Name:      string;
    dlEntry:   Pointer;
  end;

//------------------------------------------------------------------------------

type
  TDataStorage = class(TObject)
  private
    FTMLCore:         TTMLCore;
    FTMLProfile:      TTMLProfile;

    FStorage:         TSIDEXDocument;
    FStorageFileName: string;
    FStorageLock:     TCriticalSection;

    FAsyncCallMethod: TDataEvent;

    FNextFriendID:    Integer;

    FAliveCheckInProgress: Boolean;

  public
    constructor Create(ATMLCore:         TTMLCore;
                       ATMLProfile:      TTMLProfile;
                       AAsyncCallMethod: TDataEvent); overload;
    destructor Destroy; override;

    function  GetLock: Boolean;
    procedure ReleaseLock;

    function  GetHostName(ALocking: Boolean): string;
    function  GetHostName: string;
    procedure SetHostName(AHostName: string; ALocking: Boolean);
    procedure SetHostName(AHostName: string);
    function  GetHostIP(ALocking: Boolean): string;
    function  GetHostIP: string;
    procedure SetHostIP(AHostIP: string; ALocking: Boolean);
    procedure SetHostIP(AHostIP: string);
    function  GetListenerIP(ALocking: Boolean): string;
    function  GetListenerIP: string;
    procedure SetListenerIP(AListenerIP: string; ALocking: Boolean);
    procedure SetListenerIP(AListenerIP: string);
    function  GetListenerPort(ALocking: Boolean): string;
    function  GetListenerPort: string;
    procedure SetListenerPort(AListenerPort: string; ALocking: Boolean);
    procedure SetListenerPort(AListenerPort: string);
    function  GetMyName(ALocking: Boolean): string;
    function  GetMyName: string;
    procedure SetMyName(AMyName: string; ALocking: Boolean);
    procedure SetMyName(AMyName: string);

    function  LoadStorage(AStorageFileName: string): Boolean;
    function  SaveStorage(AStorageFileName: string = ''): Boolean;

    function  IsItMe(IP, Port: string; Host: string = ''; ALocking: Boolean = true): Boolean;
    function  CreateFriendID: Integer;

    function  AddFriendToList(IP, Port, Host, AName: string; ALocking: Boolean = true): Boolean;
    function  RemoveFriendFromList(IP, Port: string; ALocking: Boolean = true): Boolean;
    function  RemoveAllFriends(ALocking: Boolean = true): Boolean;
    function  IsMyFriend(IP, Port: string; ALocking: Boolean = true): Boolean;
    function  MergeFriends(newFriends: TSIDEXDocument; ALocking: Boolean = true): Boolean;
    function  ChangeNameOfFriend(IP, Port, NewName: string; ALocking: Boolean = true): Boolean;
    function  FriendCount(ALocking: Boolean = true): Integer;
    function  GetFriendByIndex(iFriend: Integer; ALocking: Boolean = true): TFriendInfo;
    function  GetFriendById(FriendID: Integer; ALocking: Boolean = true): TFriendInfo;

    procedure RemoveAllDestinations(ALocking: Boolean = true);

    procedure SignalDataUpdate;

    property  Storage        : TSIDEXDocument read FStorage;
    property  StorageFileName: string read FStorageFileName;

    property  HostName       : string read GetHostName     write SetHostName;
    property  HostIP         : string read GetHostIP       write SetHostIP;
    property  ListenerIP     : string read GetListenerIP   write SetListenerIP;
    property  ListenerPort   : string read GetListenerPort write SetListenerPort;
    property  MyName         : string read GetMyName       write SetMyName;

    property  TMLCore        : TTMLCore    read FTMLCore;
    property  TMLProfile     : TTMLProfile read FTMLProfile;

    property  AliveCheckInProgress : Boolean read FAliveCheckInProgress write FAliveCheckInProgress;
  end;

//------------------------------------------------------------------------------

type
  TDataProcessingThread = class(TThread)
  private
    FData: TDataStorage;
    FCall: TAsyncCallData;

  protected
    procedure Execute(); override;

  public
    constructor Create(AData: TDataStorage;
                       ACall: TAsyncCallData); overload;
    destructor  Destroy; override;
  end;

//------------------------------------------------------------------------------

function FormatFriendInfo(const IP, Port, Host, HisName: string): string;

//------------------------------------------------------------------------------

function GetIPFromHost(out HostName, IP, WSAErr: string): Boolean;
function GetIPByName(HostName: string; out IP, WSAErr: string): Boolean;

//------------------------------------------------------------------------------

implementation

uses
  {$ifdef Unix}
    unix,
    Process,
  {$else}
    winsock,
  {$endif}
  uSidexLib, uTMLErrors, uSidexVariant, variants;

//------------------------------------------------------------------------------

function FormatFriendInfo(const IP, Port, Host, HisName: string): string;
begin
  if HisName <> '' then
  begin
    if (Host <> IP) and (Host <> '') then
    begin
      Result := Format('%s:%s (%s) "%s"', [IP, Port, Host, HisName]);
    end
    else Result := Format('%s:%s "%s"', [IP, Port, HisName]);
  end
  else
  begin
    if (Host <> IP) and (Host <> '') then
    begin
      Result := Format('%s:%s (%s)', [IP, Port, Host]);
    end
    else Result := Format('%s:%s', [IP, Port]);
  end;
end;

//------------------------------------------------------------------------------

{$ifdef Unix}
type
  { THostEnt Object }
  THostEnt = record
    h_name:      pchar;   // official name
    h_aliases:   ppchar;  // null-terminated list of aliases
    h_addrtype:  longint; // host address type
    h_length:    longint; // length of address
    h_addr_list: ppchar;  // null-terminated list of adresses
  end;
  pHostEnt = ^THostEnt;

function gethostbyname(Name: Pchar): pHostEnt; cdecl; external 'c';
{$endif}

function GetIPFromHost(out HostName, IP, WSAErr: string): Boolean;
type
  Name = array[0..100] of AnsiChar;
  PName = ^Name;
var
  HEnt: pHostEnt;
  i, j: Integer;
  {$ifdef windows}
    HName:   PName;
    WSAData: TWSAData;
  {$endif}
  {$ifdef unix}
    {$ifndef Darwin}
      Proc: TProcess;
      Info: TStringList;
    {$endif}
  {$endif}
begin
  Result   := false;
  HostName := '';
  IP       := '';
  WSAErr   := '';
  {$ifdef windows}
    WSAData.iMaxSockets := 0; // to avoid compiler hint
    if WSAStartup($0101, WSAData) <> 0 then
    begin
      WSAErr := 'Winsock is not responding!';
      Exit;
    end;

    New(HName);
    if gethostname(HName^, SizeOf(Name)) = 0 then
    begin
      HostName := FormatHostName(StrPas(HName^));
    end;
  {$else}
    HostName := FormatHostName(GetHostName);
  {$endif}
  Result := (HostName <> '');
  if Result then
  begin
    {$ifndef Unix}
      HEnt := gethostbyname(HName^);
      if HEnt <> nil then
      begin
        j := 0;
        while HEnt^.h_addr_list[j] <> nil do
        begin
          for i := 0 to HEnt^.h_length - 1 do
            IP := Concat(IP, IntToStr(Ord(HEnt^.h_addr_list[j][i])) + '.');
          SetLength(IP, Length(IP) - 1);
          IP := FormatIP(IP);
          if IP <> '127.0.0.1' then Break;
          j := j + 1;
        end;
        Result := IsIPAddress(IP);
        if not Result then IP := '';
      end;
    {$else} // Unix
      {$ifdef Darwin}
        HEnt := gethostbyname(PAnsiChar(HostName));
        if HEnt <> nil then
        begin
          j := 0;
          while HEnt^.h_addr_list[j] <> nil do
          begin
            for i := 0 to HEnt^.h_length - 1 do
              IP := Concat(IP, IntToStr(Ord(HEnt^.h_addr_list[j][i])) + '.');
            SetLength(IP, Length(IP) - 1);
            IP := FormatIP(IP);
            if IP <> '127.0.0.1' then Break;
            j := j + 1;
          end;
          Result := IsIPAddress(IP);
          if not Result then IP := '';
        end;
      {$else} // Darwin
        Proc := TProcess.Create(nil);
        if Assigned(Proc) then
        begin
          Info := TStringList.Create;
          if Assigned(Info) then
          begin
            Proc.CommandLine := 'hostname -I';
            Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes];
            Proc.Execute;
            Info.LoadFromStream(Proc.Output);
            if Info.Count > 0 then
            begin
              IP := FormatIP(Info[0]);
              Result := IsIPAddress(IP);
              if not Result then
                WSAErr := 'Illegal answer from "hostname -I"!';
            end
            else WSAErr := 'Unsuccessful execution of "hostname -I"!';
            FreeAndNil(Info);
          end;
          FreeAndNil(Proc);
        end;
      {$endif}  // Darwin
    {$endif}  // Unix
  end
  {$ifndef windows}
  else WSAErr := 'Unsuccessful call to GetHostName!'
  {$endif};
  if not Result then
  begin
    HostName := '';
    IP       := '';
    {$ifdef windows}
      case WSAGetLastError of
        WSANOTINITIALISED: WSAErr := 'WSANotInitialised';
        WSAENETDOWN      : WSAErr := 'WSAENetDown';
        WSAEINPROGRESS   : WSAErr := 'WSAEInProgress';
        WSAHOST_NOT_FOUND: WSAErr := 'WSAHostNotFound';
        WSATRY_AGAIN     : WSAErr := 'WSATryAgain';
        WSANO_RECOVERY   : WSAErr := 'WSANoRecovery';
        WSANO_DATA       : WSAErr := 'WSANoData';
        WSAEFAULT        : WSAErr := 'WSAEFault';
        WSAEINTR         : WSAErr := 'WSAIntr';
      end;
    {$endif}
  end;
  {$ifdef windows}
    Dispose(HName);
    WSACleanup;
  {$endif}
end;

function GetIPByName(HostName: string; out IP, WSAErr: string): Boolean;
var
  HEnt: pHostEnt;
  {$ifdef windows}
    WSAData: TWSAData;
  {$endif}
  i: Integer;
begin
  Result := False;
  {$ifdef windows}
    WSAData.iMaxSockets := 0; // to avoid compiler hint
    if WSAStartup($0101, WSAData) <> 0 then
    begin
      WSAErr := 'Winsock is not responding!';
      Exit;
    end
    else WSAErr := '';
  {$else}
    WSAErr := '';
  {$endif}
  IP := '';
  HEnt := gethostbyname(PAnsiChar(HostName));
  if HEnt <> nil then
  begin
    for i := 0 to HEnt^.h_length - 1 do
      IP := Concat(IP, IntToStr(Ord(HEnt^.h_addr_list[0][i])) + '.');
    SetLength(IP, Length(IP) - 1);
    IP := FormatIP(IP);
    Result := IsIPAddress(IP);
  end
  {$ifndef windows}
  else WSAErr := 'Unsuccessful call to GetHostName!'
  {$endif};
  if not Result then
  begin
    IP := '';
    {$ifdef windows}
      case WSAGetLastError of
        WSANOTINITIALISED: WSAErr := 'WSANotInitialised';
        WSAENETDOWN      : WSAErr := 'WSAENetDown';
        WSAEINPROGRESS   : WSAErr := 'WSAEInProgress';
        WSAHOST_NOT_FOUND: WSAErr := 'WSAHostNotFound';
        WSATRY_AGAIN     : WSAErr := 'WSATryAgain';
        WSANO_RECOVERY   : WSAErr := 'WSANoRecovery';
        WSANO_DATA       : WSAErr := 'WSANoData';
        WSAEFAULT        : WSAErr := 'WSAEFault';
        WSAEINTR         : WSAErr := 'WSAIntr';
      end;
    {$endif}
  end;
  {$ifdef windows}
    WSACleanup;
  {$endif}
end;

//------------------------------------------------------------------------------

constructor TDataStorage.Create(ATMLCore:         TTMLCore;
                                ATMLProfile:      TTMLProfile;
                                AAsyncCallMethod: TDataEvent);
begin
  FTMLCore         := ATMLCore;
  FTMLProfile      := ATMLProfile;
  FAsyncCallMethod := AAsyncCallMethod;

  FStorageFileName := '';
  FStorageLock     := TCriticalSection.Create;
  FStorage         := TSIDEXDocument.Create('Storage');

  FNextFriendID    := 1;

  FAliveCheckInProgress := false;

  inherited Create();
end;

destructor TDataStorage.Destroy;
begin
  if GetLock then
  begin
    FreeAndNil(FStorage);
    FreeAndNil(FStorageLock);
  end;
  inherited Destroy;
end;

function TDataStorage.GetLock: Boolean;
begin
  if Assigned(FStorageLock) then
  begin
    FStorageLock.Enter;
    Result := true;
  end
  else Result := false;
end;

procedure TDataStorage.ReleaseLock;
begin
  if Assigned(FStorageLock) then
  begin
    FStorageLock.Leave;
  end;
end;

function TDataStorage.GetHostName(ALocking: Boolean): string;
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      Result := FormatHostName(FStorage.ReadString(GROUP_NAME_MYSELF,
                                                   PARAM_NAME_HOST_NAME, ''));
    end
    else Result := '';
  finally
    if ALocking then ReleaseLock;
  end;
end;

function TDataStorage.GetHostName: string;
begin
  Result := GetHostName(true);
end;

procedure TDataStorage.SetHostName(AHostName: string; ALocking: Boolean);
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      FStorage.WriteString(GROUP_NAME_MYSELF, PARAM_NAME_HOST_NAME,
                           FormatHostName(AHostName));
    end;
  finally
    if ALocking then ReleaseLock;
  end;
end;

procedure TDataStorage.SetHostName(AHostName: string);
begin
  SetHostName(AHostName, true);
end;

function TDataStorage.GetHostIP(ALocking: Boolean): string;
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      Result := FormatIP(FStorage.ReadString(GROUP_NAME_MYSELF,
                                             PARAM_NAME_HOST_IP, ''));
    end
    else Result := '';
  finally
    if ALocking then ReleaseLock;
  end;
end;

function TDataStorage.GetHostIP: string;
begin
  Result := GetHostIP(true);
end;

procedure TDataStorage.SetHostIP(AHostIP: string; ALocking: Boolean);
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      FStorage.WriteString(GROUP_NAME_MYSELF, PARAM_NAME_HOST_IP,
                           FormatIP(AHostIP));
    end;
  finally
    if ALocking then ReleaseLock;
  end;
end;

procedure TDataStorage.SetHostIP(AHostIP: string);
begin
  SetHostIP(AHostIP, true);
end;

function TDataStorage.GetListenerIP(ALocking: Boolean): string;
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      Result := FormatIP(FStorage.ReadString(GROUP_NAME_MYSELF,
                                             PARAM_NAME_LISTENER_IP, ''));
    end
    else Result := '';
  finally
    if ALocking then ReleaseLock;
  end;
end;

function TDataStorage.GetListenerIP: string;
begin
  Result := GetListenerIP(true);
end;

procedure TDataStorage.SetListenerIP(AListenerIP: string; ALocking: Boolean);
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      FStorage.WriteString(GROUP_NAME_MYSELF, PARAM_NAME_LISTENER_IP,
                           FormatIP(AListenerIP));
    end;
  finally
    if ALocking then ReleaseLock;
  end;
end;

procedure TDataStorage.SetListenerIP(AListenerIP: string);
begin
  SetListenerIP(AListenerIP, true);
end;

function TDataStorage.GetListenerPort(ALocking: Boolean): string;
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      Result := FormatPort(FStorage.ReadString(GROUP_NAME_MYSELF,
                                               PARAM_NAME_LISTENER_PORT, ''));
    end
    else Result := '';
  finally
    if ALocking then ReleaseLock;
  end;
end;

function TDataStorage.GetListenerPort: string;
begin
  Result := GetListenerPort(true);
end;

procedure TDataStorage.SetListenerPort(AListenerPort: string; ALocking: Boolean);
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      FStorage.WriteString(GROUP_NAME_MYSELF, PARAM_NAME_LISTENER_PORT,
                           FormatPort(AListenerPort));
    end;
  finally
    if ALocking then ReleaseLock;
  end;
end;

procedure TDataStorage.SetListenerPort(AListenerPort: string);
begin
  SetListenerPort(AListenerPort, true);
end;

function TDataStorage.GetMyName(ALocking: Boolean): string;
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      Result := FStorage.ReadString(GROUP_NAME_MYSELF, PARAM_NAME_MY_NAME, '');
    end
    else Result := '';
  finally
    if ALocking then ReleaseLock;
  end;
end;

function TDataStorage.GetMyName: string;
begin
  Result := GetMyName(true);
end;

procedure TDataStorage.SetMyName(AMyName: string; ALocking: Boolean);
begin
  if ALocking then GetLock;
  try
    if Assigned(FStorage) then
    begin
      FStorage.WriteString(GROUP_NAME_MYSELF, PARAM_NAME_MY_NAME, AMyName);
    end;
  finally
    if ALocking then ReleaseLock;
  end;
end;

procedure TDataStorage.SetMyName(AMyName: string);
begin
  SetMyName(AMyName, true);
end;

function TDataStorage.LoadStorage(AStorageFileName: string): Boolean;
begin
  Result := false;
  if AStorageFileName <> '' then
  begin
    if GetLock then
    begin
      try
        if Assigned(FStorage) then
        begin
          FStorage.LoadContent(AStorageFileName);
          Result := true;
        end;
      except
        Result := false;
      end;
      ReleaseLock;
    end;
  end;
end;

function TDataStorage.SaveStorage(AStorageFileName: string): Boolean;
begin
  Result := false;
  if AStorageFileName = '' then AStorageFileName := FStorageFileName;
  if AStorageFileName <> '' then
  begin
    if GetLock then
    begin
      try
        if Assigned(FStorage) then
        begin
          FStorage.SaveContent(AStorageFileName);
          Result := true;
        end;
      except
        Result := false;
      end;
      ReleaseLock;
    end;
  end;
end;

function TDataStorage.IsItMe(IP, Port, Host: string;
                             ALocking: Boolean): Boolean;
var
  sameName, sameIP, samePort: Boolean;
  sIP: string;
begin
  // for the first time, this is a simple compare, but it must be more complex!
  sIP      := GetHostIP(ALocking);
  sameName := (FormatHostName(Host) = GetHostName(ALocking));
  sameIP   := (FormatIP(Host) = sIP) or (FormatIP(IP) = sIP);
  samePort := (FormatPort(Port) = GetListenerPort(ALocking));
  Result   := (sameName or sameIP) and samePort;
end;

function TDataStorage.CreateFriendID: Integer;
begin
  Result := FNextFriendID;
  FNextFriendID := FNextFriendID + 1;
end;

function TDataStorage.AddFriendToList(IP, Port, Host, AName: string;
                                      ALocking: Boolean): Boolean;
var
  sIP, sP, WSAErr: string;
  Exists:          Boolean;
  Table, val:      Variant;
  Call:            TAsyncCallData;
begin
  Result := false;
  if not IsIPAddress(IP) then GetIPByName(Host, IP, WSAErr);
  if IsIPAddress(IP) and (Port <> '') then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        IP   := FormatIP(IP);
        Port := FormatPort(Port);
        Host := FormatHostName(Host);
        if not IsItMe(IP, Port, Host, false) then
        begin
          try
            Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
          except
            Table := Null;
          end;
          if not VarIsSidexTable(Table) then Table := VarSidexTableCreate();
          if VarIsSidexTable(Table) then
          begin
            if not VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) then
              VarSidexTableAddColumn(Table, TABLE_COLUMN_IP);
            if not VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
              VarSidexTableAddColumn(Table, TABLE_COLUMN_PORT);
            if not VarSidexTableHasColumn(Table, TABLE_COLUMN_HOST) then
              VarSidexTableAddColumn(Table, TABLE_COLUMN_HOST);
            if not VarSidexTableHasColumn(Table, TABLE_COLUMN_NAME) then
              VarSidexTableAddColumn(Table, TABLE_COLUMN_NAME);
            if not VarSidexTableHasColumn(Table, TABLE_COLUMN_ID) then
              VarSidexTableAddColumn(Table, TABLE_COLUMN_ID);

            Result := true;
            Exists := false;
            VarSidexTableFirst(Table);
            while not VarSidexTableEof(Table) do
            begin
              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
                if VarIsStr(val) then sIP := FormatIP(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
                if VarIsStr(val) then sP := FormatPort(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              if (sIP = IP) and (sP = Port) then
              begin
                Exists := true;
                Break;
              end;
              VarSidexTableNext(Table);
            end;

            if Result and not Exists then
            begin
              // this is a new friend...
              VarSidexTableAppendRow(Table);
              try
                VarSidexTableSetField(Table, TABLE_COLUMN_IP,   IP);
                VarSidexTableSetField(Table, TABLE_COLUMN_PORT, Port);
                VarSidexTableSetField(Table, TABLE_COLUMN_HOST, Host);
                VarSidexTableSetField(Table, TABLE_COLUMN_NAME, AName);
                VarSidexTableSetField(Table, TABLE_COLUMN_ID,   CreateFriendID);
                FStorage.WriteValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Table);
              except
                Result := false;
              end;

              if Result then
              begin
                // The new friend should receive events from us...
                FTMLProfile.AddEventDestination(IP, Port);
                FTMLProfile.AddBalDestination(IP, Port);

                // ...and let's ask him for his friends...
                if Assigned(FAsyncCallMethod) then
                begin
                  Call := TAsyncCallData.Create;
                  if Assigned(Call) then
                  begin
                    Call.ID        := ASYNC_ASK_FOR_FRIENDS;
                    Call.Info.IP   := IP;
                    Call.Info.Port := Port;
                    Application.QueueAsyncCall(FAsyncCallMethod, PtrInt(Call));
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

function TDataStorage.RemoveFriendFromList(IP, Port: string;
                                           ALocking: Boolean): Boolean;
var
  sIP, sP, WSAErr: string;
  Exists:          Boolean;
  Table, val:      Variant;
begin
  Result := false;
  if not IsIPAddress(IP) then GetIPByName(IP, IP, WSAErr);
  if IsIPAddress(IP) and (Port <> '') then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        IP   := FormatIP(IP);
        Port := FormatPort(Port);
        FTMLProfile.RemoveEventDestination(IP, Port);
        FTMLProfile.RemoveBalDestination(IP, Port);
        try
          Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
        except
          Table := Null;
        end;
        if VarIsSidexTable(Table) then
        begin
          Result := true;
          if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) and
             VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
          begin
            Exists := false;
            VarSidexTableFirst(Table);
            while not VarSidexTableEof(Table) do
            begin
              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
                if VarIsStr(val) then sIP := FormatIP(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
                if VarIsStr(val) then sP := FormatPort(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              if (sIP = IP) and (sP = Port) then
              begin
                Exists := true;
                VarSidexTableDeleteRow(Table);
                Break;
              end;
              VarSidexTableNext(Table);
            end;

            if Result and Exists then
            begin
              try
                FStorage.WriteValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Table);
              except
                Result := false;
              end;
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

function TDataStorage.RemoveAllFriends(ALocking: Boolean): Boolean;
var
  Table: Variant;
begin
  Result := false;
  if ALocking then GetLock;
  if Assigned(FStorage) then
  begin
    try
      RemoveAllDestinations(false);
      try
        Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
      except
        Table := Null;
      end;
      if VarIsSidexTable(Table) then
      begin
        try
          VarSidexTableClear(Table);
          FStorage.WriteValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Table);
          Result := true;
        except
          Result := false;
        end;
      end;
    finally
      if ALocking then ReleaseLock;
    end;
  end;
end;

procedure TDataStorage.RemoveAllDestinations(ALocking: Boolean);
var
  sIP, sP:    string;
  Result:     Boolean;
  Table, val: Variant;
begin
  Result := false;
  if Assigned(FStorage) then
  begin
    if ALocking then GetLock;
    try
      try
        Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
      except
        Table := Null;
      end;
      if VarIsSidexTable(Table) then
      begin
        if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) and
           VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
        begin
          VarSidexTableFirst(Table);
          while not VarSidexTableEof(Table) do
          begin
            Result := true;
            try
              val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
              if VarIsStr(val) then sIP := FormatIP(val)
              else Result := false;
            except
              Result := false;
            end;
            if not Result then Break;

            try
              val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
              if VarIsStr(val) then sP := FormatPort(val)
              else Result := false;
            except
              Result := false;
            end;
            if not Result then Break;

            try
              FTMLProfile.RemoveEventDestination(sIP, sP);
              FTMLProfile.RemoveBalDestination(sIP, sP);
            except
              // do nothing
            end;
            VarSidexTableNext(Table);
          end;
        end;
      end;
    finally
      if ALocking then ReleaseLock;
    end;
  end;
end;

function TDataStorage.IsMyFriend(IP, Port: string;
                                 ALocking: Boolean): Boolean;
var
  sIP, sP, WSAErr: string;
  Table, val:      Variant;
begin
  Result := false;
  if not IsIPAddress(IP) then GetIPByName(IP, IP, WSAErr);
  if IsIPAddress(IP) and (Port <> '') then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        IP   := FormatIP(IP);
        Port := FormatPort(Port);
        try
          Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
        except
          Table := Null;
        end;
        if VarIsSidexTable(Table) then
        begin
          if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) and
             VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
          begin
            VarSidexTableFirst(Table);
            while not VarSidexTableEof(Table) do
            begin
              Result := true;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
                if VarIsStr(val) then sIP := FormatIP(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
                if VarIsStr(val) then sP := FormatPort(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              Result := ((sIP = IP) and (sP = Port));
              if Result then Break;
              VarSidexTableNext(Table);
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

function TDataStorage.MergeFriends(newFriends: TSIDEXDocument;
                                   ALocking: Boolean): Boolean;
var
  sIP, sP:    string;
  Table, val: Variant;
  Call:       TAsyncCallData;
begin
  Result := false;
  if Assigned(newFriends) then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        try
          Table := newFriends.ReadValue(GROUP_NAME_ANSWER, TABLE_NAME_FRIENDS, Null);
        except
          Table := Null;
        end;
        if not VarIsSidexTable(Table) then
        begin
          try
            Table := newFriends.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
          except
            Table := Null;
          end;
        end;
        if VarIsSidexTable(Table) then
        begin
          Result := true;
          if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) and
             VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
          begin
            VarSidexTableFirst(Table);
            while not VarSidexTableEof(Table) do
            begin
              Result := true;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
                if VarIsStr(val) then sIP := FormatIP(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              try
                val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
                if VarIsStr(val) then sP := FormatPort(val)
                else Result := false;
              except
                Result := false;
              end;
              if not Result then Break;

              if not (IsItMe(sIP, sP, sIP, false) or
                      IsMyFriend(sIP, sP, false)) then
              begin
                if Assigned(FAsyncCallMethod) then
                begin
                  Call := TAsyncCallData.Create;
                  if Assigned(Call) then
                  begin
                    Call.ID        := ASYNC_ASK_TO_BE_MY_FRIEND;
                    Call.Info.IP   := sIP;
                    Call.Info.Port := sP;
                    Application.QueueAsyncCall(FAsyncCallMethod, PtrInt(Call));
                  end;
                end;
              end;
              VarSidexTableNext(Table);
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

function TDataStorage.ChangeNameOfFriend(IP, Port, NewName: string;
                                         ALocking: Boolean): Boolean;
var
  sIP, sP, WSAErr: string;
  Table, val:      Variant;
begin
  Result := false;
  if not IsIPAddress(IP) then GetIPByName(IP, IP, WSAErr);
  if IsIPAddress(IP) and (Port <> '') then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        IP   := FormatIP(IP);
        Port := FormatPort(Port);
        if not IsItMe(IP, IP, Port, false) then
        begin
          try
            Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
          except
            Table := Null;
          end;
          if VarIsSidexTable(Table) then
          begin
            if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) and
               VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
            begin
              if not VarSidexTableHasColumn(Table, TABLE_COLUMN_NAME) then
                VarSidexTableAddColumn(Table, TABLE_COLUMN_NAME);

              VarSidexTableFirst(Table);
              while not VarSidexTableEof(Table) do
              begin
                Result := true;

                try
                  val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
                  if VarIsStr(val) then sIP := FormatIP(val)
                  else Result := false;
                except
                  Result := false;
                end;
                if not Result then Break;

                try
                  val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
                  if VarIsStr(val) then sP := FormatPort(val)
                  else Result := false;
                except
                  Result := false;
                end;
                if not Result then Break;

                if (sIP = IP) and (sP = Port) then
                begin
                  try
                    VarSidexTableSetField(Table, TABLE_COLUMN_NAME, NewName);
                    FStorage.WriteValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Table);
                  except
                    Result := false;
                  end;
                  Break;
                end;
                VarSidexTableNext(Table);
              end;
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

function TDataStorage.FriendCount(ALocking: Boolean): Integer;
var
  Table: Variant;
begin
  Result := -1;
  if ALocking then GetLock;
  if Assigned(FStorage) then
  begin
    try
      try
        Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
        if VarIsSidexTable(Table) then
        begin
          Result := VarSidexTableRowCount(Table);
        end;
      except
        Result := -1;
      end;
    finally
      if ALocking then ReleaseLock;
    end;
  end;
end;

function TDataStorage.GetFriendByIndex(iFriend: Integer;
                                       ALocking: Boolean): TFriendInfo;
var
  Table, val: Variant;
  nFriends:   Integer;
begin
  Result.ID   := -1;
  Result.IP   := '';
  Result.Port := '';
  Result.Host := '';
  Result.Name := '';

  if ALocking then GetLock;
  if Assigned(FStorage) then
  begin
    try
      try
        Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
      except
        Table := Null;
      end;
      if VarIsSidexTable(Table) then
      begin
        nFriends := VarSidexTableRowCount(Table);
        if (iFriend >= 0) and (iFriend < nFriends) then
        begin
          VarSidexTableSetCurrentRow(Table, iFriend);

          if VarSidexTableHasColumn(Table, TABLE_COLUMN_ID) then
          begin
            val := VarSidexTableGetField(Table, TABLE_COLUMN_ID);
            if VarIsOrdinal(val) then
            begin
              try
                Result.ID := val;
              except
                Result.ID := -1;
              end;
            end;
          end;

          if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) then
          begin
            val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
            if VarIsStr(val) then
            begin
              try
                Result.IP := FormatIP(val);
              except
                Result.IP := '';
              end;
            end;
          end;

          if VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
          begin
            val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
            if VarIsStr(val) then
            begin
              try
                Result.Port := FormatPort(val);
              except
                Result.Port := '';
              end;
            end;
          end;

          if VarSidexTableHasColumn(Table, TABLE_COLUMN_HOST) then
          begin
            val := VarSidexTableGetField(Table, TABLE_COLUMN_HOST);
            if VarIsStr(val) then
            begin
              try
                Result.Host := FormatHostName(val);
              except
                Result.Host := '';
              end;
            end;
          end;

          if VarSidexTableHasColumn(Table, TABLE_COLUMN_NAME) then
          begin
            val := VarSidexTableGetField(Table, TABLE_COLUMN_NAME);
            if VarIsStr(val) then
            begin
              try
                Result.Name := val;
              except
                Result.Name := '';
              end;
            end;
          end;
        end;
      end;
    finally
      if ALocking then ReleaseLock;
    end;
  end;
end;

function TDataStorage.GetFriendByID(FriendID: Integer;
                                    ALocking: Boolean): TFriendInfo;
var
  Table, val: Variant;
begin
  Result.ID   := -1;
  Result.IP   := '';
  Result.Port := '';
  Result.Host := '';
  Result.Name := '';

  if FriendID <> -1 then
  begin
    if ALocking then GetLock;
    if Assigned(FStorage) then
    begin
      try
        try
          Table := FStorage.ReadValue(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS, Null);
        except
          Table := Null;
        end;
        if VarIsSidexTable(Table) then
        begin
          if VarSidexTableHasColumn(Table, TABLE_COLUMN_ID) then
          begin
            VarSidexTableFirst(Table);
            while not VarSidexTableEof(Table) do
            begin
              val := VarSidexTableGetField(Table, TABLE_COLUMN_ID);
              if VarIsOrdinal(val) then
              begin
                try
                  Result.ID := val;
                except
                  Result.ID := -1;
                end;
                if Result.ID = FriendID then Break
                else Result.ID := -1;
              end;
              VarSidexTableNext(Table);
            end;
          end;

          if not VarSidexTableEof(Table) then
          begin
            if VarSidexTableHasColumn(Table, TABLE_COLUMN_IP) then
            begin
              val := VarSidexTableGetField(Table, TABLE_COLUMN_IP);
              if VarIsStr(val) then
              begin
                try
                  Result.IP := FormatIP(val);
                except
                  Result.IP := '';
                end;
              end;
            end;

            if VarSidexTableHasColumn(Table, TABLE_COLUMN_PORT) then
            begin
              val := VarSidexTableGetField(Table, TABLE_COLUMN_PORT);
              begin
                if VarIsStr(val) then
                begin
                  try
                    Result.Port := FormatPort(val);
                  except
                    Result.Port := '';
                  end;
                end;
              end;
            end;

            if VarSidexTableHasColumn(Table, TABLE_COLUMN_HOST) then
            begin
              val := VarSidexTableGetField(Table, TABLE_COLUMN_HOST);
              begin
                if VarIsStr(val) then
                begin
                  try
                    Result.Host := FormatHostName(val);
                  except
                    Result.Host := '';
                  end;
                end;
              end;
            end;

            if VarSidexTableHasColumn(Table, TABLE_COLUMN_NAME) then
            begin
              val := VarSidexTableGetField(Table, TABLE_COLUMN_NAME);
              begin
                if VarIsStr(val) then
                begin
                  try
                    Result.Name := val;
                  except
                    Result.Name := '';
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        if ALocking then ReleaseLock;
      end;
    end;
  end;
end;

procedure TDataStorage.SignalDataUpdate;
var
  Call: TAsyncCallData;
begin
  Call := TAsyncCallData.Create;
  if Assigned(Call) then
  begin
    Call.ID := ASYNC_LIST_FRIENDS;
    Application.QueueAsyncCall(FAsyncCallMethod, PtrInt(Call));
  end;
end;

//------------------------------------------------------------------------------

constructor TDataProcessingThread.Create(AData: TDataStorage;
                                         ACall: TAsyncCallData);
begin
  // 'TDataProcessingThread' must free 'ACall' after use
  FData := AData;
  FCall := ACall;
  FreeOnTerminate := true;
  inherited Create(false);
end;

destructor TDataProcessingThread.Destroy;
begin
  FData := nil;
  FreeAndNil(FCall);
  inherited Destroy;
end;

procedure TDataProcessingThread.Execute();
var
  cmd:       TTMLCmdMsg;
  err, i, n: Integer;
  fi:        TFriendInfo;
  bSuccess:  Boolean;
begin
  if Assigned(FData) and Assigned(FCall) then
  begin
    if Assigned(FData.TMLCore) and Assigned(FData.TMLProfile) then
    begin
      case FCall.ID of
        ASYNC_ASK_FOR_FRIENDS:
        begin
          cmd := TTMLCmdMsg.Create(CMD_FINDER_ASK_FOR_FRIENDS);
          if Assigned(cmd) then
          begin
            err := FData.TMLCore.CallSync(FData.TMLProfile.Profile,
                                          FCall.Info.IP, FCall.Info.Port,
                                          cmd, 10000);
            if (err = TML_SUCCESS) and not Application.Terminated then
            begin
              FData.MergeFriends(cmd.Data);
              FData.SignalDataUpdate;
            end;
            FreeAndNil(cmd);
          end;
        end;
        ASYNC_ASK_TO_BE_MY_FRIEND:
        begin
          cmd := TTMLCmdMsg.Create(CMD_FINDER_SAY_HELLO);
          if Assigned(cmd) then
          begin
            try
              cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
              cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
              cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
              cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
              err := FData.TMLCore.CallSync(FData.TMLProfile.Profile,
                                            FCall.Info.IP, FCall.Info.Port,
                                            cmd, 10000);
              if (err = TML_SUCCESS) and not Application.Terminated then
              begin
                if cmd.Data.ReadBoolean(GROUP_NAME_ANSWER, PARAM_NAME_ACCEPT, false) then
                begin
                  if FData.AddFriendToList(FCall.Info.IP, FCall.Info.Port,
                                           cmd.Data.ReadString(GROUP_NAME_ANSWER,
                                                               PARAM_NAME_HOST,
                                                               FCall.Info.IP),
                                           cmd.Data.ReadString(GROUP_NAME_ANSWER,
                                                               PARAM_NAME_NAME, '')) then
                  begin
                    FData.SignalDataUpdate;
                  end;
                end;
              end;
            except
              // do nothing
            end;
            FreeAndNil(cmd);
          end;
        end;
        ASYNC_ALIVE_CHECK:
        begin
          if not FData.AliveCheckInProgress then
          begin
            FData.AliveCheckInProgress := true;
            n := FData.FriendCount(true);
            for i := 0 to n - 1 do
            begin
              if Application.Terminated then Break;
              fi := FData.GetFriendByIndex(i, true);
              if fi.ID >= 0 then
              begin
                cmd := TTMLCmdMsg.Create(CMD_FINDER_ALIVE_CHECK);
                if Assigned(cmd) then
                begin
                  bSuccess := false;
                  try
                    err := FData.TMLCore.CallSync(FData.TMLProfile.Profile,
                                                  fi.IP, fi.Port, cmd, 10000);
                    if err = TML_SUCCESS then
                    begin
                      bSuccess := cmd.Data.ReadBoolean(GROUP_NAME_ANSWER,
                                                       PARAM_NAME_ACCEPT,
                                                       false);
                    end;
                  except
                    bSuccess := false;
                  end;
                  if not bSuccess then
                  begin
                    // the alive check wasn't positive - remove friend...
                    if FData.RemoveFriendFromList(fi.IP, fi.Port, true) then
                    begin
                      FData.SignalDataUpdate;
                    end;
                  end;
                  FreeAndNil(cmd);
                end;
              end
              else Break;
            end;
            FData.AliveCheckInProgress := false;
          end;
        end;
        ASYNC_SYNC_DATA_PROCESS:
        begin
          n := FCall.iParam[0];
          for i := 0 to n - 1 do
          begin
            if Terminated then Break;
            cmd := TTMLCmdMsg.Create(CMD_DEMO_DATA_PROCESSING);
            if Assigned(cmd) then
            begin
              try
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
                cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_I_PORTION,    i);
                cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_N_PORTIONS,   n);
                cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_DURATION,     FCall.iParam[1]);
                cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_N_PROGRESSES, FCall.iParam[2]);
                case FCall.iParam[3] of
                  0:  // synchron
                  begin
                    FData.TMLCore.CallSync(FData.TMLProfile.Profile,
                                           FCall.Info.IP, FCall.Info.Port,
                                           cmd, FCall.iParam[4]);
                    // now, we could read a result from cmd
                  end;
                  2:  // Balancer synchron
                  begin
                    FData.TMLProfile.CallBalSync(cmd, FCall.iParam[4]);
                    // now, we could read a result from cmd
                  end;
                end;
              except
                Break;
              end;
              FreeAndNil(cmd);
            end;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

end.

