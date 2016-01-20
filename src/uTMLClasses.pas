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

//------------------------------------------------------------------------------

unit uTMLClasses;

{$if not defined(FPC)}
  {$if CompilerVersion >= 25}
    {$LEGACYIFEND ON}
  {$ifend}
{$ifend}

{$if defined(FPC)}
  {$mode objfpc}{$H+}
{$ifend}

//------------------------------------------------------------------------------

interface

uses
  {$if defined(ANDROID)}
  System.Generics.Collections,
  {$ifend}
  SyncObjs, Classes, SysUtils,
  cSidexDocument, uSidexLib, uTMLCore, uTMLErrors, uTMLTypes, uSidexVariant;

//------------------------------------------------------------------------------

type

ETMLError = class(Exception);

// forward declarations
TTMLCmdMsg  = class;
TTMLCore    = class;
TTMLCommand = class;
TTMLProfile = class;

// TML Profile, Core and Command event functions
TTMLOnProgress     = procedure(Sender: TObject; aCmdMsg: TTMLCmdMsg; aProgress: TML_INT32) of object;
TTMLOnCommandReady = procedure(Sender: TObject; aCmdMsg: TTMLCmdMsg) of object;
TTMLOnStatusReply  = procedure(Sender: TObject; aCmdMsg: TTMLCmdMsg; aType: TML_INT32; aMsg : string) of object;

//#-----------------------------------------------------------------------------
// TML Command Message class
TTMLCmdMsg = class(TObject)
private
  FLastError    : TML_INT32;
  FLastMessage  : string;
  FHandle       : TML_COMMAND_HANDLE;
  FIsOwner      : Boolean;
  FIsAsync      : Boolean;
  FShowProgress : Boolean;
  FTMLCore      : TTMLCore;
  FProfile      : TTMLProfile;
  FProfileName  : string;
  FProgress     : TML_INT32;
  FUID          : Cardinal;
  FData         : TSIDEXDocument;

  FSyncReply    : Boolean;

  FOnProgress     : TTMLOnProgress;
  FOnCommandReady : TTMLOnCommandReady;
  FOnStatusReply  : TTMLOnStatusReply;

  function  GetCommandId: Cardinal;
  procedure SetCommandId(const Value: Cardinal);
  function  GetCreationTime: string;
  function  GetProfileName: string;
  procedure SetProgress(const Value: TML_INT32);
  function  GetError: TML_INT32;
  function  GetErrorMessage: string;
  procedure SetError(const Value: TML_INT32);
  procedure SetErrorMessage(const Value: string);
  function  GetReplyMessage: string;
  function  GetReplyTyp: TML_INT32;
  function  GetData: TSIDEXDocument;

protected
  function  RegisterCallbacks: TML_INT32;

public
  property Handle       : TML_COMMAND_HANDLE read FHandle;
  property CommandId    : TML_COMMAND_ID read GetCommandId    write SetCommandId;
  property CreationTime : string         read GetCreationTime;
  property IsAsync      : Boolean        read FIsAsync;
  property ShowProgress : Boolean        read FShowProgress   write FShowProgress;
  property TMLCore      : TTMLCore       read FTMLCore        write FTMLCore;
  property Profile      : TTMLProfile    read FProfile        write FProfile;
  property ProfileName  : string         read GetProfileName  write FProfileName;
  property Progress     : TML_INT32      read FProgress       write SetProgress;
  property Error        : TML_INT32      read GetError        write SetError;
  property ErrorMessage : string         read GetErrorMessage write SetErrorMessage;
  property ReplyTyp     : TML_INT32      read GetReplyTyp;
  property ReplyMessage : string         read GetReplyMessage;
  property UID          : Cardinal       read FUID            write FUID;
  property LastError    : TML_INT32      read FLastError;
  property LastMessage  : string         read FLastMessage;
  property Data         : TSIDEXDocument read GetData;
  property SyncReply    : Boolean        read FSyncReply      write FSyncReply;

  constructor Create; overload;
  constructor Create(cmdid : TML_COMMAND_ID); overload;
  constructor CreateWithHandle(ahandle : TML_COMMAND_HANDLE;
                               TakeOwnership: Boolean = false); overload;
  destructor  Destroy; override;

  // communication
  procedure SendStatusReply(atype : TML_INT32; amsg : string);

  // Data access functions
  function  AcquireData : TSIDEXDocument;
  procedure ReleaseData;

  // events
  property OnProgress     : TTMLOnProgress     read FOnProgress      write FOnProgress;
  property OnCommandReady : TTMLOnCommandReady read FOnCommandReady  write FOnCommandReady;
  property OnStatusReply  : TTMLOnStatusReply  read FOnStatusReply   write FOnStatusReply;
end;

TTMLCmdMsgList = Array of TTMLCmdMsg;

// TML command callback functions
TTMLOnCmdCall = procedure(Sender : TTMLCommand; acmd : TTMLCmdMsg) of object;

//------------------------------------------------------------------------------
// TML Command class
TTMLCommand = class(TComponent)
private
  FProfile   : TTMLProfile;

  // events
  FOnCmdCall : TTMLOnCmdCall;
  FCommandId : Cardinal;
  FSyncCall  : Boolean;

protected
  procedure ReadState(Reader: TReader); override;
  procedure SetParentComponent(AParent: TComponent); override;

public
  property Profile : TTMLProfile read FProfile write FProfile;

  constructor Create(AOwner: TComponent); reintroduce;
  destructor  Destroy; override;

  function GetParentComponent: TComponent; override;
  function HasParent: Boolean; override;

  procedure Execute(acmd : TTMLCmdMsg);

published
  property CommandId   : Cardinal      read FCommandId write FCommandId;
  property OnCmdCall   : TTMLOnCmdCall read FOnCmdCall write FOnCmdCall;
  property Synchronize : Boolean       read FSyncCall  write FSyncCall;
end;

//#-----------------------------------------------------------------------------
// TML Commands class
TTMLCommands = class(TPersistent)
private
  FProfile : TTMLProfile;
  {$if defined(FPC)}
  FList    : TFPList;
  {$elseif defined(ANDROID)}
  FList    : System.Generics.Collections.TList<TTMLCommand>;
  {$else}
  FList    : TList;
  {$ifend}
  function  GetItem(Index: Integer): TTMLCommand;
  procedure SetItem(Index: Integer; const Value: TTMLCommand);

public
  constructor Create(AProfile : TTMLProfile);
  destructor  Destroy; override;

  function  Add(cmd : TTMLCommand): integer;
  function  Count: Integer;
  function  IndexOf(cmd : TTMLCommand): integer;
  procedure Delete(Index: Integer);

  property  Profile: TTMLProfile read FProfile;
  property  Items[Index: Integer]: TTMLCommand read GetItem write SetItem; default;
end;

TTMLStreamTypeBase = class;
TTMLStreamDownloadCallback = function(Sender:    TTMLStreamTypeBase;
  { Meaning of return value: }        StreamID:  TML_STREAM_ID;
  {  0 = Success             }        Buffer:    TML_POINTER;
  { -1 = Error               }        BufSize:   TML_INT32;
  { -2 = Canceled by user    }        iBuffer:   TML_INT64; // 1..nBuffers
                                      nBuffers:  TML_INT64): TML_INT32 of object;
TTMLStreamProgressCallback = function(Sender:    TTMLStreamTypeBase;
  { Meaning of return value:      }   StreamID:  TML_STREAM_ID;
  { TML_TRUE  = Cancel operation  }   iBuffer:   TML_INT64; // 1..nBuffers
  { TML_FALSE = Continue download }   nBuffers:  TML_INT64;
                                      Percent:   TML_INT32): TML_BOOL of object;
TTMLStreamFinishedCallback = procedure(Sender:   TTMLStreamTypeBase;
                                       StreamID: TML_STREAM_ID;
                                       errCode:  TML_INT32) of object;
TTMLStreamClosedCallback = procedure(Sender:     TTMLStreamTypeBase;
                                     StreamID:   TML_STREAM_ID) of object;
TTMLStreamErrorCallback = procedure(Sender:      TTMLStreamTypeBase;
                                    StreamID:    TML_STREAM_ID;
                                    iError:      TML_INT32) of object;
TTMLStreamTypeBase = class(TObject)
protected
  FTMLProfile:            TTMLProfile;
  FTMLCore:               TTMLCore;

  FReceiverHost:          string;
  FReceiverPort:          string;
  FSenderHost:            string;
  FSenderPort:            string;

  FStreamID:              TML_STREAM_ID;
  FIsSender, FIsReceiver: Boolean;

  FStreamSize:            TML_INT64;
  FStreamPos_Local:       TML_INT64;  // for local access
  FStreamPos_Remote:      TML_INT64;  // for remote access

  FLastError:             TML_INT32;
  FLastPercent:           TML_INT32;  // can be used to limit UI activities

  FSynchronizeCall:       Boolean;
  FDownloadCallback:      TTMLStreamDownloadCallback; // download data
  FFinishedCallback:      TTMLStreamFinishedCallback; // download finished
  FClosedCallback:        TTMLStreamClosedCallback;   // receiver closed
  FErrorCallback:         TTMLStreamErrorCallback;    // receiver disconnects

  {$if defined(FPC)}
    {$if defined(Unix)}
      FStreamLock : TCriticalSection;
    {$else}
      FStreamLock : TRTLCriticalSection;
    {$ifend}
  {$else}
    FStreamLock : TCriticalSection;
  {$ifend}

  constructor Create; overload;
  procedure   InitStream; virtual;
  procedure   PostInitStream; virtual;

  procedure   GetLock;
  procedure   ReleaseLock;

  function    GetStreamSize: TML_INT64; virtual;
  function    GetStreamPos(bRemote: Boolean): TML_INT64; virtual;
  function    _GetStreamPos: TML_INT64; virtual;
  procedure   SetStreamPos(bRemote: Boolean; AStreamPos: TML_INT64); virtual;
  procedure   _SetStreamPos(AStreamPos: TML_INT64); virtual;
  function    ReadFromStream(bRemote: Boolean; Buffer: Pointer;
                             BufferLength: TML_INT32): TML_INT32; virtual;
  function    WriteToStream(bRemote: Boolean; Buffer: Pointer;
                            BufferLength: TML_INT32): Boolean; virtual;

public
  constructor Create(ATMLProfile: TTMLProfile; ReceiverHost, ReceiverPort: string;
                     pCBReceiverStreamClosed: TTMLStreamClosedCallback = nil;
                     pCBReceiverStreamError:  TTMLStreamErrorCallback  = nil); overload;  // Sender
  constructor Create(ATMLProfile: TTMLProfile; AStreamID: TML_STREAM_ID;
                     SenderHost, SenderPort: string); overload; // Receiver
  destructor  Destroy; override;

  function Read(Buffer: Pointer; BufferLength: TML_INT32;
                SeekPos: TML_INT64 = -1 {from current position}): TML_INT32;
  function Write(Buffer: Pointer; BufferLength: TML_INT32;
                 SeekPos: TML_INT64 = -1 {at current position}): Boolean;
  function Download(pCBDownloadFunc: TTMLStreamDownloadCallback;
                    pCBFinishedFunc: TTMLStreamFinishedCallback = nil;
                    ASynchronizeCall:  Boolean = false;
                    DesiredBufferSize: LongInt = 4096): Boolean;

  function IsSender:   Boolean;
  function IsReceiver: Boolean;

  property TMLProfile:   TTMLProfile read FTMLProfile;
  property TMLCore:         TTMLCore read FTMLCore;
  property ReceiverHost:      string read FReceiverHost;
  property ReceiverPort:      string read FReceiverPort;
  property SenderHost:        string read FSenderHost;
  property SenderPort:        string read FSenderPort;
  property StreamID:   TML_STREAM_ID read FStreamID;
  property StreamSize:     TML_INT64 read GetStreamSize;
  property StreamPos:      TML_INT64 read _GetStreamPos write _SetStreamPos;
  property LastError:      TML_INT32 read FLastError;
  property LastPercent:    TML_INT32 read FLastPercent;
  property SynchronizeCall:  Boolean read FSynchronizeCall;
  property DownloadCallback: TTMLStreamDownloadCallback read FDownloadCallback;
  property FinishedCallback: TTMLStreamFinishedCallback read FFinishedCallback;
  property ClosedCallback:   TTMLStreamClosedCallback   read FClosedCallback;
  property ErrorCallback:    TTMLStreamErrorCallback    read FErrorCallback;
end;
TTMLStreamTypeClass = class of TTMLStreamTypeBase;

TTMLStreamTypeMemory = class(TTMLStreamTypeBase)
protected
  FMemoryBuffer: Pointer;
  FMemorySize:   TML_INT32; // size of buffer
  FMemoryLength: TML_INT32; // data length in buffer
  FMemoryOwner:  Boolean;

  procedure InitStream; override;
  function  ReadFromStream(bRemote: Boolean; Buffer: Pointer;
                           BufferLength: TML_INT32): TML_INT32; override;
  function  WriteToStream(bRemote: Boolean; Buffer: Pointer;
                          BufferLength: TML_INT32): Boolean; override;

public
  destructor Destroy; override;

  procedure  SetMemoryBuffer(Buffer: Pointer; BufferSize: TML_INT32);
  function   CreateMemoryBuffer(BufferSize: TML_INT32): Boolean;
  procedure  ReleaseMemoryBuffer;

  function   LoadStreamInMemory: Boolean;   // will be trunced at MemorySize, on given MemoryBuffer

  function   LoadFirstStreamPart: Boolean;  // 'true' = success
  function   LoadNextStreamPart: Boolean;   // 'true' = success

  property   MemoryBuffer: Pointer   read FMemoryBuffer;
  property   MemorySize:   TML_INT32 read FMemorySize;    // size of buffer
  property   MemoryLength: TML_INT32 read FMemoryLength;  // data length in buffer
end;

TTMLStreamTypeFile = class(TTMLStreamTypeBase)
protected
  FLoadFileStream:  TFileStream;
  FSaveFileStream:  TFileStream;
  FLockFileStream:  TFileStream;
  FLoadFileName:    string;
  FSaveFileName:    string;
  FTempFileName:    string;
  FUseTempFile:     Boolean;
  FWritePermission: Boolean;
  FSaveFileResult:  TML_INT32;

  FSaveFileProgressCallback: TTMLStreamProgressCallback;
  FSaveFileFinishedCallback: TTMLStreamFinishedCallback;

  procedure InitStream; override;
  procedure PostInitStream; override;
  function  ReadFromStream(bRemote: Boolean; Buffer: Pointer;
                           BufferLength: TML_INT32): TML_INT32; override;
  function  WriteToStream(bRemote: Boolean; Buffer: Pointer;
                          BufferLength: TML_INT32): Boolean; override;

  procedure SetFileName(AFileName: string);
  procedure SetWritePermission(WritingAllowed: Boolean);

  function  SaveFileCallback(Sender:         TTMLStreamTypeBase;
                             AStreamID:      TML_STREAM_ID;
                             Buffer:         TML_POINTER;
                             BytesRead:      TML_INT32;
                             TotalBytesRead: TML_INT64;
                             theStreamSize:  TML_INT64): TML_INT32;

  procedure SaveFileFinishedCallback(Sender:    TTMLStreamTypeBase;
                                     AStreamID: TML_STREAM_ID;
                                     errCode:   TML_INT32);

public
  destructor Destroy; override;

  function   SaveStreamToFile(AFileName: string = '';
                              UseTempFile: Boolean = false;
                              pCBProgress: TTMLStreamProgressCallback = nil;
                              pCBFinished: TTMLStreamFinishedCallback = nil;
                              ASynchronizeCall: Boolean = false): Boolean;
  procedure  ReleaseFile;

  property   FileName: string read FLoadFileName write SetFileName;
  property   WritePermission: Boolean read FWritePermission write SetWritePermission;
end;

// TML Profile event functions
TTMLOnPopulateReceiver  = procedure(Sender : TTMLProfile) of object;
TTMLOnCustomDispatch    = procedure(Sender : TTMLProfile; cmdid : TML_COMMAND_ID; acmd : TTMLCmdMsg) of object;
TTMLOnQueueOverflow     = procedure(Sender : TTMLProfile; cmdid : TML_COMMAND_ID) of object;
TTMLOnEventError        = procedure(Sender : TTMLProfile; cmdid : TML_COMMAND_ID; Error : TML_INT32; Host, Port : string) of object;
TTMLOnPeerRegister      = function(Sender : TTMLProfile; bSubscribe : Boolean; Host, Port : string): Boolean of object;
TTMLOnBusyStatusRequest = function(Sender : TTMLProfile; acmd : TTMLCmdMsg): TML_INT32 of object;
TTMLOnCalculation       = function(Sender : TTMLProfile; listenerBusyStateArray : TTMLCmdMsgList; out iNextListenerIndex : TML_INT32): TML_INT32 of object;
TTMLOnGetProfileName    = function(Sender : TTMLProfile; CurrentProfileName: string): string of object;

//------------------------------------------------------------------------------
// TML Profile class
TTMLProfile = class(TComponent)
private
  FLastError   : TML_INT32;
  FLastMessage : string;
  FTMLCore     : TTMLCore;
  FProfile     : string;
  FCommands    : TTMLCommands;

  FSyncReply   : Boolean;

  // events
  FOnPopulateEventReceiver  : TTMLOnPopulateReceiver;
  FOnPopulateBalReceiver    : TTMLOnPopulateReceiver;
  FOnProgress               : TTMLOnProgress;
  FOnCommandReady           : TTMLOnCommandReady;
  FOnStatusReply            : TTMLOnStatusReply;
  FOnCustomDispatch         : TTMLOnCustomDispatch;
  FOnQueueOverflow          : TTMLOnQueueOverflow;
  FOnGetProfileName         : TTMLOnGetProfileName;
  FOnEventError             : TTMLOnEventError;
  FOnPeerRegister_Evt       : TTMLOnPeerRegister;
  FOnPeerRegister_Bal       : TTMLOnPeerRegister;
  FOnBusyStatusRequest      : TTMLOnBusyStatusRequest;
  FOnCalculation            : TTMLOnCalculation;

  function  GetCore: TTMLCore;
  procedure SetCore(const Value: TTMLCore);
  function  GetProfile: string;
  procedure SetProfile(const Value: string);
  function  GetCommands: TTMLCommands;
  function  GetRegistered: Boolean;
  procedure SetRegistered(const Value: Boolean);
  procedure SetOnBusyStatusRequest(const val: TTMLOnBusyStatusRequest);
  procedure SetOnCalculation(const val: TTMLOnCalculation);

  // command functions
  procedure RegisterCommand(cmd : TTMLCommand);
  procedure UnregisterCommand(cmd : TTMLCommand);
  procedure RegisterCommands;
  procedure UnregisterCommands;
  procedure RegisterCallbacks;
  procedure UnregisterCallbacks;

protected
  procedure Loaded; override;

public
  property Registered  : Boolean   read GetRegistered write SetRegistered stored False;
  property LastError   : TML_INT32 read FLastError;
  property LastMessage : string    read FLastMessage;

  constructor Create(AOwner: TComponent); override;
  destructor  Destroy; override;

  procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;

  procedure DeleteCommand(cmd : TTMLCommand);
  procedure AddCommand(cmd : TTMLCommand);

  // commands
  function  SendCommandSync(host, port : string;
                            cmd: TTMLCmdMsg; atimeout: Cardinal): TML_INT32;
  function  SendCommandAsync(host, port : string;
                            cmd: TTMLCmdMsg; atimeout: Cardinal): TML_INT32;

  // events
  procedure AddEventDestination(aHost, aPort : string);
  procedure RemoveEventDestination(aHost, aPort : string);
  procedure RemoveAllEventDestinations;
  function  GetEventDestinations: variant;

  procedure RegisterEventPeer(ahost, aport : string; atimeout : Cardinal);
  procedure UnregisterEventPeer(ahost, aport : string; atimeout : Cardinal);

  procedure SendEvent(cmd : TTMLCmdMsg);

  // balancer
  procedure AddBalDestination(aHost, aPort : string);
  procedure RemoveBalDestination(aHost, aPort : string);
  procedure RemoveAllBalDestinations;
  function  GetBalDestinations: variant;

  procedure CallBalAsync(cmd : TTMLCmdMsg; atimeout : Cardinal);
  procedure CallBalSync(cmd : TTMLCmdMsg; atimeout : Cardinal);

  procedure RegisterBalancerPeer(ahost, aport : string; atimeout : Cardinal);
  procedure UnregisterBalancerPeer(ahost, aport : string; atimeout : Cardinal);

  // event dispatcher
  procedure PopulateEventReceiver;
  procedure PopulateBalReceiver;

  // Streaming
  function  CreateStream(StreamType: TTMLStreamTypeClass; ReceiverHost, ReceiverPort: string;
                         pCBReceiverStreamClosed: TTMLStreamClosedCallback = nil;
                         pCBReceiverStreamError:  TTMLStreamErrorCallback  = nil): TTMLStreamTypeBase; // Sender
  function  OpenStream(StreamID: TML_STREAM_ID; StreamType: TTMLStreamTypeClass;
                       SenderHost, SenderPort: string): TTMLStreamTypeBase;  // Receiver

published
  property TMLCore  : TTMLCore     read GetCore    write SetCore;
  property Profile  : string       read GetProfile write SetProfile;
  property Commands : TTMLCommands read GetCommands;
  property SyncReply: boolean      read FSyncReply write FSyncReply;

  // events
  property OnGetProfileName        : TTMLOnGetProfileName    read FOnGetProfileName
                                                            write FOnGetProfileName;
  property OnPopulateEventReceiver : TTMLOnPopulateReceiver  read FOnPopulateEventReceiver
                                                            write FOnPopulateEventReceiver;
  property OnPopulateBalReceiver   : TTMLOnPopulateReceiver  read FOnPopulateBalReceiver
                                                            write FOnPopulateBalReceiver;
  property OnProgress              : TTMLOnProgress          read FOnProgress
                                                            write FOnProgress;
  property OnCommandReady          : TTMLOnCommandReady      read FOnCommandReady
                                                            write FOnCommandReady;
  property OnStatusReply           : TTMLOnStatusReply       read FOnStatusReply
                                                            write FOnStatusReply;
  property OnCustomDispatch        : TTMLOnCustomDispatch    read FOnCustomDispatch
                                                            write FOnCustomDispatch;
  property OnQueueOverflow         : TTMLOnQueueOverflow     read FOnQueueOverflow
                                                            write FOnQueueOverflow;
  property OnEventError            : TTMLOnEventError        read FOnEventError
                                                            write FOnEventError;
  property OnPeerRegister_Evt      : TTMLOnPeerRegister      read FOnPeerRegister_Evt
                                                            write FOnPeerRegister_Evt;
  property OnPeerRegister_Bal      : TTMLOnPeerRegister      read FOnPeerRegister_Bal
                                                            write FOnPeerRegister_Bal;
  property OnBusyStatusRequest     : TTMLOnBusyStatusRequest read FOnBusyStatusRequest
                                                            write SetOnBusyStatusRequest;
  property OnCalculation           : TTMLOnCalculation       read FOnCalculation
                                                            write SetOnCalculation;
end;

//------------------------------------------------------------------------------
// TML Core class
TTMLCore = class(TComponent)
protected
  FProfiles       : Array of TTMLProfile; // list of linked profiles

private
  FTMLCoreHandle  : TML_CORE_HANDLE;
  FLastError      : TML_INT32;
  FLastMessage    : string;
  FDesignIndex    : Integer;    // used by design component only!
  FSyncReply      : Boolean;

  // events
  FOnProgress     : TTMLOnProgress;
  FOnCommandReady : TTMLOnCommandReady;
  FOnStatusReply  : TTMLOnStatusReply;

  FCurrentUID : Cardinal;
  {$if defined(FPC)}
    {$if defined(Unix)}
      FThreadLock : TCriticalSection;
    {$else}
      FThreadLock : TRTLCriticalSection;
    {$ifend}
  {$else}
    FThreadLock : TCriticalSection;
  {$ifend}

  function  GetListenerIP: string;
  procedure SetListenerIP(const Value: string);
  function  GetListenerPort: string;
  procedure SetListenerPort(const Value: string);
  function  GetListenerEnabled: Boolean;
  procedure SetListenerEnabled(const Value: Boolean);
  function  GetLogLevel: TML_INT32;
  procedure SetLogLevel(const Value: TML_INT32);
  function  GetMaxEvtFailCount: Cardinal;
  procedure SetMaxEvtFailCount(const Value: Cardinal);
  function  GetMaxQueuedEvents: Cardinal;
  procedure SetMaxQueuedEvents(const Value: Cardinal);
  function  GetSidexCopyright: string;
  function  GetSidexVersion: string;
  function  GetTmlCopyright: string;
  function  GetTmlVersion: string;
  function  GetMaxBalFailCount: Cardinal;
  procedure SetMaxBalFailCount(const Value: Cardinal);
  function  GetWindowSize: TML_INT32;
  procedure SetWindowSize(const Value: TML_INT32);

protected
  procedure AddProfile(Profile: TTMLProfile);
  procedure RemoveProfile(Profile: TTMLProfile);
  function  FindProfile(Profile: TTMLProfile): Integer;
  procedure RegisterProfile(Profile: TTMLProfile);
  procedure UnregisterProfile(Profile: TTMLProfile);

public
  property TMLCoreHandle : TML_CORE_HANDLE read FTMLCoreHandle;
  property LastError     : TML_INT32       read FLastError;
  property LastMessage   : string          read FLastMessage;
  property WindowSize    : TML_INT32       read GetWindowSize write SetWindowSize default 32768;

  constructor Create(AOwner: TComponent); override;
  destructor  Destroy; override;

  function    GeneralDeregistration: Boolean;

  class procedure SetPassword(User, Pass: string);

  function  CreateUID : Cardinal;

  // profile functions
  function  IsProfileRegistered(profile : string) : Boolean;
  function  GetEventDestinations: variant;
  function  GetBalDestinations: variant;

  // sending functions
  function CallSync(profile, host, port : string;
                    cmd: TTMLCmdMsg; atimeout: Cardinal): TML_INT32;
  function CallAsync(profile, host, port : string;
                     cmd: TTMLCmdMsg; atimeout: Cardinal): TML_INT32;

published
  property LogLevel       : TML_INT32 read GetLogLevel        write SetLogLevel default TML_LOG_OFF;
  property ListenerIP     : string    read GetListenerIP      write SetListenerIP;
  property ListenerPort   : string    read GetListenerPort    write SetListenerPort;
  property ListenerEnabled: Boolean   read GetListenerEnabled write SetListenerEnabled;
  property MaxEvtFailCount: Cardinal  read GetMaxEvtFailCount write SetMaxEvtFailCount default 1;
  property MaxBalFailCount: Cardinal  read GetMaxBalFailCount write SetMaxBalFailCount default 1;
  property MaxQueuedEvents: Cardinal  read GetMaxQueuedEvents write SetMaxQueuedEvents default 1000;
  property sidexCopyright : string    read GetSidexCopyright;
  property sidexVersion   : string    read GetSidexVersion;
  property tmlCopyright   : string    read GetTmlCopyright;
  property tmlVersion     : string    read GetTmlVersion;
  property SyncReply      : boolean   read FSyncReply         write FSyncReply;

  // events
  property OnProgress     : TTMLOnProgress     read FOnProgress      write FOnProgress;
  property OnCommandReady : TTMLOnCommandReady read FOnCommandReady  write FOnCommandReady;
  property OnStatusReply  : TTMLOnStatusReply  read FOnStatusReply   write FOnStatusReply;
end;

//------------------------------------------------------------------------------
// Critical section for synchronized execute of commands

TTMLOnSyncToMainThread_Function = function(aCmdMsg: TTMLCmdMsg;
                                           aInt:    TML_INT32;
                                           aMsg:    string;
                                           aData:   TObject): TML_INT32;
TTMLOnSyncToMainThread_Method   = function(aCmdMsg: TTMLCmdMsg;
                                           aInt:    TML_INT32;
                                           aMsg:    string;
                                           aData:   TObject): TML_INT32 of object;

TSyncExecuteCritSec = class(TCriticalSection)
public
  CmdCallMethod      : TTMLOnCmdCall;
  CmdRdyMethod       : TTMLOnCommandReady;
  CmdProgressMethod  : TTMLOnProgress;
  CmdStatusMethod    : TTMLOnStatusReply;
  DownloadMethod     : TTMLStreamDownloadCallback;
  FinishedMethod     : TTMLStreamFinishedCallback;
  SyncToMainFunction : TTMLOnSyncToMainThread_Function;
  SyncToMainMethod   : TTMLOnSyncToMainThread_Method;
  Command            : TTMLCommand;
  CmdMsg             : TTMLCmdMsg;
  Core               : TTMLCore;
  Profile            : TTMLProfile;
  stb                : TTMLStreamTypeBase;
  StreamID           : TML_STREAM_ID;
  p1                 : TML_POINTER;
  iResult, i1        : TML_INT32;
  l1, l2             : TML_INT64;
  s1                 : string;
  o1                 : TObject;

  procedure Clear;
  procedure SyncExecute;
end;


function TMLSyncToMainThread(aCallBack: TTMLOnSyncToMainThread_Function;
                             aCmdMsg:   TTMLCmdMsg = nil;
                             aInteger:  TML_INT64  = 0;
                             aString:   string     = '';
                             aObject:   TObject    = nil): TML_INT32; overload;
function TMLSyncToMainThread(aCallBack: TTMLOnSyncToMainThread_Method;
                             aCmdMsg:   TTMLCmdMsg = nil;
                             aInteger:  TML_INT64  = 0;
                             aString:   string     = '';
                             aObject:   TObject    = nil): TML_INT32; overload;

//------------------------------------------------------------------------------

function FormatHostName(HostName: string): string;
function FormatIP(IP: string): string;
function FormatPort(Port: string): string;
function IsIPAddress(IP: string): Boolean;

// stream callback functions
function OnStreamGetPosition(iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER): TML_INT64; cdecl;
function OnStreamGetSize    (iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER): TML_INT64; cdecl;
function OnStreamRead       (iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER;
                             buffer       : TML_POINTER;
                             count        : TML_INT32;
                         out bytesRead    : TML_INT32): TML_INT32; cdecl;
function OnStreamWrite      (iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER;
                             buffer       : TML_POINTER;
                             count        : TML_INT32): TML_INT32; cdecl;
function OnStreamSeek       (iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER;
                             seekPosition : TML_INT64;
                             seekOrigin   : TML_SEEK_ORIGIN): TML_INT32; cdecl;
procedure OnReceiverStreamClosed(iStreamID: TML_STREAM_ID;
                             pCBData      : TML_POINTER); cdecl;
procedure OnReceiverStreamError(iStreamID : TML_STREAM_ID;
                             iError       : TML_INT32;
                             pCBData      : TML_POINTER); cdecl;
function OnStreamDownload   (iStreamID    : TML_STREAM_ID;
                             pCBData      : TML_POINTER;
                             buffer       : TML_POINTER;
                             bytesRead    : TML_INT32;
                             actBufferNo  : TML_INT64;
                             bufferCount  : TML_INT64): TML_INT32; cdecl;
procedure OnStreamFinished(  iStreamID    : TML_STREAM_ID;
                             errCode      : TML_INT32;
                             pCBDataRet   : TML_POINTER); cdecl;

//------------------------------------------------------------------------------

implementation

uses
  {$if defined(Windows) or defined(MSWINDOWS)}
    Windows,
  {$ifend}
  {$if defined(ANDROID)}
    Posix.Unistd, Posix.Stdio,
  {$ifend}
  DateUtils, uSidexTypes, uSidexErrors;

//------------------------------------------------------------------------------

var
  tmlSyncCallCritSect : TSyncExecuteCritSec;

//------------------------------------------------------------------------------

procedure TSyncExecuteCritSec.Clear;
begin
  CmdCallMethod      := nil;
  CmdRdyMethod       := nil;
  CmdProgressMethod  := nil;
  CmdStatusMethod    := nil;
  DownloadMethod     := nil;
  FinishedMethod     := nil;
  SyncToMainFunction := nil;
  SyncToMainMethod   := nil;
  Command            := nil;
  CmdMsg             := nil;
  Core               := nil;
  Profile            := nil;
  stb                := nil;
  p1                 := nil;
  o1                 := nil;
  StreamID           := 0;
  i1                 := 0;
  l1                 := 0;
  l2                 := 0;
  s1                 := '';
end;

procedure TSyncExecuteCritSec.SyncExecute;
begin
  iResult := 0;
  if Assigned(CmdCallMethod) then
  begin
    CmdCallMethod(Command, CmdMsg);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end
  else if Assigned(CmdRdyMethod) then
  begin
    if assigned(Profile) then
        CmdRdyMethod(Profile, CmdMsg)
    else if Assigned(Core) then
        CmdRdyMethod(Core, CmdMsg)
    else
        CmdRdyMethod(CmdMsg, CmdMsg);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end
  else if Assigned(CmdProgressMethod) then
  begin
    if assigned(Profile) then
        CmdProgressMethod(Profile, CmdMsg, i1)
    else if Assigned(Core) then
        CmdProgressMethod(Core, CmdMsg, i1)
    else
        CmdProgressMethod(CmdMsg, CmdMsg, i1);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end
  else if Assigned(CmdStatusMethod) then
  begin
    if assigned(Profile) then
        CmdStatusMethod(Profile, CmdMsg, i1, s1)
    else if Assigned(Core) then
        CmdStatusMethod(Core, CmdMsg, i1, s1)
    else
        CmdStatusMethod(CmdMsg, CmdMsg, i1, s1);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end
  else if Assigned(DownloadMethod) then
  begin
    iResult := DownloadMethod(stb, StreamID, p1, i1, l1, l2);
  end
  else if Assigned(FinishedMethod) then
  begin
    FinishedMethod(stb, StreamID, i1);
  end
  else if Assigned(SyncToMainMethod) then
  begin
    iResult := SyncToMainMethod(CmdMsg, l1, s1, o1);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end
  else if Assigned(SyncToMainFunction) then
  begin
    iResult := SyncToMainFunction(CmdMsg, l1, s1, o1);
    if Assigned(CmdMsg) then CmdMsg.ReleaseData;
  end;
  Clear;
end;

function TMLSyncToMainThread(aCallBack: TTMLOnSyncToMainThread_Function;
                             aCmdMsg:   TTMLCmdMsg;
                             aInteger:  TML_INT64;
                             aString:   string;
                             aObject:   TObject): TML_INT32;
begin
  Result := -1;
  if not Assigned(tmlSyncCallCritSect) then
  begin
    tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
  end;
  if Assigned(tmlSyncCallCritSect) then
  begin
    if Assigned(aCmdMsg) then aCmdMsg.ReleaseData;
    tmlSyncCallCritSect.Enter;
    try
      tmlSyncCallCritSect.Clear;
      tmlSyncCallCritSect.SyncToMainFunction := aCallBack;
      tmlSyncCallCritSect.CmdMsg             := aCmdMsg;
      tmlSyncCallCritSect.l1                 := aInteger;
      tmlSyncCallCritSect.s1                 := aString;
      tmlSyncCallCritSect.o1                 := aObject;
      {$if  defined(FPC)}
        TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
      {$else}
        TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
      {$ifend}
      Result := tmlSyncCallCritSect.iResult;
    finally
      tmlSyncCallCritSect.Leave;
    end;
  end;
end;

function TMLSyncToMainThread(aCallBack: TTMLOnSyncToMainThread_Method;
                             aCmdMsg:   TTMLCmdMsg;
                             aInteger:  TML_INT64;
                             aString:   string;
                             aObject:   TObject): TML_INT32;
begin
  Result := -1;
  if not Assigned(tmlSyncCallCritSect) then
  begin
    tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
  end;
  if Assigned(tmlSyncCallCritSect) then
  begin
    if Assigned(aCmdMsg) then aCmdMsg.ReleaseData;
    tmlSyncCallCritSect.Enter;
    try
      tmlSyncCallCritSect.Clear;
      tmlSyncCallCritSect.SyncToMainMethod := aCallBack;
      tmlSyncCallCritSect.CmdMsg           := aCmdMsg;
      tmlSyncCallCritSect.l1               := aInteger;
      tmlSyncCallCritSect.s1               := aString;
      tmlSyncCallCritSect.o1               := aObject;
      {$if  defined(FPC)}
        TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
      {$else}
        TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
      {$ifend}
      Result := tmlSyncCallCritSect.iResult;
    finally
      tmlSyncCallCritSect.Leave;
    end;
  end;
end;

//#---------------------------------------------------------------------
//                     CALLBACK PROCEDURES
//#---------------------------------------------------------------------

procedure OnEventErrorClb(sProfile, sHost, sPort : PSIDEX_STRING;
                          cmd       : TML_COMMAND_ID;
                          ErrorCode : TML_INT32;
                          pCBData   : TML_POINTER); cdecl;
var
  core             : TTMLCore;
  profile          : TTMLProfile;
  prof, host, port : string;
  n, i             : Integer;
begin
  core := TTMLCore(pCBData);
  if Assigned(core) and Assigned(sProfile) then
  begin
    prof := sProfile;
    if prof <> '' then
    begin
      if Assigned(sHost) then host := sHost else host := '';
      if Assigned(sPort) then port := sPort else port := '';

      n := Length(core.FProfiles);
      for i := 0 to n - 1 do
      begin
        profile := core.FProfiles[i];
        if Assigned(profile) then
        begin
          if profile.Profile = prof then
          begin
            if Assigned(profile.OnEventError) then
            begin
              profile.OnEventError(profile, cmd, ErrorCode, host, port);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnQueueOverflowClb(sProfile : PSIDEX_STRING;
                             cmdid    : TML_COMMAND_ID;
                             pCBData  : TML_POINTER); cdecl;
var
  core    : TTMLCore;
  profile : TTMLProfile;
  prof    : string;
  n, i    : Integer;
begin
  core := TTMLCore(pCBData);
  if Assigned(core) and Assigned(sProfile) then
  begin
    prof := sProfile;
    if prof <> '' then
    begin
      n := Length(core.FProfiles);
      for i := 0 to n - 1 do
      begin
        profile := core.FProfiles[i];
        if Assigned(profile) then
        begin
          if profile.Profile = prof then
          begin
            if Assigned(profile.OnQueueOverflow) then
            begin
              profile.OnQueueOverflow(profile, cmdid);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnCustomDispatchClb(cmd       : TML_COMMAND_ID;
                              tmlhandle : TML_COMMAND_HANDLE;
                              pCBData   : TML_POINTER); cdecl;
var
  core              : TTMLCore;
  profile, profile2 : TTMLProfile;
  prof              : string;
  cmdmsg            : TTMLCmdMsg;
  n, i              : Integer;
begin
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then
  begin
    prof := profile.Profile;
    core := profile.TMLCore;
    if Assigned(core) then
    begin
      cmdmsg := TTMLCmdMsg.CreateWithHandle(tmlhandle);
      if Assigned(cmdmsg) then
      begin
        try
          if Assigned(profile.OnCustomDispatch) then
          begin
            cmdmsg.TMLCore := core;
            cmdmsg.Profile := profile;
            profile.OnCustomDispatch(profile, cmd, cmdmsg);
            cmdmsg.ReleaseData;
          end;

          n := Length(core.FProfiles);
          for i := 0 to n - 1 do
          begin
            profile2 := core.FProfiles[i];
            if Assigned(profile2) and (profile2 <> profile) then
            begin
              if profile2.Profile = prof then
              begin
                if Assigned(profile2.OnCustomDispatch) then
                begin
                  cmdmsg.TMLCore := core;
                  cmdmsg.Profile := profile2;
                  profile2.OnCustomDispatch(profile2, cmd, cmdmsg);
                  cmdmsg.ReleaseData;
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(cmdmsg); // will call cmdmsg.ReleaseData
        end;
      end;
    end;
  end;
end;

procedure ExecuteCommandClb(tmlhandle : TML_COMMAND_HANDLE;
                            pCBData   : TML_POINTER); cdecl;
var
  command : TTMLCommand;
  cmdmsg  : TTMLCmdMsg;
begin
  command := TTMLCommand(pCBData);
  if Assigned(command) then
  begin
    cmdmsg := TTMLCmdMsg.CreateWithHandle(tmlhandle);
    if Assigned(cmdmsg) then
    begin
      try
        cmdmsg.Profile := command.Profile;
        if not Assigned(command.Profile) then cmdmsg.TMLCore := nil
        else cmdmsg.TMLCore := command.Profile.TMLCore;
        command.Execute(cmdmsg);
      finally
        FreeAndNil(cmdmsg); // will call cmdmsg.ReleaseData
      end;
    end;
  end;
end;

procedure OnProgressClb(tmlhandle : TML_COMMAND_HANDLE;
                        pCBData   : TML_POINTER;
                        progress  : TML_INT32); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.OnProgress) then
    begin
      if cmdmsg.SyncReply and cmdmsg.IsAsync then
      begin
        if not Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
        end;
        if Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect.Enter;
          try
            tmlSyncCallCritSect.Clear;
            tmlSyncCallCritSect.CmdMsg             := cmdmsg;
            tmlSyncCallCritSect.i1                 := progress;
            tmlSyncCallCritSect.CmdProgressMethod  := cmdmsg.OnProgress;
            {$if  defined(FPC)}
              TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
            {$else}
              TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
            {$ifend}
          finally
            tmlSyncCallCritSect.Leave;
          end;
        end;
      end
      else
      begin
        try
          cmdmsg.OnProgress(cmdmsg, cmdmsg, progress);
        finally
          cmdmsg.ReleaseData;
        end;
      end;
    end;
  end;
end;

procedure OnStatusReplyClb(tmlhandle : TML_COMMAND_HANDLE;
                           pCBData   : TML_POINTER;
                           atype     : TML_INT32;
                           msg       : PSIDEX_STRING); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.OnStatusReply) then
    begin
      if cmdmsg.SyncReply and cmdmsg.IsAsync then
      begin
        if not Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
        end;
        if Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect.Enter;
          try
            tmlSyncCallCritSect.Clear;
            tmlSyncCallCritSect.CmdMsg             := cmdmsg;
            tmlSyncCallCritSect.i1                 := atype;
            tmlSyncCallCritSect.s1                 := msg;
            tmlSyncCallCritSect.CmdStatusMethod    := cmdmsg.OnStatusReply;
            {$if  defined(FPC)}
              TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
            {$else}
              TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
            {$ifend}
          finally
            tmlSyncCallCritSect.Leave;
          end;
        end;
      end
      else
      begin
        try
          cmdmsg.OnStatusReply(cmdmsg, cmdmsg, atype, msg);
        finally
          cmdmsg.ReleaseData;
        end;
      end;
    end;
  end;
end;

procedure OnCommandReadyClb(tmlhandle : TML_COMMAND_HANDLE;
                            pCBData   : TML_POINTER); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.OnCommandReady) then
      if cmdmsg.SyncReply and cmdmsg.IsAsync then
      begin
        if not Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
        end;
        if Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect.Enter;
          try
            tmlSyncCallCritSect.Clear;
            tmlSyncCallCritSect.CmdMsg        := cmdmsg;
            tmlSyncCallCritSect.CmdRdyMethod  := cmdmsg.OnCommandReady;
            {$if  defined(FPC)}
              TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
            {$else}
              TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
            {$ifend}
          finally
            tmlSyncCallCritSect.Leave;
          end;
        end;
      end
      else
      begin
        try
          cmdmsg.OnCommandReady(cmdmsg, cmdmsg);
        finally
          cmdmsg.ReleaseData;
        end;
      end;

    // command finished - free the command and the handle...
    if cmdmsg.IsAsync then
    begin
      {$if defined(AUTOREFCOUNT)}
        cmdmsg.__ObjRelease;
      {$ifend}
      FreeAndNil(cmdmsg);
    end;
  end;
end;

procedure OnCoreProgressClb(tmlhandle : TML_COMMAND_HANDLE;
                            pCBData   : TML_POINTER;
                            progress  : TML_INT32); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.TMLCore) then
    begin
      if Assigned(cmdmsg.TMLCore.OnProgress) then
      begin
        if cmdmsg.TMLCore.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg             := cmdmsg;
              tmlSyncCallCritSect.i1                 := progress;
              tmlSyncCallCritSect.CmdProgressMethod  := cmdmsg.TMLCore.OnProgress;
              tmlSyncCallCritSect.Core               := cmdmsg.TMLCore;

              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.TMLCore.OnProgress(cmdmsg.TMLCore, cmdmsg, progress);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnCoreStatusReplyClb(tmlhandle : TML_COMMAND_HANDLE;
                               pCBData   : TML_POINTER;
                               atype     : TML_INT32;
                               msg       : PSIDEX_STRING); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.TMLCore) then
    begin
      if Assigned(cmdmsg.TMLCore.OnStatusReply) then
      begin
        if cmdmsg.TMLCore.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg             := cmdmsg;
              tmlSyncCallCritSect.i1                 := atype;
              tmlSyncCallCritSect.s1                 := msg;
              tmlSyncCallCritSect.CmdStatusMethod    := cmdmsg.TMLCore.OnStatusReply;
              tmlSyncCallCritSect.Core               := cmdmsg.TMLCore;
              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.TMLCore.OnStatusReply(cmdmsg.TMLCore, cmdmsg, atype, msg);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnCoreCommandReadyClb(tmlhandle : TML_COMMAND_HANDLE;
                                pCBData   : TML_POINTER); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.TMLCore) then
    begin
      if Assigned(cmdmsg.TMLCore.OnCommandReady) then
        if cmdmsg.TMLCore.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg        := cmdmsg;
              tmlSyncCallCritSect.CmdRdyMethod  := cmdmsg.TMLCore.OnCommandReady;
              tmlSyncCallCritSect.Core          := cmdmsg.TMLCore;
              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.TMLCore.OnCommandReady(cmdmsg.TMLCore, cmdmsg);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
    end;
    // command finished - free the command and the handle...
    if cmdmsg.IsAsync then
    begin
      {$if defined(AUTOREFCOUNT)}
        cmdmsg.__ObjRelease;
      {$ifend}
      FreeAndNil(cmdmsg);
    end;
  end;
end;

procedure OnProfileProgressClb(tmlhandle : TML_COMMAND_HANDLE;
                               pCBData   : TML_POINTER;
                               progress  : TML_INT32); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.Profile) then
    begin
      if Assigned(cmdmsg.Profile.OnProgress) then
      begin
        if cmdmsg.Profile.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg             := cmdmsg;
              tmlSyncCallCritSect.i1                 := progress;
              tmlSyncCallCritSect.CmdProgressMethod  := cmdmsg.Profile.OnProgress;
              tmlSyncCallCritSect.Profile            := cmdmsg.Profile;

              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.Profile.OnProgress(cmdmsg.Profile, cmdmsg, progress);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnProfileStatusReplyClb(tmlhandle : TML_COMMAND_HANDLE;
                                  pCBData   : TML_POINTER;
                                  atype     : TML_INT32;
                                  msg       : PSIDEX_STRING); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.Profile) then
    begin
      if Assigned(cmdmsg.Profile.OnStatusReply) then
      begin
        if cmdmsg.Profile.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg             := cmdmsg;
              tmlSyncCallCritSect.i1                 := atype;
              tmlSyncCallCritSect.s1                 := msg;
              tmlSyncCallCritSect.CmdStatusMethod    := cmdmsg.Profile.OnStatusReply;
              tmlSyncCallCritSect.Profile            := cmdmsg.Profile;
              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.Profile.OnStatusReply(cmdmsg.Profile, cmdmsg, atype, msg);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
      end;
    end;
  end;
end;

procedure OnProfileCommandReadyClb(tmlhandle : TML_COMMAND_HANDLE;
                                   pCBData   : TML_POINTER); cdecl;
var
  cmdmsg : TTMLCmdMsg;
begin
  cmdmsg := TTMLCmdMsg(pCBData);
  if Assigned(cmdmsg) then
  begin
    if Assigned(cmdmsg.Profile) then
    begin
      if Assigned(cmdmsg.Profile.OnCommandReady) then
        if cmdmsg.Profile.SyncReply and cmdmsg.IsAsync then
        begin
          if not Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
          end;
          if Assigned(tmlSyncCallCritSect) then
          begin
            tmlSyncCallCritSect.Enter;
            try
              tmlSyncCallCritSect.Clear;
              tmlSyncCallCritSect.CmdMsg        := cmdmsg;
              tmlSyncCallCritSect.CmdRdyMethod  := cmdmsg.Profile.OnCommandReady;
              tmlSyncCallCritSect.Profile       := cmdmsg.Profile;
              {$if  defined(FPC)}
                TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
              {$else}
                TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
              {$ifend}
            finally
              tmlSyncCallCritSect.Leave;
            end;
          end;
        end
        else
        begin
          try
            cmdmsg.Profile.OnCommandReady(cmdmsg.Profile, cmdmsg);
          finally
            cmdmsg.ReleaseData;
          end;
        end;
    end;
    // command finished - free the command and the handle...
    if cmdmsg.IsAsync then
    begin
      {$if defined(AUTOREFCOUNT)}
        cmdmsg.__ObjRelease;
      {$ifend}
      FreeAndNil(cmdmsg);
    end;
  end;
end;

function OnPeerRegister_EvtClb(bSubscribe   : TML_BOOL;
                               sHost, sPort : PSIDEX_STRING;
                               pCBData      : TML_POINTER) : TML_BOOL; cdecl;
var
  profile    : TTMLProfile;
  host, port : string;
begin
  Result  := TML_FALSE;
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then
  begin
    if Assigned(profile.OnPeerRegister_Evt) then
    begin
      if Assigned(sHost) then host := sHost else host := '';
      if Assigned(sPort) then port := sPort else port := '';
      if profile.OnPeerRegister_Evt(profile, bSubscribe = TML_TRUE,
                                    host, port) then
      begin
        Result := TML_TRUE;
      end;
    end;
  end;
end;

function OnPeerRegister_BalClb(bSubscribe   : TML_BOOL;
                               sHost, sPort : PSIDEX_STRING;
                               pCBData      : TML_POINTER) : TML_BOOL; cdecl;
var
  profile    : TTMLProfile;
  host, port : string;
begin
  Result  := TML_FALSE;
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then
  begin
    if Assigned(profile.OnPeerRegister_Bal) then
    begin
      if Assigned(sHost) then host := sHost else host := '';
      if Assigned(sPort) then port := sPort else port := '';
      if profile.OnPeerRegister_Bal(profile, bSubscribe = TML_TRUE,
                                    host, port) then
      begin
        Result := TML_TRUE;
      end;
    end;
  end;
end;

procedure OnPopulateEventReceiverClb(sProfile : PSIDEX_STRING;
                                     pCBData  : TML_POINTER); cdecl;
var
  profile : TTMLProfile;
begin
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then profile.PopulateEventReceiver;
end;

procedure OnPopulateBalReceiverClb(sProfile : PSIDEX_STRING;
                                   pCBData  : TML_POINTER); cdecl;
var
  profile : TTMLProfile;
begin
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then profile.PopulateBalReceiver;
end;

function OnBusyStatusRequestClb(tmlhandle : TML_COMMAND_HANDLE;
                                pCBData   : TML_POINTER): TML_INT32; cdecl;
var
  cmdmsg  : TTMLCmdMsg;
  profile : TTMLProfile;
begin
  Result  := TML_ERR_INFORMATION_UNDEFINED;
  profile := TTMLProfile(pCBData);
  if Assigned(profile) then
  begin
    if Assigned(profile.OnBusyStatusRequest) then
    begin
      cmdmsg := TTMLCmdMsg.CreateWithHandle(tmlhandle);
      if Assigned(cmdmsg) then
      begin
        try
          Result := profile.OnBusyStatusRequest(profile, cmdmsg);
        finally
          FreeAndNil(cmdmsg); // will call cmdmsg.ReleaseData
        end;
      end;
    end;
  end;
end;

function OnCalculationClb(iCountOfDestinations   : TML_INT32;
                          listenerBusyStateArray : PTML_COMMAND_HANDLE_ARRAY;
                          pCBData                : TML_POINTER;
                          out iNextListenerIndex : TML_INT32): TML_INT32; cdecl;
var
  cmds    : TTMLCmdMsgList;
  profile : TTMLProfile;
  nextListenerIndex, i : TML_INT32;
begin
  Result  := TML_ERR_INFORMATION_UNDEFINED;
  profile := TTMLProfile(pCBData);
  if Assigned(profile) and (iCountOfDestinations > 0) then
  begin
    if Assigned(profile.OnCalculation) then
    begin
      SetLength(cmds, iCountOfDestinations);
      for i := 0 to iCountOfDestinations - 1 do
      begin
        cmds[i] := TTMLCmdMsg.CreateWithHandle(listenerBusyStateArray^[i]);
      end;
      try
        nextListenerIndex := 0;
        Result := profile.OnCalculation(profile, cmds, nextListenerIndex);
        iNextListenerIndex := nextListenerIndex;
      finally
        for i := 0 to iCountOfDestinations - 1 do
        begin
          FreeAndNil(cmds[i]); // will call cmds[i].ReleaseData
        end;
      end;
    end;
  end;
end;

function FormatHostName(HostName: string): string;
begin
  Result := AnsiUpperCase(Trim(HostName));
end;

function FormatIP(IP: string): string;
var
  i, n: Integer;
  Part: string;
  Flag: Boolean;
begin
  IP := Trim(IP);
  n := Length(IP);
  Part := '';
  Result := '';
  Flag := false;
  for i := 1 to n do
  begin
    if (IP[i] <> '.') and (Flag or (IP[i] <> '0')) then
    begin
      Flag := true;
      Part := Part + IP[i];
    end;
    if (IP[i] = '.') or (IP[i] <= ' ') or (i = n) then
    begin
      if Result <> '' then Result := Result + '.';
      Part := Trim(Part);
      if Part = '' then Part := '0';
      Result := Result + Part;
      Part := '';
      Flag := false;
      if IP[i] <= ' ' then Break;
    end;
  end;
end;

function FormatPort(Port: string): string;
begin
  Result := Trim(Port);
end;

function IsIPAddress(IP: string): Boolean;
var
  i, n, v, nParts: Integer;
  Part: string;
begin
  IP := Trim(IP);
  n := Length(IP);
  nParts := 0;
  Result := (n >= 3);
  if Result then
  begin
    Part := '';
    for i := 1 to n do
    begin
      if IP[i] <> '.' then Part := Part + IP[i];
      if (IP[i] = '.') or (i = n) then
      begin
        Part := Trim(Part);
        if Part = '' then Part := '0';
        v := -1;
        if TryStrToInt(Part, v) then
        begin
          if (v < 0) or (v > 255) then Result := false
          else Part := '';
        end
        else Result := false;
        if Result then nParts := nParts + 1;
      end;
      if not Result then Break;
    end;
  end;
  Result := Result and (nParts = 4);
end;

//#---------------------------------------------------------------------
{ TTMLCore }
//#---------------------------------------------------------------------

constructor TTMLCore.Create(AOwner: TComponent);
var
  err : TML_INT32;
begin
  inherited Create(AOwner);

  if csDesigning in ComponentState then
  begin
    FDesignIndex := Length(tmlDesignInfos);
    SetLength(tmlDesignInfos, FDesignIndex + 1);
    // apply the default values of the properties here...
    tmlDesignInfos[FDesignIndex].ListenerEnabled := false;
    tmlDesignInfos[FDesignIndex].ListenerIP      := '';
    tmlDesignInfos[FDesignIndex].ListenerPort    := '';
    tmlDesignInfos[FDesignIndex].MaxEvtFailCount := 1;
    tmlDesignInfos[FDesignIndex].MaxBalFailCount := 1;
    tmlDesignInfos[FDesignIndex].MaxQueuedEvents := 1000;
    tmlDesignInfos[FDesignIndex].WindowSize      := 32768;
    tmlDesignInfos[FDesignIndex].LogLevel        := TML_LOG_OFF;
  end;

  FOnProgress          := nil;
  FOnCommandReady      := nil;
  FOnStatusReply       := nil;
  FSyncReply           := false;

  SetLength(FProfiles, 0);

  FCurrentUID     := 0;
  {$if defined(FPC)}
    {$if defined(Unix)}
      FThreadLock := TCriticalSection.Create;
    {$else}
      InitializeCriticalSection(FThreadLock);
    {$ifend}
  {$else}
    FThreadLock := TCriticalSection.Create;
  {$ifend}

  FTMLCoreHandle := TML_CORE_HANDLE_NULL;
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Open(FTMLCoreHandle, 0);
    if err <> TML_SUCCESS then
    begin
      FTMLCoreHandle := TML_CORE_HANDLE_NULL;
    end;
  end;
end;

destructor TTMLCore.Destroy;
begin
  if FTMLCoreHandle <> TML_CORE_HANDLE_NULL then
  begin
    ListenerEnabled := False;
    FLastError      := tml_Core_Close(FTMLCoreHandle);
    FTMLCoreHandle  := TML_CORE_HANDLE_NULL;
    if FLastError <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Core_Close failed (%d)', [FLastError]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
  {$if defined(FPC)}
    {$if defined(Unix)}
      FreeAndNil(FThreadLock);
    {$else}
      DeleteCriticalSection(FThreadLock);
    {$ifend}
  {$else}
    FreeAndNil(FThreadLock);
  {$ifend}
  inherited Destroy;
end;

//----------------------------------------------------------------------

procedure TTMLCore.AddProfile(Profile: TTMLProfile);
var
  i, n:  Integer;
  Found: Boolean;
begin
  if Assigned(Profile) then
  begin
    Found := false;
    n     := Length(FProfiles);
    for i := 0 to n - 1 do
    begin
      if FProfiles[i] = Profile then
      begin
        Found := true;
        Break;
      end;
    end;
    if not Found then
    begin
      SetLength(FProfiles, n + 1);
      FProfiles[n] := Profile;

      for i := 0 to n - 1 do
      begin
        if FProfiles[i].Profile = Profile.Profile then
        begin
          Found := true;
          Break;
        end;
      end;
      if not Found then RegisterProfile(Profile);
    end;
  end;
end;

procedure TTMLCore.RemoveProfile(Profile: TTMLProfile);
var
  i, j, n: Integer;
  Found:   Boolean;
begin
  if Assigned(Profile) then
  begin
    n := Length(FProfiles);
    for i := 0 to n - 1 do
    begin
      if FProfiles[i] = Profile then
      begin
        for j := i to n - 2 do
        begin
          FProfiles[i] := FProfiles[i + 1];
        end;
        SetLength(FProfiles, n - 1);

        Found := false;
        for j := 0 to n - 2 do
        begin
          if FProfiles[j].Profile = Profile.Profile then
          begin
            Found := true;
            Break;
          end;
        end;
        if not Found then UnregisterProfile(Profile);

        Break;
      end;
    end;
  end;
end;

function TTMLCore.FindProfile(Profile: TTMLProfile): Integer;
var
  i, n: Integer;
begin
  Result := -1;
  if Assigned(Profile) then
  begin
    n := Length(FProfiles);
    for i := 0 to n - 1 do
    begin
      if FProfiles[i] = Profile then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TTMLCore.RegisterProfile(Profile: TTMLProfile);
var
  err: TML_INT32;
begin
  err := tml_Profile_Register(FTMLCoreHandle, PSIDEX_STRING(Profile.Profile));
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Profile_Register failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';

  // OnCustomDispatch
  err := tml_Profile_Set_OnCustomDispatch(FTMLCoreHandle,
                                          PSIDEX_STRING(Profile.Profile),
                                          @OnCustomDispatchClb,
                                          TML_POINTER(Profile));
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Profile_Set_OnCustomDispatch failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';

  // OnQueueOverflow
  err := tml_Evt_Set_OnQueueOverflow(FTMLCoreHandle,
                                     PSIDEX_STRING(Profile.Profile),
                                     @OnQueueOverflowClb,
                                     TML_POINTER(Self));
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Evt_Set_OnQueueOverflow failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';

  // OnEventError
  if Assigned(tml_Evt_Set_OnError) then
  begin
    err := tml_Evt_Set_OnError(FTMLCoreHandle,
                               PSIDEX_STRING(Profile.Profile),
                               @OnEventErrorClb,
                               TML_POINTER(Self));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnError failed (%d) : %s',
                             [err, Profile.Profile]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLCore.UnregisterProfile(Profile: TTMLProfile);
var
  err: TML_INT32;
begin
  FLastMessage := '';

  // OnCustomDispatch
  err := tml_Profile_Set_OnCustomDispatch(FTMLCoreHandle,
                                          PSIDEX_STRING(Profile.Profile), nil, nil);
  FLastError := err;
  if not (err in [TML_SUCCESS, TML_ERR_DISPATCHER_NOT_CREATED]) then
  begin
    FLastMessage := Format('tml_Profile_Set_OnCustomDispatch failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end;

  // OnQueueOverflow
  err := tml_Evt_Set_OnQueueOverflow(FTMLCoreHandle,
                                     PSIDEX_STRING(Profile.Profile), nil, nil);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Evt_Set_OnQueueOverflow failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end;

  // OnEventError
  if Assigned(tml_Evt_Set_OnError) then
  begin
    err := tml_Evt_Set_OnError(FTMLCoreHandle,
                               PSIDEX_STRING(Profile.Profile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnError failed (%d) : %s',
                             [err, Profile.Profile]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;

  err := tml_Profile_Unregister(FTMLCoreHandle, PSIDEX_STRING(Profile.Profile));
  FLastError := err;
  if not (err in [TML_SUCCESS, TML_ERR_DISPATCHER_NOT_CREATED]) then
  begin
    FLastMessage := Format('tml_Profile_Unregister failed (%d) : %s',
                           [err, Profile.Profile]);
    raise ETMLError.Create(FLastMessage);
  end;
end;

//----------------------------------------------------------------------

function TTMLCore.CallAsync(profile, host, port: string;
                            cmd: TTMLCmdMsg; atimeout: Cardinal): TML_INT32;
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBReady    : TTML_OnCmdReady;
  pCBData     : TML_POINTER;
begin
  Result := TML_ERR_COMMON;
  FLastMessage := '';
  if Assigned(cmd) then
  begin
    if FTMLCoreHandle <> TML_CORE_HANDLE_NULL then
    begin
      cmd.TMLCore     := Self;
      cmd.ProfileName := profile;
      cmd.UID         := CreateUID;
      cmd.FIsAsync    := true;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        Result := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            Result := tml_Cmd_Register_Progress(cmd.Handle, @OnCoreProgressClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (CallAsync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (CallAsync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        Result := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            Result := tml_Cmd_Register_StatusReply(cmd.Handle, @OnCoreStatusReplyClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (CallAsync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (CallAsync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // command ready
      // do NOT test for Assigned(FOnCommandReady) here!
      pCBReady := nil;
      Result := tml_Cmd_Registered_CommandReady(cmd.Handle, pCBReady, pCBData);
      FLastError := Result;
      if Result = TML_SUCCESS then
      begin
        // do not register, if already done...
        if not Assigned(pCBReady) then
        begin
          Result := tml_Cmd_Register_CommandReady(cmd.Handle, @OnCoreCommandReadyClb, TML_POINTER(cmd));
          FLastError := Result;
          if Result <> TML_SUCCESS then
          begin
            FreeAndNil(cmd);
            FLastMessage := Format('tml_Cmd_Register_CommandReady failed (%d) (CallAsync)', [Result]);
            raise ETMLError.Create(FLastMessage);
          end;
        end;
      end
      else
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Cmd_Registered_CommandReady failed (%d) (CallAsync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end;

      cmd.ReleaseData; // release data for access from other threads
      Result := tml_Send_AsyncMessage(FTMLCoreHandle, cmd.Handle,
                                      PSIDEX_STRING(profile),
                                      PSIDEX_STRING(host),
                                      PSIDEX_STRING(Port), atimeout);
      FLastError := Result;
      if Result <> TML_SUCCESS then
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Core_SendAsyncMessage failed (%d) (CallAsync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end
      else
      begin
        {$if defined(AUTOREFCOUNT)}
          cmd.__ObjAddRef;
        {$ifend}
      end;
    end
    else
    begin
      FreeAndNil(cmd);
      FLastMessage := 'CallAsync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

function TTMLCore.CallSync(profile, host, port: string; cmd: TTMLCmdMsg;
                           atimeout: Cardinal): TML_INT32;
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBData     : TML_POINTER;
begin
  Result := TML_ERR_COMMON;
  FLastMessage := '';
  if Assigned(cmd) then
  begin
    if FTMLCoreHandle <> TML_CORE_HANDLE_NULL then
    begin
      cmd.TMLCore     := Self;
      cmd.ProfileName := profile;
      cmd.UID         := CreateUID;
      cmd.FIsAsync    := false;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        Result := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            Result := tml_Cmd_Register_Progress(cmd.Handle, @OnCoreProgressClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (CallSync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (CallSync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        Result := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            Result := tml_Cmd_Register_StatusReply(cmd.Handle, @OnCoreStatusReplyClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (CallSync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (CallSync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      cmd.ReleaseData; // release data for access from other threads
      Result := tml_Send_SyncMessage(FTMLCoreHandle, cmd.Handle,
                                     PSIDEX_STRING(profile),
                                     PSIDEX_STRING(host),
                                     PSIDEX_STRING(Port), atimeout);
      FLastError := Result;

      if Assigned(FOnCommandReady) then FOnCommandReady(Self, cmd);

      if Result <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Core_SendSyncMessage failed (%d) (CallSync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end;
    end
    else
    begin
      FLastMessage := 'CallSync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

function TTMLCore.GeneralDeregistration: Boolean;
begin
  FLastMessage := '';
  FLastError   := tml_Core_GeneralDeregistration(FTMLCoreHandle);
  Result       := (FLastError = TML_SUCCESS);
end;

function TTMLCore.CreateUID: Cardinal;
begin
  {$if defined(FPC)}
    {$if defined(Unix)}
      FThreadLock.Enter;
    {$else}
      EnterCriticalSection(FThreadLock);
    {$ifend}
  {$else}
    FThreadLock.Enter;
  {$ifend}

  try
    Inc(FCurrentUID);
    Result := FCurrentUID;
  finally
    {$if defined(FPC)}
      {$if defined(Unix)}
        FThreadLock.Leave;
      {$else}
        LeaveCriticalSection(FThreadLock);
      {$ifend}
    {$else}
      FThreadLock.Leave;
    {$ifend}
  end;
end;

function TTMLCore.GetWindowSize: TML_INT32;
var
  iVal, err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    iVal := 0;
    err := tml_Core_Get_WindowSize(FTMLCoreHandle, iVal);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then
    begin
      if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
      begin
        FLastMessage := Format('tml_Core_Get_WindowSize failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else iVal := 0;
    end;
    Result := iVal;
  end
  else Result := tmlDesignInfos[FDesignIndex].WindowSize;
end;

function TTMLCore.GetListenerEnabled: Boolean;
var
  isEnabled: TML_BOOL;
  err:       TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    isEnabled := TML_FALSE;
    err := tml_Core_Get_ListenerEnabled(FTMLCoreHandle, isEnabled);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then isEnabled := TML_FALSE;
    Result := (isEnabled <> TML_FALSE);
  end
  else Result := tmlDesignInfos[FDesignIndex].ListenerEnabled;
end;

function TTMLCore.GetListenerIP: string;
var
  IP:  PSIDEX_STRING;
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Get_ListenerIP(FTMLCoreHandle, IP);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then Result := ''
                          else Result := IP;
  end
  else Result := tmlDesignInfos[FDesignIndex].ListenerIP;
end;

function TTMLCore.GetListenerPort: string;
var
  port: PSIDEX_STRING;
  err:  TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Get_ListenerPort(FTMLCoreHandle, port);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then Result := ''
                          else Result := port;
  end
  else Result := tmlDesignInfos[FDesignIndex].ListenerPort;
end;

function TTMLCore.GetLogLevel: TML_INT32;
var
  iVal, err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    iVal := TML_LOG_OFF;
    err := tml_Core_Get_LoggingValue(FTMLCoreHandle, iVal);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then
    begin
      if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
      begin
        FLastMessage := Format('tml_Core_Get_Logging_Value failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else iVal := TML_LOG_OFF;
    end;
    Result := iVal;
  end
  else Result := tmlDesignInfos[FDesignIndex].LogLevel;
end;

function TTMLCore.GetMaxBalFailCount: Cardinal;
var
  lVal, err: TML_UINT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    lVal := 0;
    err := tml_Bal_Get_MaxConnectionFailCount(FTMLCoreHandle, lVal);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then
    begin
      if err <> TML_ERR_INFORMATION_UNDEFINED then // = value doesn't exist
      begin
        FLastMessage := Format('tml_Bal_Get_MaxConnectionFailCount failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else lVal := 0;
    end;
    Result := lVal;
  end
  else Result := tmlDesignInfos[FDesignIndex].MaxBalFailCount;
end;

function TTMLCore.GetMaxEvtFailCount: Cardinal;
var
  lVal: TML_UINT32;
  err:  TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    lVal := 0;
    err := tml_Evt_Get_MaxConnectionFailCount(FTMLCoreHandle, lVal);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then
    begin
      if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
      begin
        FLastMessage := Format('tml_Evt_Get_MaxConnectionFailCount failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else lVal := 0;
    end;
    Result := lVal;
  end
  else Result := tmlDesignInfos[FDesignIndex].MaxEvtFailCount;
end;

function TTMLCore.GetMaxQueuedEvents: Cardinal;
var
  lVal: TML_UINT32;
  err:  TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    lVal := 0;
    err := tml_Evt_Get_MaxQueuedEventMessages(FTMLCoreHandle, lVal);
    FLastError   := err;
    FLastMessage := '';
    if err <> TML_SUCCESS then
    begin
      if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
      begin
        FLastMessage := Format('tml_Evt_Get_MaxQueuedEventMessages failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else lVal := 0;
    end;
    Result := lVal;
  end
  else Result := tmlDesignInfos[FDesignIndex].MaxQueuedEvents;
end;

function TTMLCore.GetSidexCopyright: string;
begin
  if not (csDesigning in ComponentState) then
  begin
    Result := sidex_Copyright();
  end
  else Result := sidexDesignCopyright;
end;

function TTMLCore.GetSidexVersion: string;
begin
  if not (csDesigning in ComponentState) then
  begin
    Result := sidex_Version();
  end
  else Result := sidexDesignVersion;
end;

function TTMLCore.GetTmlCopyright: string;
begin
  if not (csDesigning in ComponentState) then
  begin
    Result := tml_Copyright();
  end
  else Result := tmlDesignCopyright;
end;

function TTMLCore.GetTmlVersion: string;
begin
  if not (csDesigning in ComponentState) then
  begin
    Result := tml_Version();
  end
  else Result := tmlDesignVersion;
end;

function TTMLCore.IsProfileRegistered(profile: string): Boolean;
var
  isRegistered: TML_BOOL;
  err:          TML_INT32;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;

  isRegistered := TML_FALSE;
  err := tml_Profile_Get_RegisterState(FTMLCoreHandle, PSIDEX_STRING(profile),
                                       isRegistered);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err = SIDEX_ERR_NOCONTENT then
    begin
      isRegistered := TML_FALSE;
    end
    else
    begin
      FLastMessage := Format('tml_Profile_Get_RegisterState failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;
  Result := (isRegistered <> TML_FALSE);
end;

procedure TTMLCore.SetWindowSize(const Value: TML_INT32);
var
  err: TML_INT32;
begin
  err := tml_Core_Set_WindowSize(FTMLCoreHandle, Value);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Core_Set_WindowSize failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
end;

class procedure TTMLCore.SetPassword(User, Pass: string);
var
  err: TML_INT32;
begin
  err := tml_Core_Set_Password(PSIDEX_STRING(User), PSIDEX_STRING(Pass));
  if err <> TML_SUCCESS then
  begin
    raise ETMLError.CreateFmt('tml_Core_Set_Password failed (%d)', [err]);
  end;
end;

procedure TTMLCore.SetListenerEnabled(const Value: Boolean);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Value then
      err := tml_Core_Set_ListenerEnabled(FTMLCoreHandle, TML_TRUE)
    else
      err := tml_Core_Set_ListenerEnabled(FTMLCoreHandle, TML_FALSE);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('TML_Core_Enable_Listener failed (%d)', [err]);
      if tmlUseExceptions then raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].ListenerEnabled := Value;
end;

procedure TTMLCore.SetListenerIP(const Value: string);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Set_ListenerIP(FTMLCoreHandle, PSIDEX_STRING(Value));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Core_Set_ListenerIP failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].ListenerIP := Value;
end;

procedure TTMLCore.SetListenerPort(const Value: string);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Set_ListenerPort(FTMLCoreHandle, PSIDEX_STRING(Value));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Core_Set_ListenerPort failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].ListenerPort := Value;
end;

procedure TTMLCore.SetLogLevel(const Value: TML_INT32);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Core_Set_LoggingValue(FTMLCoreHandle, Value);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Core_Set_Logging_Value failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].LogLevel := Value;
end;

procedure TTMLCore.SetMaxBalFailCount(const Value: Cardinal);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Bal_Set_MaxConnectionFailCount(FTMLCoreHandle, Value);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('Ttml_Bal_Set_MaxConnectionFailCount failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].MaxBalFailCount := Value;
end;

procedure TTMLCore.SetMaxEvtFailCount(const Value: Cardinal);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Evt_Set_MaxConnectionFailCount(FTMLCoreHandle, Value);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_MaxConnectionFailCount failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].MaxEvtFailCount := Value;
end;

procedure TTMLCore.SetMaxQueuedEvents(const Value: Cardinal);
var
  err: TML_INT32;
begin
  if not (csDesigning in ComponentState) then
  begin
    err := tml_Evt_Set_MaxQueuedEventMessages(FTMLCoreHandle, Value);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_MaxQueuedEventMessages failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else tmlDesignInfos[FDesignIndex].MaxQueuedEvents := Value;
end;

function TTMLCore.GetEventDestinations: variant;
var
  err: TML_INT32;
  subscriptions: SIDEX_VARIANT;
begin
  subscriptions := SIDEX_VARIANT_NULL;
  err := tml_Evt_Get_Subscribed_MessageDestinations(FTMLCoreHandle,
                                                    nil,
                                                    subscriptions);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Evt_Get_Subscribed_MessageDestinations failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
  Result := SidexVariantAsVariant(subscriptions);
  sidex_Variant_DecRef(subscriptions);
end;

function TTMLCore.GetBalDestinations: variant;
var
  err: TML_INT32;
  subscriptions: SIDEX_VARIANT;
begin
  subscriptions := SIDEX_VARIANT_NULL;
  err := tml_Evt_Get_Subscribed_MessageDestinations(FTMLCoreHandle,
                                                    nil,
                                                    subscriptions);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Evt_Get_Subscribed_MessageDestinations failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
  Result := SidexVariantAsVariant(subscriptions);
  sidex_Variant_DecRef(subscriptions);
end;

//#---------------------------------------------------------------------
{ TTMLProfile }
//#---------------------------------------------------------------------

procedure TTMLProfile.AddBalDestination(aHost, aPort: string);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Bal_Subscribe_MessageDestination(FTMLCore.TMLCoreHandle,
                                                PSIDEX_STRING(FProfile),
                                                PSIDEX_STRING(aHost),
                                                PSIDEX_STRING(aPort));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Subscribe_MessageDestination failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.AddEventDestination(aHost, aPort: string);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Evt_Subscribe_MessageDestination(FTMLCore.TMLCoreHandle,
                                                PSIDEX_STRING(FProfile),
                                                PSIDEX_STRING(aHost),
                                                PSIDEX_STRING(aPort));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Subscribe_MessageDestination failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
  else
  begin
    FLastMessage := 'TML Core not assigned';
    raise ETMLError.Create(FLastMessage);
  end;
end;

constructor TTMLProfile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLastError               := TML_SUCCESS;
  FLastMessage             := '';
  FTMLCore                 := nil;
  FOnGetProfileName        := nil;
  FOnPopulateEventReceiver := nil;
  FOnPopulateBalReceiver   := nil;
  FOnCustomDispatch        := nil;
  FOnQueueOverflow         := nil;
  FOnProgress              := nil;
  FOnCommandReady          := nil;
  FOnStatusReply           := nil;
  FOnEventError            := nil;
  FOnPeerRegister_Evt      := nil;
  FOnPeerRegister_Bal      := nil;
  FOnBusyStatusRequest     := nil;
  FOnCalculation           := nil;
  FSyncReply               := False;

  FProfile  := 'urn:yourcompany.com:' + Name;
  FCommands := TTMLCommands.Create(Self);
end;

destructor TTMLProfile.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FTMLCore) then
    begin
      RemoveAllEventDestinations;
      RemoveAllBalDestinations;
    end;
  end;
  Registered := False;
  FreeAndNil(FCommands);
  inherited Destroy;
end;

procedure TTMLProfile.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
var
  i: Integer;
begin
  for i := 0 to FCommands.Count - 1 do
  begin
   if FCommands[i].Owner = ARoot then AProc(FCommands[i]);
  end;
end;

procedure TTMLProfile.DeleteCommand(cmd: TTMLCommand);
var
  i: Integer;
begin
  i := FCommands.FList.IndexOf(cmd);
  if i >= 0 then FCommands.Delete(i);
end;

procedure TTMLProfile.AddCommand(cmd: TTMLCommand);
begin
  FCommands.Add(cmd);
end;

function TTMLProfile.GetCommands: TTMLCommands;
begin
  Result := FCommands;
end;

function TTMLProfile.GetCore: TTMLCore;
begin
  Result := FTMLCore;
end;

function TTMLProfile.GetProfile: string;
begin
  Result := FProfile;
end;

function TTMLProfile.GetRegistered: Boolean;
begin
  if csDesigning in ComponentState then Result := false
  else if not Assigned(FTMLCore)   then Result := False
  else Result := FTMLCore.IsProfileRegistered(FProfile);
end;

procedure TTMLProfile.Loaded;
begin
  inherited Loaded;
  Registered := True;
end;

procedure TTMLProfile.PopulateBalReceiver;
begin
  if Assigned(FOnPopulateBalReceiver) then FOnPopulateBalReceiver(Self);
end;

procedure TTMLProfile.PopulateEventReceiver;
begin
  if Assigned(FOnPopulateEventReceiver) then FOnPopulateEventReceiver(Self);
end;

procedure TTMLProfile.RegisterBalancerPeer(ahost, aport: string;
                                           atimeout: Cardinal);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    if FTMLCore.ListenerEnabled then
    begin
      err := tml_Bal_Send_SubscriptionRequest(FTMLCore.TMLCoreHandle,
                                              PSIDEX_STRING(FProfile),
                                              PSIDEX_STRING(FTMLCore.ListenerIP),
                                              PSIDEX_STRING(FTMLCore.ListenerPort),
                                              PSIDEX_STRING(ahost),
                                              PSIDEX_STRING(aport),
                                              atimeout);
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Bal_Send_SubscriptionRequest failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else FLastMessage := '';
    end
    else
    begin
      FLastMessage := 'Listener must be enabled to subscribe to balancer';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.UnregisterBalancerPeer(ahost, aport: string;
                                             atimeout: Cardinal);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    if FTMLCore.ListenerEnabled then
    begin
      err := tml_Bal_Send_UnsubscriptionRequest(FTMLCore.TMLCoreHandle,
                                                PSIDEX_STRING(FProfile),
                                                PSIDEX_STRING(FTMLCore.ListenerIP),
                                                PSIDEX_STRING(FTMLCore.ListenerPort),
                                                PSIDEX_STRING(ahost),
                                                PSIDEX_STRING(aport),
                                                atimeout);
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Bal_Send_UnsubscriptionRequest failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else FLastMessage := '';
    end
    else
    begin
      FLastMessage := 'Listener must be enabled to unsubscribe from balancer';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.RegisterCallbacks;
var
  err: TML_INT32;
begin
  if csDesigning in ComponentState then Exit;
  if Assigned(FTMLCore) then
  begin
    FLastMessage := '';

    // OnPopulateEvtReceicver
    err := tml_Evt_Set_OnPopulate(FTMLCore.TMLCoreHandle,
                                  PSIDEX_STRING(FProfile),
                                  @OnPopulateEventReceiverClb,
                                  TML_POINTER(Self));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnPopulate failed (%d) (RegisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPopulateBalReceicver
    err := tml_Bal_Set_OnPopulate(FTMLCore.TMLCoreHandle,
                                  PSIDEX_STRING(FProfile),
                                  @OnPopulateBalReceiverClb,
                                  TML_POINTER(Self));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnPopulate failed (%d) (RegisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPeerRegister_Evt
    err := tml_Evt_Set_OnPeerRegister(FTMLCore.TMLCoreHandle,
                                      PSIDEX_STRING(FProfile),
                                      @OnPeerRegister_EvtClb,
                                      TML_POINTER(Self));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnPeerRegister failed (%d) (RegisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPeerRegister_Bal
    err := tml_Bal_Set_OnPeerRegister(FTMLCore.TMLCoreHandle,
                                      PSIDEX_STRING(FProfile),
                                      @OnPeerRegister_BalClb,
                                      TML_POINTER(Self));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnPeerRegister failed (%d) (RegisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnBusyStatusRequest
    if Assigned(FOnBusyStatusRequest) then
    begin
      err := tml_Bal_Set_OnBusyStatusRequest(FTMLCore.TMLCoreHandle,
                                             PSIDEX_STRING(FProfile),
                                             @OnBusyStatusRequestClb,
                                             TML_POINTER(Self));
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Bal_Set_OnBusyStatusRequest failed (%d) (RegisterCallbacks)', [err]);
        raise ETMLError.Create(FLastMessage);
      end;
    end;

    // OnCalculation
    if Assigned(FOnCalculation) then
    begin
      err := tml_Bal_Set_OnCalculation(FTMLCore.TMLCoreHandle,
                                       PSIDEX_STRING(FProfile),
                                       @OnCalculationClb,
                                       TML_POINTER(Self));
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Bal_Set_OnCalculation failed (%d) (RegisterCallbacks)', [err]);
        raise ETMLError.Create(FLastMessage);
      end;
    end;
  end;
end;

procedure TTMLProfile.RegisterCommand(cmd: TTMLCommand);
var
  err: TML_INT32;
begin
  if csDesigning in ComponentState then Exit;

  if Assigned(FTMLCore) then
  begin
    err := tml_Profile_Register_Cmd(FTMLCore.TMLCoreHandle,
                                    PSIDEX_STRING(FProfile), cmd.CommandId,
                                    @ExecuteCommandClb, TML_POINTER(cmd));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Profile_Register_Cmd failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end
end;

procedure TTMLProfile.RegisterCommands;
var
  index : Integer;
  cmd   : TTMLCommand;
begin
  if csDesigning in ComponentState then Exit;
  for index := 0 to FCommands.Count - 1 do
  begin
    cmd := FCommands.Items[index];
    if Assigned(cmd) then RegisterCommand(cmd);
  end;
end;

procedure TTMLProfile.RegisterEventPeer(ahost, aport: string;
                                        atimeout: Cardinal);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    if FTMLCore.ListenerEnabled then
    begin
      err := tml_Evt_Send_SubscriptionRequest(FTMLCore.TMLCoreHandle,
                                              PSIDEX_STRING(FProfile),
                                              PSIDEX_STRING(FTMLCore.ListenerIP),
                                              PSIDEX_STRING(FTMLCore.ListenerPort),
                                              PSIDEX_STRING(ahost),
                                              PSIDEX_STRING(aport),
                                              atimeout);
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Evt_Send_SubscriptionRequest failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else FLastMessage := '';
    end
    else
    begin
      FLastMessage := 'Listener must be enabled to subscribe an event receiver';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.UnregisterEventPeer(ahost, aport: string;
                                          atimeout: Cardinal);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    if FTMLCore.ListenerEnabled then
    begin
      err := tml_Evt_Send_UnsubscriptionRequest(FTMLCore.TMLCoreHandle,
                                                PSIDEX_STRING(FProfile),
                                                PSIDEX_STRING(FTMLCore.ListenerIP),
                                                PSIDEX_STRING(FTMLCore.ListenerPort),
                                                PSIDEX_STRING(ahost),
                                                PSIDEX_STRING(aport),
                                                atimeout);
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Evt_Send_UnsubscriptionRequest failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else FLastMessage := '';
    end
    else
    begin
      FLastMessage := 'Listener must be enabled to unsubscribe an event receiver';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.RemoveBalDestination(aHost, aPort: string);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Bal_Unsubscribe_MessageDestination(FTMLCore.TMLCoreHandle,
                                                  PSIDEX_STRING(FProfile),
                                                  PSIDEX_STRING(aHost),
                                                  PSIDEX_STRING(aPort));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Unsubscribe_MessageDestination failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.RemoveAllBalDestinations;
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Bal_Unsubscribe_All_MessageDestinations(FTMLCore.TMLCoreHandle,
                                                       PSIDEX_STRING(FProfile));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Unsubscribe_All_MessageDestinations failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

function TTMLProfile.GetBalDestinations: variant;
var
  err: TML_INT32;
  subscriptions: SIDEX_VARIANT;
begin
  if Assigned(FTMLCore) then
  begin
    subscriptions := SIDEX_VARIANT_NULL;
    err := tml_Bal_Get_Subscribed_MessageDestinations(FTMLCore.TMLCoreHandle,
                                                      PSIDEX_STRING(FProfile),
                                                      subscriptions);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Get_Subscribed_MessageDestinations failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
    Result := SidexVariantAsVariant(subscriptions);
    sidex_Variant_DecRef(subscriptions);
  end
  else
  begin
    FLastMessage := 'TML Core not assigned';
    raise ETMLError.Create(FLastMessage);
  end;
end;

procedure TTMLProfile.RemoveEventDestination(aHost, aPort: string);
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Evt_Unsubscribe_MessageDestination(FTMLCore.TMLCoreHandle,
                                                  PSIDEX_STRING(FProfile),
                                                  PSIDEX_STRING(aHost),
                                                  PSIDEX_STRING(aPort));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Unsubscribe_MessageDestination failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.RemoveAllEventDestinations;
var
  err: TML_INT32;
begin
  if Assigned(FTMLCore) then
  begin
    err := tml_Evt_Unsubscribe_All_MessageDestinations(FTMLCore.TMLCoreHandle,
                                                       PSIDEX_STRING(FProfile));
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Unsubscribe_All_MessageDestinations failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

function TTMLProfile.GetEventDestinations: variant;
var
  err: TML_INT32;
  subscriptions: SIDEX_VARIANT;
begin
  if Assigned(FTMLCore) then
  begin
    subscriptions := SIDEX_VARIANT_NULL;
    err := tml_Evt_Get_Subscribed_MessageDestinations(FTMLCore.TMLCoreHandle,
                                                      PSIDEX_STRING(FProfile),
                                                      subscriptions);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Get_Subscribed_MessageDestinations failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
    Result := SidexVariantAsVariant(subscriptions);
    sidex_Variant_DecRef(subscriptions);
  end
  else
  begin
    FLastMessage := 'TML Core not assigned';
    raise ETMLError.Create(FLastMessage);
  end;
end;

procedure TTMLProfile.CallBalAsync(cmd: TTMLCmdMsg; atimeout: Cardinal);
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBReady    : TTML_OnCmdReady;
  pCBData     : TML_POINTER;
  err         : TML_INT32;
begin
  if Assigned(cmd) then
  begin
    if Assigned(FTMLCore) then
    begin
      FLastMessage := '';

      cmd.TMLCore  := FTMLCore;
      cmd.Profile  := Self;
      cmd.UID      := FTMLCore.CreateUID;
      cmd.FIsAsync := true;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        err := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := err;
        if err = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            err := tml_Cmd_Register_Progress(cmd.Handle, @OnProfileProgressClb, TML_POINTER(cmd));
            FLastError := err;
            if err <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (CallBalAsync)', [err]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (CallBalAsync)', [err]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        err := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := err;
        if err = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            err := tml_Cmd_Register_StatusReply(cmd.Handle, @OnProfileStatusReplyClb, TML_POINTER(cmd));
            FLastError := err;
            if err <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (CallBalAsync)', [err]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (CallBalAsync)', [err]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // command ready
      // do NOT test for Assigned(FOnCommandReady) here!
      pCBReady := nil;
      err := tml_Cmd_Registered_CommandReady(cmd.Handle, pCBReady, pCBData);
      FLastError := err;
      if err = TML_SUCCESS then
      begin
        // do not register, if already done...
        if not Assigned(pCBReady) then
        begin
          err := tml_Cmd_Register_CommandReady(cmd.Handle, @OnProfileCommandReadyClb, TML_POINTER(cmd));
          FLastError := err;
          if err <> TML_SUCCESS then
          begin
            FreeAndNil(cmd);
            FLastMessage := Format('tml_Cmd_Register_CommandReady failed (%d) (CallBalAsync)', [err]);
            raise ETMLError.Create(FLastMessage);
          end;
        end;
      end
      else
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Cmd_Registered_CommandReady failed (%d) (CallBalAsync)', [err]);
        raise ETMLError.Create(FLastMessage);
      end;

      cmd.ReleaseData; // release data for access from other threads
      err := tml_Bal_Send_AsyncMessage(FTMLCore.TMLCoreHandle, cmd.Handle,
                                       PSIDEX_STRING(FProfile), atimeout);
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Bal_Send_AsyncMessage failed (%d) (CallBalAsync)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else
      begin
        {$if defined(AUTOREFCOUNT)}
          cmd.__ObjAddRef;
        {$ifend}
      end;
    end
    else
    begin
      FreeAndNil(cmd);
      FLastMessage := 'CallBalAsync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.CallBalSync(cmd: TTMLCmdMsg; atimeout: Cardinal);
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBData     : TML_POINTER;
  err         : TML_INT32;
begin
  if Assigned(cmd) then
  begin
    if Assigned(TMLCore) then
    begin
      FLastMessage := '';

      cmd.TMLCore  := FTMLCore;
      cmd.Profile  := Self;
      cmd.UID      := FTMLCore.CreateUID;
      cmd.FIsAsync := false;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        err := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := err;
        if err = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            err := tml_Cmd_Register_Progress(cmd.Handle, @OnProfileProgressClb, TML_POINTER(cmd));
            FLastError := err;
            if err <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (CallBalSync)', [err]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (CallBalSync)', [err]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        err := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := err;
        if err = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            err := tml_Cmd_Register_StatusReply(cmd.Handle, @OnProfileStatusReplyClb, TML_POINTER(cmd));
            FLastError := err;
            if err <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (CallBalSync)', [err]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (CallBalSync)', [err]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      cmd.ReleaseData; // release data for access from other threads
      err := tml_Bal_Send_SyncMessage(FTMLCore.TMLCoreHandle, cmd.Handle,
                                      PSIDEX_STRING(FProfile), atimeout);
      FLastError := err;

      if Assigned(OnCommandReady) then OnCommandReady(Self, cmd);

      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Bal_Send_SyncMessage failed (%d) (CallBalSync)', [err]);
        raise ETMLError.Create(FLastMessage);
      end;
    end
    else
    begin
      FLastMessage := 'CallBalSync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.SendEvent(cmd: TTMLCmdMsg);
var
  err: TML_INT32;
begin
  if Assigned(cmd) then
  begin
    if Assigned(FTMLCore) then
    begin
      cmd.TMLCore := FTMLCore;
      cmd.Profile := Self;
      cmd.ReleaseData; // release data for access from other threads
      err := tml_Evt_Send_Message(FTMLCore.TMLCoreHandle,
                                  cmd.FHandle, PSIDEX_STRING(FProfile));
      FLastError := err;
      if err <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Evt_Send_Message failed (%d)', [err]);
        raise ETMLError.Create(FLastMessage);
      end
      else FLastMessage := '';
    end
    else
    begin
      FLastMessage := 'TML Core not assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

function TTMLProfile.SendCommandSync(host, port : string; cmd: TTMLCmdMsg;
                                     atimeout: Cardinal): TML_INT32;
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBData     : TML_POINTER;
begin
  Result := TML_ERR_COMMON;
  if Assigned(cmd) then
  begin
    if Assigned(FTMLCore) then
    begin
      FLastMessage := '';

      cmd.TMLCore  := FTMLCore;
      cmd.Profile  := Self;
      cmd.UID      := FTMLCore.CreateUID;
      cmd.FIsAsync := false;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        Result := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            Result := tml_Cmd_Register_Progress(cmd.Handle, @OnProfileProgressClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (SendCommandSync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (SendCommandSync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        Result := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            Result := tml_Cmd_Register_StatusReply(cmd.Handle, @OnProfileStatusReplyClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (SendCommandSync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (SendCommandSync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      cmd.ReleaseData; // release data for access from other threads
      Result := tml_Send_SyncMessage(FTMLCore.TMLCoreHandle, cmd.Handle,
                                     PSIDEX_STRING(profile), PSIDEX_STRING(host),
                                     PSIDEX_STRING(Port), atimeout);
      FLastError := Result;

      if Assigned(OnCommandReady) then OnCommandReady(Self, cmd);

      if Result <> TML_SUCCESS then
      begin
        FLastMessage := Format('tml_Core_SendSyncMessage failed (%d) (SendCommandSync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end;
    end
    else
    begin
      FLastMessage := 'SendCommandSync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

function TTMLProfile.SendCommandAsync(host, port : string; cmd: TTMLCmdMsg;
                                      atimeout: Cardinal): TML_INT32;
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBReady    : TTML_OnCmdReady;
  pCBData     : TML_POINTER;
begin
  Result := TML_ERR_COMMON;
  if Assigned(cmd) then
  begin
    if Assigned(FTMLCore) then
    begin
      FLastMessage := '';

      cmd.TMLCore  := FTMLCore;
      cmd.Profile  := Self;
      cmd.UID      := FTMLCore.CreateUID;
      cmd.FIsAsync := true;

      // register callbacks for this command...
      cmd.RegisterCallbacks;

      // progress
      if Assigned(FOnProgress) and cmd.ShowProgress then
      begin
        pCBProgress := nil;
        Result := tml_Cmd_Registered_Progress(cmd.Handle, pCBProgress, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBProgress) then
          begin
            cmd.SetProgress(0);
            Result := tml_Cmd_Register_Progress(cmd.Handle, @OnProfileProgressClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (SendCommandAsync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (SendCommandAsync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // status reply
      if Assigned(FOnStatusReply) then
      begin
        pCBStatus := nil;
        Result := tml_Cmd_Registered_StatusReply(cmd.Handle, pCBStatus, pCBData);
        FLastError := Result;
        if Result = TML_SUCCESS then
        begin
          // do not register, if already done...
          if not Assigned(pCBStatus) then
          begin
            Result := tml_Cmd_Register_StatusReply(cmd.Handle, @OnProfileStatusReplyClb, TML_POINTER(cmd));
            FLastError := Result;
            if Result <> TML_SUCCESS then
            begin
              FreeAndNil(cmd);
              FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (SendCommandAsync)', [Result]);
              raise ETMLError.Create(FLastMessage);
            end;
          end;
        end
        else
        begin
          FreeAndNil(cmd);
          FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (SendCommandAsync)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;

      // command ready
      // do NOT test for Assigned(FOnCommandReady) here!
      pCBReady := nil;
      Result := tml_Cmd_Registered_CommandReady(cmd.Handle, pCBReady, pCBData);
      FLastError := Result;
      if Result = TML_SUCCESS then
      begin
        // do not register, if already done...
        if not Assigned(pCBReady) then
        begin
          Result := tml_Cmd_Register_CommandReady(cmd.Handle, @OnProfileCommandReadyClb, TML_POINTER(cmd));
          FLastError := Result;
          if Result <> TML_SUCCESS then
          begin
            FreeAndNil(cmd);
            FLastMessage := Format('tml_Cmd_Register_CommandReady failed (%d) (SendCommandAsync)', [Result]);
            raise ETMLError.Create(FLastMessage);
          end;
        end;
      end
      else
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Cmd_Registered_CommandReady failed (%d) (SendCommandAsync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end;

      cmd.ReleaseData; // release data for access from other threads
      Result := tml_Send_AsyncMessage(FTMLCore.TMLCoreHandle, cmd.Handle,
                                      PSIDEX_STRING(profile), PSIDEX_STRING(host),
                                      PSIDEX_STRING(Port), atimeout);
      FLastError := Result;
      if Result <> TML_SUCCESS then
      begin
        FreeAndNil(cmd);
        FLastMessage := Format('tml_Core_SendAsyncMessage failed (%d) (SendCommandAsync)', [Result]);
        raise ETMLError.Create(FLastMessage);
      end
      else
      begin
        {$if defined(AUTOREFCOUNT)}
          cmd.__ObjAddRef;
        {$ifend}
      end;
    end
    else
    begin
      FreeAndNil(cmd);
      FLastMessage := 'SendCommandAsync failed - no TML Core assigned';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.SetCore(const Value: TTMLCore);
begin
  Registered := False;
  FTMLCore   := Value;
  Registered := True;
end;

procedure TTMLProfile.SetProfile(const Value: string);
begin
  Registered := False;
  FProfile   := Value;
  Registered := True;
end;

procedure TTMLProfile.SetRegistered(const Value: Boolean);
begin
  if (csDesigning in ComponentState) or
     (csLoading   in ComponentState) then Exit;

  if Assigned(FTMLCore) and (Registered <> Value) then
  begin
    if Value then
    begin
      if Assigned(FOnGetProfileName) then
      begin
        FProfile := FOnGetProfileName(Self, FProfile);
      end;

      FTMLCore.AddProfile(Self);
      FLastError   := FTMLCore.LastError;
      FLastMessage := FTMLCore.LastMessage;

      RegisterCallbacks;
      RegisterCommands;
    end
    else
    begin
      UnregisterCommands;
      UnregisterCallbacks;

      FTMLCore.RemoveProfile(Self);
      FLastError   := FTMLCore.LastError;
      FLastMessage := FTMLCore.LastMessage;
    end;
  end;
end;

procedure TTMLProfile.SetOnBusyStatusRequest(const val: TTMLOnBusyStatusRequest);
var
  err: TML_INT32;
begin
  FOnBusyStatusRequest := val;
  if csDesigning in ComponentState then Exit;
  if Assigned(FTMLCore) and Registered then
  begin
    if Assigned(FOnBusyStatusRequest) then
    begin
      err := tml_Bal_Set_OnBusyStatusRequest(FTMLCore.TMLCoreHandle,
                                             PSIDEX_STRING(FProfile),
                                             @OnBusyStatusRequestClb,
                                             TML_POINTER(Self));
    end
    else
    begin
      err := tml_Bal_Set_OnBusyStatusRequest(FTMLCore.TMLCoreHandle,
                                             PSIDEX_STRING(FProfile), nil, nil);
    end;
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnBusyStatusRequest failed (%d) (SetOnBusyStatusRequest)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.SetOnCalculation(const val: TTMLOnCalculation);
var
  err: TML_INT32;
begin
  FOnCalculation := val;
  if csDesigning in ComponentState then Exit;
  if Assigned(FTMLCore) and Registered then
  begin
    if Assigned(FOnCalculation) then
    begin
      err := tml_Bal_Set_OnCalculation(FTMLCore.TMLCoreHandle,
                                       PSIDEX_STRING(FProfile),
                                       @OnCalculationClb,
                                       TML_POINTER(Self));
    end
    else
    begin
      err := tml_Bal_Set_OnCalculation(FTMLCore.TMLCoreHandle,
                                       PSIDEX_STRING(FProfile), nil, nil);
    end;
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnCalculation failed (%d) (SetOnCalculation)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.UnregisterCallbacks;
var
  err: TML_INT32;
begin
  if csDesigning in ComponentState then Exit;

  if Assigned(FTMLCore) then
  begin
    FLastMessage := '';

    // OnPopulateEvtReceicver
    err := tml_Evt_Set_OnPopulate(FTMLCore.TMLCoreHandle,
                                  PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnPopulate failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPopulateBalReceicver
    err := tml_Bal_Set_OnPopulate(FTMLCore.TMLCoreHandle,
                                  PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnPopulate failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPeerRegister_Evt
    err := tml_Evt_Set_OnPeerRegister(FTMLCore.TMLCoreHandle,
                                      PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Evt_Set_OnPeerRegister failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnPeerRegister_Bal
    err := tml_Bal_Set_OnPeerRegister(FTMLCore.TMLCoreHandle,
                                      PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnPeerRegister failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnBusyStatusRequest
    err := tml_Bal_Set_OnBusyStatusRequest(FTMLCore.TMLCoreHandle,
                                           PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnBusyStatusRequest failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;

    // OnCalculation
    err := tml_Bal_Set_OnCalculation(FTMLCore.TMLCoreHandle,
                                     PSIDEX_STRING(FProfile), nil, nil);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Bal_Set_OnCalculation failed (%d) (UnregisterCallbacks)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

procedure TTMLProfile.UnregisterCommand(cmd: TTMLCommand);
var
  err: TML_INT32;
begin
  if csDesigning in ComponentState then Exit;

  if Assigned(FTMLCore) and Registered then
  begin
    err := tml_Profile_Register_Cmd(FTMLCore.TMLCoreHandle,
                                    PSIDEX_STRING(FProfile), cmd.CommandId,
                                    nil, nil);
    FLastError := err;
    if not (err in [TML_SUCCESS, TML_ERR_DISPATCHER_NOT_CREATED,
                    TML_ERR_DISPATCHER_CMD_NOT_REGISTERED]) then
    begin
      FLastMessage := Format('tml_Profile_Register_Cmd failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

procedure TTMLProfile.UnregisterCommands;
var
  index : Integer;
  cmd   : TTMLCommand;
begin
  if csDesigning in ComponentState then exit;
  for index := 0 to FCommands.Count - 1 do
  begin
    cmd := FCommands.Items[index];
    if Assigned(cmd) then UnregisterCommand(cmd);
  end;
end;

function TTMLProfile.CreateStream(StreamType: TTMLStreamTypeClass;
                                  ReceiverHost, ReceiverPort: string;
                                  pCBReceiverStreamClosed: TTMLStreamClosedCallback;
                                  pCBReceiverStreamError:  TTMLStreamErrorCallback): TTMLStreamTypeBase;
begin
  if Assigned(FTMLCore) then
  begin
    Result := StreamType.Create(Self, ReceiverHost, ReceiverPort,
                                pCBReceiverStreamClosed,
                                pCBReceiverStreamError);
  end
  else Result := nil;
end;

function TTMLProfile.OpenStream(StreamID:   TML_STREAM_ID;
                                StreamType: TTMLStreamTypeClass;
                                SenderHost: string;
                                SenderPort: string): TTMLStreamTypeBase;
begin
  if Assigned(FTMLCore) then
  begin
    Result := StreamType.Create(Self, StreamID, SenderHost, SenderPort);
  end
  else Result := nil;
end;

//#---------------------------------------------------------------------
{ TTMLCommands }
//#---------------------------------------------------------------------

constructor TTMLCommands.Create(AProfile : TTMLProfile);
begin
  FProfile := AProfile;
  {$if defined(FPC)}
    FList  := TFPList.Create;
  {$elseif defined(ANDROID)}
    FList  := System.Generics.Collections.TList<TTMLCommand>.Create;
  {$else}
    FList  := TList.Create;
  {$ifend}
end;

destructor TTMLCommands.Destroy;
var
  i: Integer;
begin
  if Assigned(FList) then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      Items[i].FProfile := nil;
      {$if not defined(AUTOREFCOUNT)}
      Items[i].Free;
      {$ifend}
      Items[i] := nil;
    end;
    FreeAndNil(FList);
  end;
  inherited Destroy;
end;

function TTMLCommands.Add(cmd: TTMLCommand): Integer;
begin
  cmd.Profile := FProfile;
  Result := FList.Add(cmd);
end;

function TTMLCommands.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TTMLCommands.Delete(Index: Integer);
var
  itemcmd : TTMLCommand;
begin
  itemcmd := Items[Index];
  if Assigned(FProfile) then FProfile.UnregisterCommand(itemcmd);
  FList.Delete(Index);
end;

function TTMLCommands.IndexOf(cmd: TTMLCommand): integer;
begin
  Result := FList.IndexOf(cmd);
end;

function TTMLCommands.GetItem(Index: Integer): TTMLCommand;
begin
  Result := TTMLCommand(FList.Items[Index]);
end;

procedure TTMLCommands.SetItem(Index: Integer; const Value: TTMLCommand);
begin
  Value.Profile := FProfile;
  FList.Items[Index] := Value;
  if Assigned(FProfile) then FProfile.RegisterCommand(Value);
end;

//#---------------------------------------------------------------------
{ TTMLCommand }
//#---------------------------------------------------------------------

procedure TTMLCommand.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TTMLProfile then
  begin
    (Reader.Parent as TTMLProfile).AddCommand(Self);
    //DebugLn('TAChart %s: %d series', [Reader.Parent.Name, (Reader.Parent as TChart).SeriesCount]);
  end;
end;

procedure TTMLCommand.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
  begin
    (AParent as TTMLProfile).AddCommand(Self);
  end;
end;

constructor TTMLCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnCmdCall  := nil;
  FProfile    := nil;
  FCommandId  := 0;
  FSyncCall   := false;
end;

destructor TTMLCommand.Destroy;
begin
  if Assigned(FProfile) then
  begin
    //Break link
    FProfile.DeleteCommand(Self);
    FProfile := nil;
  end;

  inherited Destroy;
end;

function TTMLCommand.GetParentComponent: TComponent;
begin
  Result := FProfile;
end;

function TTMLCommand.HasParent: Boolean;
begin
  Result := (FProfile <> nil);
end;

procedure TTMLCommand.Execute(acmd: TTMLCmdMsg);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnCmdCall) then
    begin
      if FSyncCall then
      begin
        if not Assigned(tmlSyncCallCritSect) then
        begin
          tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
        end;
        if Assigned(tmlSyncCallCritSect) then
        begin
          if Assigned(acmd) then acmd.ReleaseData;
          tmlSyncCallCritSect.Enter;
          try
            tmlSyncCallCritSect.Clear;
            tmlSyncCallCritSect.Command       := Self;
            tmlSyncCallCritSect.CmdMsg        := acmd;
            tmlSyncCallCritSect.CmdCallMethod := FOnCmdCall;
            {$if  defined(FPC)}
              TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
            {$else}
              TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
            {$ifend}
          finally
            tmlSyncCallCritSect.Leave;
          end;
        end;
      end
      else FOnCmdCall(Self, acmd);
    end;
  end;
end;

//#---------------------------------------------------------------------
{ TTMLCmdMsg }
//#---------------------------------------------------------------------

constructor TTMLCmdMsg.Create(cmdid: TML_COMMAND_ID);
var
  err: TML_INT32;
  hdl: TML_COMMAND_HANDLE;
begin
  err := tml_Cmd_Create(hdl);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Cmd_Create failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
  CreateWithHandle(hdl, true);
  SetCommandId(cmdid);
end;

constructor TTMLCmdMsg.CreateWithHandle(ahandle: TML_COMMAND_HANDLE;
                                        TakeOwnership: Boolean);
begin
  inherited Create;

  FLastError      := TML_SUCCESS;
  FLastMessage    := '';
  FShowProgress   := true;
  FIsAsync        := false;
  FTMLCore        := nil;
  FProfile        := nil;
  FProfileName    := '';
  FProgress       := 0;
  FData           := nil;
  FUID            := 0;
  FSyncReply      := false;

  FIsOwner        := TakeOwnership;
  FHandle         := ahandle;

  FOnProgress     := nil;
  FOnCommandReady := nil;
  FOnStatusReply  := nil;
end;

constructor TTMLCmdMsg.Create;
begin
  Create($9999);
end;

destructor TTMLCmdMsg.Destroy;
begin
  ReleaseData;

  if FIsOwner then tml_Cmd_Free(FHandle);
  FHandle := TML_COMMAND_HANDLE_NULL;

  inherited Destroy;
end;

function TTMLCmdMsg.AcquireData: TSIDEXDocument;
var
  hsidex : SIDEX_HANDLE;
begin
  if not Assigned(FData) then
  begin
    tml_Cmd_Acquire_Sidex_Handle(FHandle, hsidex);
    if hsidex <> SIDEX_HANDLE_NULL then
    begin
      FData := TSIDEXDocument.Create(hsidex);
    end
    else
    begin
      FLastMessage := 'cannot acquire data';
      raise ETMLError.Create(FLastMessage);
    end;
  end;
  Result := FData;
end;

procedure TTMLCmdMsg.ReleaseData;
begin
  if Assigned(FData) then
  begin
    FreeAndNil(FData);
    tml_Cmd_Release_Sidex_Handle(FHandle);
  end;
end;

function TTMLCmdMsg.GetData: TSIDEXDocument;
begin
  Result := AcquireData;
end;

function TTMLCmdMsg.GetCommandId: Cardinal;
var
  lVal: TML_UINT32;
  err:  TML_INT32;
begin
  lVal := 0;
  err  := tml_Cmd_Header_GetCommand(FHandle, lVal);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetCommand failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else lVal := 0;
  end;
  Result := lVal;
end;

function TTMLCmdMsg.GetCreationTime: string;
var
  value : PSIDEX_STRING;
  err: TML_INT32;
begin
  Result := '';
  err    := tml_Cmd_Header_GetCreationTime(FHandle, Value);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetCreationTime failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage)
    end;
  end
  else
  begin
    Result := value;
    sidex_Free_ReadString(value);
  end;
end;

function TTMLCmdMsg.GetProfileName: string;
begin
  if Assigned(FProfile) then Result := FProfile.Profile
                        else Result := FProfileName;
end;

function TTMLCmdMsg.GetError: TML_INT32;
var
  iVal, err : TML_INT32;
begin
  iVal := TML_SUCCESS;
  err  := tml_Cmd_Header_GetError(FHandle, iVal);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetError failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else iVal := TML_SUCCESS;
  end;
  Result := iVal;
end;

function TTMLCmdMsg.GetErrorMessage: string;
var
  value:        PSIDEX_STRING;
  alength, err: TML_INT32;
begin
  Result := '';
  alength := 0;
  err := tml_Cmd_Header_GetErrorMessage(FHandle, value, alength);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetErrorMessage failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;
  end
  else
  begin
    Result := value;
    sidex_Free_ReadString(value);
  end;
end;

function TTMLCmdMsg.GetReplyMessage: string;
var
  value:        PSIDEX_STRING;
  alength, err: TML_INT32;
begin
  Result  := '';
  alength := 0;
  err     := tml_Cmd_Header_GetReplyMessage(FHandle, value, alength);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetReplyMessage failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end;
  end
  else
  begin
    Result := value;
    sidex_Free_ReadString(value);
  end;
end;

function TTMLCmdMsg.GetReplyTyp: TML_INT32;
var
  iVal, err: TML_INT32;
begin
  iVal := TMLCOM_RPY_UNDEFINED;
  err := tml_Cmd_Header_GetReplyType(FHandle, iVal);
  FLastError   := err;
  FLastMessage := '';
  if err <> TML_SUCCESS then
  begin
    if err <> TML_ERR_INFORMATION_UNDEFINED then  // = value doesn't exist
    begin
      FLastMessage := Format('tml_Cmd_Header_GetReplyType failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else iVal := TMLCOM_RPY_UNDEFINED;
  end;
  Result := iVal;
end;

procedure TTMLCmdMsg.SetCommandId(const Value: Cardinal);
var
  err: TML_INT32;
begin
  err := tml_Cmd_Header_SetCommand(FHandle, Value);
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Cmd_Header_SetCommand failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
end;

procedure TTMLCmdMsg.SendStatusReply(atype: TML_INT32; amsg: string);
var
  err: TML_INT32;
begin
  ReleaseData;
  err := tml_Send_AsyncStatusReply(FHandle, atype, PSIDEX_STRING(amsg));
  FLastError   := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Send_AsyncStatusReply failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
end;

procedure TTMLCmdMsg.SetError(const Value: TML_INT32);
var
  err: TML_INT32;
begin
  err := tml_Cmd_Header_SetError(FHandle, Value);
  FLastError   := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Cmd_Header_SetError failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
end;

procedure TTMLCmdMsg.SetErrorMessage(const Value: string);
var
  err: TML_INT32;
begin
  err := tml_Cmd_Header_SetErrorMessage(FHandle, PSIDEX_STRING(Value), length(Value));
  FLastError := err;
  if err <> TML_SUCCESS then
  begin
    FLastMessage := Format('tml_Cmd_Header_SetErrorMessage failed (%d)', [err]);
    raise ETMLError.Create(FLastMessage);
  end
  else FLastMessage := '';
end;

procedure TTMLCmdMsg.SetProgress(const Value: TML_INT32);
var
  err: TML_INT32;
begin
  if Value <> FProgress then
  begin
    FProgress := Value;
    err := tml_Send_AsyncProgressReply(FHandle, Value);
    FLastError := err;
    if err <> TML_SUCCESS then
    begin
      FLastMessage := Format('tml_Core_SendAsyncProgressReply failed (%d)', [err]);
      raise ETMLError.Create(FLastMessage);
    end
    else FLastMessage := '';
  end;
end;

//----------------------------------------------------------------------

function TTMLCmdMsg.RegisterCallbacks: TML_INT32;
var
  pCBProgress : TTML_OnCmdProgress;
  pCBStatus   : TTML_OnCmdStatusRpy;
  pCBReady    : TTML_OnCmdReady;
  pCBData     : TML_POINTER;
begin
  Result       := TML_SUCCESS;
  FLastMessage := '';

  // register callbacks for this command...

  // progress
  if Assigned(FOnProgress) and ShowProgress then
  begin
    pCBProgress := nil;
    Result := tml_Cmd_Registered_Progress(Handle, pCBProgress, pCBData);
    FLastError := Result;
    if Result = TML_SUCCESS then
    begin
      // do not register, if already done...
      if not Assigned(pCBProgress) then
      begin
        SetProgress(0);
        Result := tml_Cmd_Register_Progress(Handle, @OnProgressClb, TML_POINTER(Self));
        FLastError := Result;
        if Result <> TML_SUCCESS then
        begin
          FLastMessage := Format('tml_Cmd_Register_Progress failed (%d) (CmdMsg)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;
    end
    else
    begin
      FLastMessage := Format('tml_Cmd_Registered_Progress failed (%d) (CmdMsg)', [Result]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;

  // status reply
  if Assigned(FOnStatusReply) then
  begin
    pCBStatus := nil;
    Result := tml_Cmd_Registered_StatusReply(Handle, pCBStatus, pCBData);
    FLastError := Result;
    if Result = TML_SUCCESS then
    begin
      // do not register, if already done...
      if not Assigned(pCBStatus) then
      begin
        Result := tml_Cmd_Register_StatusReply(Handle, @OnStatusReplyClb, TML_POINTER(Self));
        FLastError := Result;
        if Result <> TML_SUCCESS then
        begin
          FLastMessage := Format('tml_Cmd_Register_StatusReply failed (%d) (CmdMsg)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;
    end
    else
    begin
      FLastMessage := Format('tml_Cmd_Registered_StatusReply failed (%d) (CmdMsg)', [Result]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;

  // command ready
  if Assigned(FOnCommandReady) then
  begin
    pCBReady := nil;
    Result := tml_Cmd_Registered_CommandReady(Handle, pCBReady, pCBData);
    FLastError := Result;
    if Result = TML_SUCCESS then
    begin
      // do not register, if already done...
      if not Assigned(pCBReady) then
      begin
        Result := tml_Cmd_Register_CommandReady(Handle, @OnCommandReadyClb, TML_POINTER(Self));
        FLastError := Result;
        if Result <> TML_SUCCESS then
        begin
          FLastMessage := Format('tml_Cmd_Register_CommandReady failed (%d) (CmdMsg)', [Result]);
          raise ETMLError.Create(FLastMessage);
        end;
      end;
    end
    else
    begin
      FLastMessage := Format('tml_Cmd_Registered_CommandReady failed (%d) (CmdMsg)', [Result]);
      raise ETMLError.Create(FLastMessage);
    end;
  end;
end;

//#---------------------------------------------------------------------
{ TTMLStreamTypeBase }
//#---------------------------------------------------------------------

constructor TTMLStreamTypeBase.Create;
begin
  inherited Create;
  InitStream;
end;

procedure TTMLStreamTypeBase.InitStream;
begin
  FTMLProfile            := nil;
  FTMLCore               := nil;
  FStreamID              := TML_STREAM_ID_NULL;
  FIsSender              := false;
  FIsReceiver            := false;
  FStreamSize            := 0;
  FStreamPos_Local       := 0;
  FStreamPos_Remote      := 0;
  FLastPercent           := -1;
  FLastError             := TML_SUCCESS;
  FDownloadCallback      := nil;
  FSynchronizeCall       := false;
  {$if defined(FPC)}
    {$if defined(Unix)}
      FStreamLock := TCriticalSection.Create;
    {$else}
      InitializeCriticalSection(FStreamLock);
    {$ifend}
  {$else}
    FStreamLock := TCriticalSection.Create;
  {$ifend}
end;

procedure TTMLStreamTypeBase.PostInitStream;
begin
  // nothing to do here
end;

constructor TTMLStreamTypeBase.Create(ATMLProfile: TTMLProfile;
                                      ReceiverHost, ReceiverPort: string;
                                      pCBReceiverStreamClosed: TTMLStreamClosedCallback;
                                      pCBReceiverStreamError:  TTMLStreamErrorCallback);
var
  err: TML_INT32;
begin
  Create;
  FTMLProfile     := ATMLProfile;
  FReceiverHost   := ReceiverHost;
  FReceiverPort   := ReceiverPort;
  FClosedCallback := pCBReceiverStreamClosed;
  FErrorCallback  := pCBReceiverStreamError;
  if Assigned(FTMLProfile) and Assigned(FTMLProfile.TMLCore) then
  begin
    FTMLCore := FTMLProfile.TMLCore;
    err := tml_SndStream_Open(FTMLCore.TMLCoreHandle, FStreamID,
                              PSIDEX_STRING(FTMLProfile.Profile),
                              PSIDEX_STRING(ReceiverHost), PSIDEX_STRING(ReceiverPort));
    if (err = TML_SUCCESS) and (FStreamID <> TML_STREAM_ID_NULL) then
    begin
      err := tml_SndStream_Register_GetPosition(FTMLCore.TMLCoreHandle,
                                                FStreamID, @OnStreamGetPosition,
                                                TML_POINTER(Self));
      if err = TML_SUCCESS then
      begin
        err := tml_SndStream_Register_GetSize(FTMLCore.TMLCoreHandle,
                                              FStreamID, @OnStreamGetSize,
                                              TML_POINTER(Self));
      end;
      if err = TML_SUCCESS then
      begin
        err := tml_SndStream_Register_Read(FTMLCore.TMLCoreHandle,
                                           FStreamID, @OnStreamRead,
                                           TML_POINTER(Self));
      end;
      if err = TML_SUCCESS then
      begin
        err := tml_SndStream_Register_Write(FTMLCore.TMLCoreHandle,
                                            FStreamID, @OnStreamWrite,
                                            TML_POINTER(Self));
      end;
      if err = TML_SUCCESS then
      begin
        err := tml_SndStream_Register_Seek(FTMLCore.TMLCoreHandle,
                                           FStreamID, @OnStreamSeek,
                                           TML_POINTER(Self));
      end;
      if err = TML_SUCCESS then
      begin
        err := tml_SndStream_Register_Close(FTMLCore.TMLCoreHandle,
                                            FStreamID, @OnReceiverStreamClosed,
                                            TML_POINTER(Self));
      end;
      if (err = TML_SUCCESS) and Assigned(tml_SndStream_Register_OnError) then
      begin
        err := tml_SndStream_Register_OnError(FTMLCore.TMLCoreHandle,
                                              FStreamID,
                                              @OnReceiverStreamError,
                                              TML_POINTER(Self));
      end;
      FIsSender := (err = TML_SUCCESS);
      if not FIsSender then
      begin
        tml_SndStream_Close(FTMLCore.TMLCoreHandle, FStreamID);
        FStreamID := TML_STREAM_ID_NULL;
      end;
    end;
    FLastError := err;
  end;
  PostInitStream;
end;

constructor TTMLStreamTypeBase.Create(ATMLProfile: TTMLProfile;
                                      AStreamID:   TML_STREAM_ID;
                                      SenderHost, SenderPort: string);
begin
  Create;
  FStreamID   := AStreamID;
  FTMLProfile := ATMLProfile;
  FSenderHost := SenderHost;
  FSenderPort := SenderPort;
  if (AStreamID <> TML_STREAM_ID_NULL) and
     Assigned(FTMLProfile) and Assigned(FTMLProfile.TMLCore) then
  begin
    FTMLCore   := FTMLProfile.TMLCore;
    FLastError := tml_RecStream_Open(FTMLCore.TMLCoreHandle, FStreamID,
                                     PSIDEX_STRING(FTMLProfile.Profile),
                                     PSIDEX_STRING(SenderHost),
                                     PSIDEX_STRING(SenderPort));
    FIsReceiver := (FLastError = TML_SUCCESS);
  end;
end;

destructor TTMLStreamTypeBase.Destroy;
begin
  if Assigned(FTMLCore) then
  begin
    if IsSender then
    begin
      tml_SndStream_Close(FTMLCore.TMLCoreHandle, FStreamID);
    end;
    if IsReceiver then
    begin
      tml_RecStream_Close(FTMLCore.TMLCoreHandle, FStreamID, TML_FALSE);
    end;
    FStreamID   := TML_STREAM_ID_NULL;
    FIsSender   := false;
    FIsReceiver := false;
  end;
  GetLock;
  {$if defined(FPC)}
    {$if defined(Unix)}
      FreeAndNil(FStreamLock);
    {$else}
      DeleteCriticalSection(FStreamLock);
    {$ifend}
  {$else}
    FreeAndNil(FStreamLock);
  {$ifend}
end;

procedure TTMLStreamTypeBase.GetLock;
begin
  {$if defined(FPC)}
    {$if defined(Unix)}
      FStreamLock.Enter;
    {$else}
      EnterCriticalSection(FStreamLock);
    {$ifend}
  {$else}
    FStreamLock.Enter;
  {$ifend}
end;

procedure TTMLStreamTypeBase.ReleaseLock;
begin
  {$if defined(FPC)}
    {$if defined(Unix)}
      FStreamLock.Leave;
    {$else}
      LeaveCriticalSection(FStreamLock);
    {$ifend}
  {$else}
    FStreamLock.Leave;
  {$ifend}
end;

function TTMLStreamTypeBase.IsSender: Boolean;
begin
  Result := FIsSender and (FStreamID <> TML_STREAM_ID_NULL);
end;

function TTMLStreamTypeBase.IsReceiver: Boolean;
begin
  Result := FIsReceiver and (FStreamID <> TML_STREAM_ID_NULL);
end;

function TTMLStreamTypeBase.GetStreamSize: TML_INT64;
begin
  Result := 0;
  if IsSender then Result := FStreamSize
  else if IsReceiver and Assigned(FTMLCore) then
  begin
    FLastError := tml_RecStream_GetSize(FTMLCore.TMLCoreHandle,
                                        FStreamID, Result);
    FStreamSize := Result;
  end;
end;

function TTMLStreamTypeBase.GetStreamPos(bRemote: Boolean): TML_INT64;
begin
  Result := 0;
  if IsSender then
  begin
    if bRemote then Result := FStreamPos_Remote
               else Result := FStreamPos_Local;
  end
  else if IsReceiver and Assigned(FTMLCore) then
  begin
    FLastError := tml_RecStream_GetPosition(FTMLCore.TMLCoreHandle,
                                            FStreamID, Result);
  end;
end;

function TTMLStreamTypeBase._GetStreamPos: TML_INT64;
begin
  Result := GetStreamPos(false);
end;

procedure TTMLStreamTypeBase.SetStreamPos(bRemote:    Boolean;
                                          AStreamPos: TML_INT64);
var
  newStreamPos: TML_INT64;
begin
  if IsSender then
  begin
         if AStreamPos < 0           then newStreamPos := 0
    else if AStreamPos > FStreamSize then newStreamPos := FStreamSize
    else                                  newStreamPos := AStreamPos;
    if bRemote then FStreamPos_Remote := newStreamPos
               else FStreamPos_Local  := newStreamPos;
  end
  else if IsReceiver and Assigned(FTMLCore) then
  begin
    FLastError := tml_RecStream_Seek(FTMLCore.TMLCoreHandle,
                                     FStreamID, AStreamPos,
                                     TML_SEEK_ORIGIN_BEGINNING);
  end;
end;

procedure TTMLStreamTypeBase._SetStreamPos(AStreamPos: TML_INT64);
begin
  SetStreamPos(false, AStreamPos);
end;

function TTMLStreamTypeBase.ReadFromStream(bRemote:      Boolean;
                                           Buffer:       Pointer;
                                           BufferLength: TML_INT32): TML_INT32;
var
  err: TML_INT32;
begin
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then
    begin
      Result := 0;
      if Assigned(TMLCore) then
      begin
        err := tml_RecStream_Read(TMLCore.TMLCoreHandle,
                                  FStreamID, Buffer, BufferLength,
                                  Result);  // bytes read -> Result
      end
      else err := TML_ERR_MISSING_OBJ;
      FLastError := err;
      if err <> TML_SUCCESS then Result := 0;
    end
    else
    begin
      // The derived class must supply data in 'Buffer' and
      // advance the stream position for the read byte count.
      // The read byte count must be returned in 'Result'.
      Result := 0;
    end;
  end
  else Result := 0;
end;

function TTMLStreamTypeBase.WriteToStream(bRemote:      Boolean;
                                          Buffer:       Pointer;
                                          BufferLength: TML_INT32): Boolean;
var
  err: TML_INT32;
begin
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then
    begin
      if Assigned(TMLCore) then
      begin
        err := tml_RecStream_Write(TMLCore.TMLCoreHandle,
                                   FStreamID, Buffer, BufferLength);
      end
      else err := TML_ERR_MISSING_OBJ;
      FLastError := err;
      Result := (err = TML_SUCCESS);
    end
    else
    begin
      // The derived class must consume the data in 'Buffer' and
      // advance the stream position for 'BufferLength'.
      // On success 'true' must be returned in 'Result' otherwise 'false'.
      Result := false;
    end;
  end
  else Result := false;
end;

function TTMLStreamTypeBase.Read(Buffer: Pointer; BufferLength: TML_INT32;
                                 SeekPos: TML_INT64): TML_INT32;
begin
  Result := 0;
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    GetLock;
    if SeekPos >= 0 then SetStreamPos(false, SeekPos);
    Result := ReadFromStream(false, Buffer, BufferLength);
    ReleaseLock;
  end;
end;

function TTMLStreamTypeBase.Write(Buffer: Pointer; BufferLength: TML_INT32;
                                  SeekPos: TML_INT64): Boolean;
begin
  Result := false;
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    GetLock;
    if SeekPos >= 0 then SetStreamPos(false, SeekPos);
    Result := WriteToStream(false, Buffer, BufferLength);
    ReleaseLock;
  end;
end;

function TTMLStreamTypeBase.Download(pCBDownloadFunc: TTMLStreamDownloadCallback;
                                     pCBFinishedFunc: TTMLStreamFinishedCallback;
                                     ASynchronizeCall:  Boolean;
                                     DesiredBufferSize: LongInt): Boolean;
var
  err, n, iBuffer, nBuffers: TML_INT32;
  Buf: Pointer;
  p:   TML_INT64;
begin
  Result := false;
  FSynchronizeCall  := ASynchronizeCall;
  FDownloadCallback := pCBDownloadFunc;
  FFinishedCallback := pCBFinishedFunc;
  if Assigned(FDownloadCallback) then
  begin
    if IsReceiver then
    begin
      if Assigned(FTMLCore) and (DesiredBufferSize > 0) then
      begin
        err := tml_RecStream_DownloadData(FTMLCore.TMLCoreHandle,
                                          FStreamID, DesiredBufferSize,
                                          @OnStreamDownload,
                                          TML_POINTER(Self),
                                          @OnStreamFinished,
                                          TML_POINTER(Self));
        FLastError := err;
        Result := (err = TML_SUCCESS);
      end;
    end
    else
    begin
      GetLock;
      p := GetStreamPos(false);
      try
        {$if defined(FPC)}
        Buf := nil;
        {$ifend}
        GetMem(Buf, DesiredBufferSize);
        if Assigned(Buf) then
        begin
          SetStreamPos(false, 0);
          iBuffer  := 0;
          nBuffers := (FStreamSize + DesiredBufferSize - 1) div DesiredBufferSize;
          repeat
            Inc(iBuffer); // iBuffer = 1..nBuffers
            n := ReadFromStream(false, Buf, DesiredBufferSize);
            if n > 0 then
            begin
              Result := (FDownloadCallback(Self, FStreamID, Buf, n,
                                           iBuffer, nBuffers) = 0);
            end;
          until (n <= 0) or (iBuffer = nBuffers) or not Result;
          FreeMem(Buf);
          FDownloadCallback := nil;
          Result := true;
        end;
      except
        Result := false;
      end;
      SetStreamPos(false, p);
      ReleaseLock;
      if Assigned(FFinishedCallback) then
      begin
        FFinishedCallback(Self, FStreamID, FLastError);
        FFinishedCallback := nil;
      end;
    end;
  end;
end;

//#---------------------------------------------------------------------
// stream callback functions
//#---------------------------------------------------------------------

function OnStreamGetPosition(iStreamID : TML_STREAM_ID;
                             pCBData   : TML_POINTER): TML_INT64; cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) then Result := stb.GetStreamPos(true)
                   else Result := -1; // "not opearable"
end;

function OnStreamGetSize(iStreamID : TML_STREAM_ID;
                         pCBData   : TML_POINTER): TML_INT64; cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) then Result := stb.GetStreamSize
                   else Result := -1; // "not opearable"
end;

function OnStreamRead(iStreamID : TML_STREAM_ID;
                      pCBData   : TML_POINTER;
                      buffer    : TML_POINTER;
                      count     : TML_INT32;
                  out bytesRead : TML_INT32): TML_INT32; cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) then
  begin
    bytesRead := stb.ReadFromStream(true, buffer, count);
    Result    := stb.LastError;
  end
  else Result := -1;  // "not opearable"
end;

function OnStreamWrite(iStreamID : TML_STREAM_ID;
                       pCBData   : TML_POINTER;
                       buffer    : TML_POINTER;
                       count     : TML_INT32): TML_INT32; cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) then
  begin
    stb.WriteToStream(true, buffer, count);
    Result := stb.LastError;
  end
  else Result := -1;  // "not opearable"
end;

function OnStreamSeek(iStreamID    : TML_STREAM_ID;
                      pCBData      : TML_POINTER;
                      seekPosition : TML_INT64;
                      seekOrigin   : TML_SEEK_ORIGIN): TML_INT32; cdecl;
var
  stb: TTMLStreamTypeBase;
  newPosition, ss: TML_INT64;
begin
  Result := -1; // "not opearable"
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) then
  begin
    ss := stb.GetStreamSize;
    case seekOrigin of
      TML_SEEK_ORIGIN_BEGINNING: newPosition := seekPosition;
      TML_SEEK_ORIGIN_CURRENT:   newPosition := stb.GetStreamPos(true) + seekPosition;
      TML_SEEK_ORIGIN_END:       newPosition := ss - seekPosition;
      else Exit;
    end;
         if newPosition > ss then newPosition := ss
    else if newPosition < 0  then newPosition := 0
    else Result := 0; // "success"
    stb.SetStreamPos(true, newPosition);
  end;
end;

procedure OnReceiverStreamClosed(iStreamID : TML_STREAM_ID;
                                 pCBData   : TML_POINTER); cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) and Assigned(stb.ClosedCallback) then
  begin
    stb.ClosedCallback(stb, iStreamID);
  end;
end;

procedure OnReceiverStreamError(iStreamID : TML_STREAM_ID;
                                iError    : TML_INT32;
                                pCBData   : TML_POINTER); cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) and Assigned(stb.ErrorCallback) then
  begin
    stb.FLastError := iError;
    stb.ErrorCallback(stb, iStreamID, iError);
  end;
end;

function OnStreamDownload(iStreamID   : TML_STREAM_ID;
                          pCBData     : TML_POINTER;
                          buffer      : TML_POINTER;
                          bytesRead   : TML_INT32;
                          actBufferNo : TML_INT64;
                          bufferCount : TML_INT64): TML_INT32; cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  Result := -1; // "not opearable"
  stb := TTMLStreamTypeBase(pCBData);
  if Assigned(stb) and Assigned(stb.DownloadCallback) then
  begin
    if stb.SynchronizeCall then
    begin
      if not Assigned(tmlSyncCallCritSect) then
        tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
      if Assigned(tmlSyncCallCritSect) then
      begin
        tmlSyncCallCritSect.Enter;
        try
          tmlSyncCallCritSect.Clear;
          tmlSyncCallCritSect.DownloadMethod := stb.DownloadCallback;
          tmlSyncCallCritSect.stb            := stb;
          tmlSyncCallCritSect.StreamID       := stb.StreamID;
          tmlSyncCallCritSect.p1             := buffer;
          tmlSyncCallCritSect.i1             := bytesRead;
          tmlSyncCallCritSect.l1             := actBufferNo;
          tmlSyncCallCritSect.l2             := bufferCount;
          {$if defined(FPC)}
            TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
          {$else}
            TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
          {$ifend}
          Result := tmlSyncCallCritSect.iResult;
        finally
          tmlSyncCallCritSect.Leave;
        end;
      end;
    end
    else Result := stb.DownloadCallback(stb, stb.StreamID, buffer, bytesRead,
                                        actBufferNo, bufferCount);
  end;
end;

procedure OnStreamFinished(iStreamID  : TML_STREAM_ID;
                           errCode    : TML_INT32;
                           pCBDataRet : TML_POINTER); cdecl;
var
  stb: TTMLStreamTypeBase;
begin
  stb := TTMLStreamTypeBase(pCBDataRet);
  if Assigned(stb) and Assigned(stb.FinishedCallback) then
  begin
    if stb.SynchronizeCall then
    begin
      if not Assigned(tmlSyncCallCritSect) then
        tmlSyncCallCritSect := TSyncExecuteCritSec.Create;
      if Assigned(tmlSyncCallCritSect) then
      begin
        tmlSyncCallCritSect.Enter;
        try
          tmlSyncCallCritSect.Clear;
          tmlSyncCallCritSect.FinishedMethod := stb.FinishedCallback;
          tmlSyncCallCritSect.stb            := stb;
          tmlSyncCallCritSect.StreamID       := stb.StreamID;
          tmlSyncCallCritSect.i1             := errCode;
          {$if defined(FPC)}
            TThread.Synchronize(nil, @tmlSyncCallCritSect.SyncExecute);
          {$else}
            TThread.Synchronize(nil, tmlSyncCallCritSect.SyncExecute);
          {$ifend}
        finally
          tmlSyncCallCritSect.Leave;
        end;
      end;
    end
    else stb.FinishedCallback(stb, stb.StreamID, errCode);
    stb.FDownloadCallback := nil;
    stb.FFinishedCallback := nil;
  end;
end;

//#---------------------------------------------------------------------
{ TTMLStreamTypeMemory }
//#---------------------------------------------------------------------

procedure TTMLStreamTypeMemory.InitStream;
begin
  inherited InitStream;
  FMemoryBuffer    := nil;
  FMemorySize      := 0;
  FMemoryOwner     := false;
end;

destructor TTMLStreamTypeMemory.Destroy;
begin
  ReleaseMemoryBuffer;
  inherited Destroy;
end;

procedure TTMLStreamTypeMemory.SetMemoryBuffer(Buffer:     Pointer;
                                               BufferSize: TML_INT32);
begin
  ReleaseMemoryBuffer;
  FMemoryOwner  := false;
  FMemoryBuffer := Buffer;
  if Assigned(FMemoryBuffer) then FMemorySize := BufferSize
                             else FMemorySize := 0;
end;

function TTMLStreamTypeMemory.CreateMemoryBuffer(BufferSize: TML_INT32): Boolean;
begin
  ReleaseMemoryBuffer;
  if BufferSize > 0 then
  begin
    try
      GetMem(FMemoryBuffer, BufferSize);
      FMemoryOwner := true;
    except
      FMemoryBuffer := nil;
    end;
  end;
  Result := Assigned(FMemoryBuffer);
  if Result then FMemorySize := BufferSize
            else FMemorySize := 0;
end;

procedure TTMLStreamTypeMemory.ReleaseMemoryBuffer;
begin
  if FMemoryOwner and Assigned(FMemoryBuffer) then
  begin
    FreeMem(FMemoryBuffer);
  end;
  FMemoryOwner  := false;
  FMemoryBuffer := nil;
  FMemorySize   := 0;
end;

function TTMLStreamTypeMemory.LoadStreamInMemory: Boolean;
begin
  if IsReceiver then
  begin
    GetLock;
    if not Assigned(FMemoryBuffer) then CreateMemoryBuffer(GetStreamSize);
    Result := LoadFirstStreamPart;
    ReleaseLock;
  end
  else Result := true;
end;

function TTMLStreamTypeMemory.LoadFirstStreamPart: Boolean;
begin
  SetStreamPos(false, 0);
  Result := LoadNextStreamPart;
end;

function TTMLStreamTypeMemory.LoadNextStreamPart: Boolean;
begin
  if Assigned(FMemoryBuffer) and (FMemorySize > 0) then
  begin
    FMemoryLength := ReadFromStream(false, FMemoryBuffer, FMemorySize);
  end
  else FMemoryLength := 0;
  Result := (FMemoryLength > 0);
end;

function TTMLStreamTypeMemory.ReadFromStream(bRemote:      Boolean;
                                             Buffer:       Pointer;
                                             BufferLength: TML_INT32): TML_INT32;
var
  pStreamPos: PTML_INT64;
begin
  Result := 0;
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then inherited ReadFromStream(bRemote, Buffer, BufferLength)
    else
    begin
      if Assigned(FMemoryBuffer) and (FMemoryLength > 0) then
      begin
        GetLock;
        if bRemote then pStreamPos := @FStreamPos_Remote
                   else pStreamPos := @FStreamPos_Local;
        Result := FMemoryLength - pStreamPos^;
        if Result > BufferLength then Result := BufferLength;
        if Result > 0 then
        begin
          Move(Pointer(Cardinal(FMemoryBuffer) + pStreamPos^)^, Buffer^, Result);
          pStreamPos^ := pStreamPos^ + Result;
        end
        else Result := 0;
        ReleaseLock;
      end;
    end;
  end;
end;

function TTMLStreamTypeMemory.WriteToStream(bRemote:      Boolean;
                                            Buffer:       Pointer;
                                            BufferLength: TML_INT32): Boolean;
var
  pStreamPos: PTML_INT64;
begin
  Result := false;
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then inherited WriteToStream(bRemote, Buffer, BufferLength)
    else
    begin
      Result := true;
      GetLock;
      if bRemote then pStreamPos := @FStreamPos_Remote
                 else pStreamPos := @FStreamPos_Local;
      if not Assigned(FMemoryBuffer) then CreateMemoryBuffer(BufferLength);
      if (pStreamPos^ + BufferLength) > FMemorySize then
      begin
        if FMemoryOwner then
        begin
          FMemorySize := pStreamPos^ + BufferLength;
          try
            ReallocMem(FMemoryBuffer, FMemorySize);
          except
            FMemoryBuffer := nil;
            FMemorySize   := 0;
            Result        := false;
          end;
        end
        else Result := false;
      end;
      if Result and Assigned(FMemoryBuffer) and
         ((pStreamPos^ + BufferLength) <= FMemorySize) then
      begin
        Move(Buffer^, Pointer(Cardinal(FMemoryBuffer) + pStreamPos^)^, BufferLength);
        pStreamPos^ := pStreamPos^ + BufferLength;
        if pStreamPos^ > FMemoryLength then
          FMemoryLength := pStreamPos^;
      end
      else Result := false;
      ReleaseLock;
    end;
  end;
end;

//#---------------------------------------------------------------------
{ TTMLStreamTypeFile }
//#---------------------------------------------------------------------

procedure TTMLStreamTypeFile.InitStream;
begin
  inherited InitStream;
  FLoadFileStream           := nil;
  FSaveFileStream           := nil;
  FLockFileStream           := nil;
  FLoadFileName             := '';
  FSaveFileName             := '';
  FWritePermission          := false;
  FSaveFileResult           := 0;
  FSaveFileProgressCallback := nil;
  FSaveFileFinishedCallback := nil;
end;

procedure TTMLStreamTypeFile.PostInitStream;
begin
  inherited PostInitStream;
  FWritePermission := IsReceiver;
end;

destructor TTMLStreamTypeFile.Destroy;
begin
  ReleaseFile;
  inherited Destroy;
end;

procedure TTMLStreamTypeFile.ReleaseFile;
begin
  FreeAndNil(FLoadFileStream);
  FreeAndNil(FSaveFileStream);
  FreeAndNil(FLockFileStream);
  FLoadFileName             := '';
  FSaveFileName             := '';
  FStreamSize               := 0;
  FStreamPos_Local          := 0;
  FStreamPos_Remote         := 0;
  FSaveFileProgressCallback := nil;
  FSaveFileFinishedCallback := nil;
end;

procedure TTMLStreamTypeFile.SetFileName(AFileName: string);
var
  Mode: Word;
begin
  if AFileName <> FLoadFileName then
  begin
    GetLock;
    ReleaseFile;
    FLoadFileName := AFileName;
    if FLoadFileName <> '' then
    begin
      Mode := 0;
      if IsSender and FileExists(FLoadFileName) then
      begin
        if FWritePermission then Mode := fmOpenReadWrite
                            else Mode := fmOpenRead;
      end
      else
      begin
        if IsReceiver or FWritePermission then
        begin
          FWritePermission := true;
          Mode := fmCreate;
        end
        else FLoadFileName := '';
      end;
      if FLoadFileName <> '' then
      begin
        FLoadFileStream := TFileStream.Create(FLoadFileName,
                                              Mode or fmShareDenyWrite);
        if Assigned(FLoadFileStream) then
        begin
          FStreamSize       := FLoadFileStream.Size;
          FStreamPos_Local  := FLoadFileStream.Position;
          FStreamPos_Remote := FStreamPos_Local;
        end;
      end;
    end;
    ReleaseLock;
  end;
end;

procedure TTMLStreamTypeFile.SetWritePermission(WritingAllowed: Boolean);
var
  fn: string;
begin
  if IsSender and (WritingAllowed <> FWritePermission) then
  begin
    GetLock;
    fn := FLoadFileName;
    ReleaseFile;
    FWritePermission := WritingAllowed;
    SetFileName(fn);
    ReleaseLock;
  end;
end;

function TTMLStreamTypeFile.SaveStreamToFile(AFileName: string;
                                             UseTempFile: Boolean;
                                             pCBProgress: TTMLStreamProgressCallback;
                                             pCBFinished: TTMLStreamFinishedCallback;
                                             ASynchronizeCall: Boolean): Boolean;
begin
  Result := false;
  if not Assigned(FSaveFileStream) then
  begin
    if AFileName <> '' then FSaveFileName := AFileName
    else if IsReceiver then FSaveFileName := FLoadFileName;
    if FSaveFileName <> '' then
    begin
      FSaveFileProgressCallback := pCBProgress;
      FSaveFileFinishedCallback := pCBFinished;
      FUseTempFile := UseTempFile and FileExists(FSaveFileName);
      if FUseTempFile then FTempFileName := FSaveFileName + '.tml.tmp'
                      else FTempFileName := '';
      {$if defined(FPC)}
        Result := Download(@SaveFileCallback,
                           @SaveFileFinishedCallback,
                           ASynchronizeCall);
      {$else}
        Result := Download(SaveFileCallback,
                           SaveFileFinishedCallback,
                           ASynchronizeCall);
      {$ifend}
      if not Assigned(pCBFinished) then
      begin
        // wait until finished...
        repeat
          Sleep(0);
        until FSaveFileName = '';
        Result := (FSaveFileResult = 0);
      end;
    end;
  end;
end;

function TTMLStreamTypeFile.ReadFromStream(bRemote:      Boolean;
                                           Buffer:       Pointer;
                                           BufferLength: TML_INT32): TML_INT32;
var
  pStreamPos: PTML_INT64;
  n:          TML_INT64;
begin
  Result := 0;
  if Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then Result := inherited ReadFromStream(bRemote, Buffer, BufferLength)
    else
    begin
      if Assigned(FLoadFileStream) then
      begin
        GetLock;
        if bRemote then pStreamPos := @FStreamPos_Remote
                   else pStreamPos := @FStreamPos_Local;
        FLoadFileStream.Position := pStreamPos^;
        n := FLoadFileStream.Size - FLoadFileStream.Position;
        if n > BufferLength then n := BufferLength;
        if n < 0 then n := 0;
        if n > 0 then Result := FLoadFileStream.Read(Buffer^, n)
                 else Result := 0;
        pStreamPos^ := FLoadFileStream.Position;
        ReleaseLock;
      end;
    end;
  end;
end;

function TTMLStreamTypeFile.WriteToStream(bRemote:      Boolean;
                                          Buffer:       Pointer;
                                          BufferLength: TML_INT32): Boolean;
var
  pStreamPos: PTML_INT64;
begin
  Result := false;
  if FWritePermission and Assigned(Buffer) and (BufferLength > 0) then
  begin
    if IsReceiver then Result := inherited WriteToStream(bRemote, Buffer, BufferLength)
    else
    begin
      if Assigned(FLoadFileStream) then
      begin
        GetLock;
        if bRemote then pStreamPos := @FStreamPos_Remote
                   else pStreamPos := @FStreamPos_Local;
        FLoadFileStream.Position := pStreamPos^;
        Result := (FLoadFileStream.Write(Buffer, BufferLength) = BufferLength);
        pStreamPos^ := FLoadFileStream.Position;
        ReleaseLock;
      end;
    end;
  end;
end;

function TTMLStreamTypeFile.SaveFileCallback(Sender:         TTMLStreamTypeBase;
                                             AStreamID:      TML_STREAM_ID;
                                             Buffer:         TML_POINTER;
                                             BytesRead:      TML_INT32;
                                             TotalBytesRead: TML_INT64;
                                             theStreamSize:  TML_INT64): TML_INT32;
var
  Percent: TML_INT32;
  TempFN:  string;
begin
  Result := 0;
  try
    if Assigned(Buffer) and (BytesRead > 0) then
    begin
      Result := -1;
      if (TotalBytesRead = BytesRead) then  // this is the first file part
      begin
        FLastPercent := -1;
        if FUseTempFile then TempFN := FTempFileName
                        else TempFN := FSaveFileName;
        if (TempFN <> '') and (FSaveFileName <> '') then
        begin
          if FUseTempFile then
          begin
            // lock the destination file...
            FLockFileStream := TFileStream.Create(FSaveFileName,
                                                  fmOpenReadWrite or fmShareExclusive);
          end;
          // create/open the file...
          FSaveFileStream := TFileStream.Create(TempFN, fmCreate or fmShareExclusive);
          if Assigned(FSaveFileStream) then
          begin
            FSaveFileResult := 0;
            if Assigned(FSaveFileProgressCallback) then
            begin
              if FSaveFileProgressCallback(Sender, AStreamID, TotalBytesRead,
                                           theStreamSize, 0) <> TML_FALSE then
              begin
                Result := -2; // canceled by user
              end;
            end;
          end;
        end;
      end;
      if Assigned(FSaveFileStream) and (Result <> -2) then
      begin
        // write data to file...
        Result := 0;
        if FSaveFileStream.Write(Buffer^, BytesRead) <> BytesRead then Result := -1;
        if Assigned(FSaveFileProgressCallback) then
        begin
          if theStreamSize > 0 then Percent := (TotalBytesRead * 100) div theStreamSize
                               else Percent := 100;
          if FSaveFileProgressCallback(Sender, AStreamID, TotalBytesRead,
                                       theStreamSize, Percent) <> TML_FALSE then
          begin
            Result := -2; // canceled by user
          end;
          FLastPercent := Percent;
        end;
      end;
    end;
  except
    Result := -1;
  end;
  if Result <> 0 then
  begin
    // close the file...
    FreeAndNil(FSaveFileStream);
    FSaveFileResult := Result;
  end;
end;

procedure TTMLStreamTypeFile.SaveFileFinishedCallback(Sender:    TTMLStreamTypeBase;
                                                      AStreamID: TML_STREAM_ID;
                                                      errCode:   TML_INT32);
begin
  if FUseTempFile and (errCode = TML_SUCCESS) and
     Assigned(FSaveFileStream) and Assigned(FLockFileStream) then
  begin
    // download is finished successfully, make temp file to destination file...
    FreeAndNil(FLockFileStream);
    SysUtils.DeleteFile(FSaveFileName);
    FreeAndNil(FSaveFileStream);
    SysUtils.RenameFile(FTempFileName, FSaveFileName);
  end
  else
  begin
    FreeAndNil(FSaveFileStream);
    FreeAndNil(FLockFileStream);
    if FUseTempFile and FileExists(FTempFileName) then
    begin
      SysUtils.DeleteFile(FTempFileName);
    end;
  end;
  FUseTempFile    := false;
  FSaveFileResult := errCode;
  FTempFileName   := '';
  FSaveFileName   := '';
  if Assigned(FSaveFileFinishedCallback) then
  begin
    FSaveFileFinishedCallback(Sender, AStreamID, errCode);
  end;
end;

//#---------------------------------------------------------------------

initialization

tmlSyncCallCritSect := nil;

Classes.RegisterClass(TTMLCommand);

//------------------------------------------------------------------------------

finalization

FreeAndNil(tmlSyncCallCritSect);

//------------------------------------------------------------------------------

end.

