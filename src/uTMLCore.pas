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
 *  For commercial support on build TML enabled solutions contact us:
 *
 * Contributors:
 *    wobe-systems GmbH
 *    support@libtml.org
 *)

//------------------------------------------------------------------------------

unit uTMLCore;

{$if not defined(FPC)}
  {$if CompilerVersion >= 25}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion >= 24}
    {$define USE_NAMESPACES}
  {$ifend}
{$ifend}

{$if defined(FPC)}
  {$mode objfpc}{$H+}
{$ifend}

//------------------------------------------------------------------------------

interface

uses
  {$if defined(FPC)}
    Interfaces,
    dynlibs,
    Classes,
  {$ifend}
  {$if defined(Windows) or defined(MSWINDOWS)}
    Windows,
  {$ifend}
  {$if defined(ANDROID)}
    System.IOUtils,
  {$ifend}
  SysUtils, uTMLTypes, uSidexTypes, uSidexLib;

//------------------------------------------------------------------------------

const
  TMLCoreLibName    = 'tmlCore';
  TMLCoreLibVersion = '11';

  {$if defined(Unix)}
    TML_CORE_DLLHANDLE_NULL = nil;
  {$else}
    TML_CORE_DLLHANDLE_NULL = 0;
  {$ifend}

  {$if defined(Unix) or defined(ANDROID)}
    TMLCoreLibPrefix = 'lib';
  {$else}
    TMLCoreLibPrefix = '';
  {$ifend}

  {$if defined(CPU64) or defined(CPUX64)}
    TMLCoreLibMiddle = '64-';
  {$else}
    TMLCoreLibMiddle = '';
  {$ifend}

  {$if defined(TML_DEBUG)}
    TMLCoreLibDebug = 'd';
  {$else}
    TMLCoreLibDebug = '';
  {$ifend}

  {$if not defined(FPC)}
    {$if defined(Unix) or defined(ANDROID)}
      SharedSuffix = 'so';
    {$else}
      SharedSuffix = 'dll';
    {$ifend}
  {$ifend}

  tmlcoredllname = TMLCoreLibPrefix + TMLCoreLibName + TMLCoreLibMiddle +
                   TMLCoreLibVersion + TMLCoreLibDebug + '.' + SharedSuffix;

  {$if defined(ANDROID)}
    SidexVortexLibName = TMLCoreLibPrefix + 'vortex-1.1' + '.' + SharedSuffix;
  {$ifend}

  {$if defined(UNICODE)}
    tmlStringTypeSuffix = '_W'; // UTF16 strings
  {$else}
    tmlStringTypeSuffix = '_A'; // UTF8 strings
  {$ifend}

  {$if defined(FPC)}
    {$PACKRECORDS C}
  {$ifend}

  /////////////////////////////////////////////////
  // The command reply types:
  /////////////////////////////////////////////////
  TMLCOM_RPY_UNDEFINED            = -1;
  TMLCOM_RPY_PROGRESS             =  0;
  TMLCOM_RPY_WARNING              = 10;
  TMLCOM_RPY_ERROR                = 20;
  TMLCOM_RPY_INFORMATION          = 30;


  /////////////////////////////////////////////////
  // The command modes:
  /////////////////////////////////////////////////
  TMLCOM_MODE_UNDEFINED           = -1;
  TMLCOM_MODE_ASYNC               =  0;
  TMLCOM_MODE_SYNC                =  1;
  TMLCOM_MODE_EVT                 =  2;


  /////////////////////////////////////////////////
  // The command states:
  /////////////////////////////////////////////////
  TMLCOM_CSTATE_UNDEFINED         = -1;
  TMLCOM_CSTATE_CREATED           =  0;
  TMLCOM_CSTATE_EXECUTED          =  1;
  TMLCOM_CSTATE_FAILED            =  2;
  TMLCOM_CSTATE_PENDING           =  3;
  MIN_INTERNAL_CMD_SET            =  0;

  /////////////////////////////////////////////////
  // The logging masks:
  /////////////////////////////////////////////////
  TML_LOG_OFF                     = $0000;
  TML_LOG_VORTEX_INTERN           = $0001;
  TML_LOG_VORTEX_VERBOSE          = $0002;
  TML_LOG_VORTEX_CMD              = $0004;
  TML_LOG_CORE_IO                 = $0008;
  TML_LOG_CORE_API                = $0010;
  TML_LOG_VORTEX_FRAMES           = $0020;
  TML_LOG_VORTEX_CH_POOL          = $0040;
  TML_LOG_VORTEX_MUTEX            = $0080;
  TML_LOG_MULTY_SYNC_CMDS         = $0100;
  TML_LOG_VORTEX_HASHTABLE        = $0400;
  TML_LOG_INTERNAL_DISPATCH       = $0800;
  TML_LOG_CONNECTION_OBJ_HANDLING = $1000;
  TML_LOG_STREAM_HANDLING         = $2000;

//------------------------------------------------------------------------------

type
  {$if defined(Unix)}
    TTML_CORE_DLLHANDLE = Pointer;
  {$else}
    TTML_CORE_DLLHANDLE = THandle;
  {$ifend}

  { callback types }
  TTML_OnCommandCall                = procedure(tmlhandle  : TML_COMMAND_HANDLE;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnCommandCall                = ^TTML_OnCommandCall;

  TTML_OnDeleteCommand              = procedure(cmd        : TML_COMMAND_ID;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnDeleteCommand              = ^TTML_OnDeleteCommand;

  TTML_OnCustomDispatch             = procedure(cmd        : TML_COMMAND_ID;
                                                cmdhandle  : TML_COMMAND_HANDLE;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnCustomDispatch             = ^TTML_OnCustomDispatch;

  TTML_OnEventError                 = procedure(sProfile   : PSIDEX_STRING;
                                                sHost      : PSIDEX_STRING;
                                                sPort      : PSIDEX_STRING;
                                                cmd        : TML_COMMAND_ID;
                                                ErrorCode  : TML_INT32;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnEventError                 = ^TTML_OnEventError;

  TTML_OnEventQueueOverflow         = procedure(sProfile   : PSIDEX_STRING;
                                                cmd        : TML_COMMAND_ID;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnEventQueueOverflow         = ^TTML_OnEventQueueOverflow;

  TTML_OnPopulateReceiver           = procedure(sProfile   : PSIDEX_STRING;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnPopulateReceiver           = ^TTML_OnPopulateReceiver;

  TTML_OnPeerRegister               = function(bSubscribe  : TML_BOOL;
                                                sHost      : PSIDEX_STRING;
                                                sPort      : PSIDEX_STRING;
                                                pCBData    : TML_POINTER) : TML_BOOL; cdecl;
  PTML_OnPeerRegister               = ^TTML_OnPeerRegister;

  TTML_OnCmdProgress                = procedure(tmlhandle  : TML_COMMAND_HANDLE;
                                                pCBData    : TML_POINTER;
                                                progress   : TML_INT32); cdecl;
  PTML_OnCmdProgress                = ^TTML_OnCmdProgress;

  TTML_OnCmdReady                   = procedure(tmlhandle  : TML_COMMAND_HANDLE;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnCmdReady                   = ^TTML_OnCmdReady;

  TTML_OnCmdStatusRpy               = procedure(tmlhandle  : TML_COMMAND_HANDLE;
                                                pCBData    : TML_POINTER;
                                                itype      : TML_INT32;
                                                smsg       : PSIDEX_STRING); cdecl;
  PTML_OnCmdStatusRpy               = ^TTML_OnCmdStatusRpy;

  TTML_OnBalBusyStatusRequest       = function(tmlhandle   : TML_COMMAND_HANDLE;
                                               pCBData     : TML_POINTER) : TML_INT32; cdecl;
  PTML_OnBalBusyStatusRequest       = ^TTML_OnBalBusyStatusRequest;

  TTML_OnBalCalculation             = function(iNumberOfListener : TML_INT32;
                                               listenerBusyStateArray : PTML_COMMAND_HANDLE_ARRAY;
                                               pCBData     : TML_POINTER;
                                               out iNextListenerIndex : TML_INT32) : TML_INT32; cdecl;
  PTML_OnBalCalculation             = ^TTML_OnBalCalculation;

  TTML_OnStrmGetSize                = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER): TML_INT64; cdecl;
  PTML_OnStrmGetSize                = ^TTML_OnStrmGetSize;

  TTML_OnStrmGetPos                 = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER): TML_INT64; cdecl;
  PTML_OnStrmGetPos                 = ^TTML_OnStrmGetPos;

  TTML_OnStrmRead                   = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER;
                                               buffer      : TML_POINTER;
                                               count       : TML_INT32;
                                           out bytesRead   : TML_INT32): TML_INT32; cdecl;
  PTML_OnStrmRead                   = ^TTML_OnStrmRead;

  TTML_OnStrmWrite                  = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER;
                                               buffer      : TML_POINTER;
                                               count       : TML_INT32): TML_INT32; cdecl;
  PTML_OnStrmWrite                  = ^TTML_OnStrmWrite;

  TTML_OnStrmSeek                   = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER;
                                               seekPosition: TML_INT64;
                                               seekOrigin  : TML_SEEK_ORIGIN): TML_INT32; cdecl;
  PTML_OnStrmSeek                   = ^TTML_OnStrmSeek;

  TTML_OnStrmClose                  = procedure(iStreamID  : TML_STREAM_ID;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnStrmClose                  = ^TTML_OnStrmClose;

  TTML_OnStrmError                  = procedure(iStreamID  : TML_STREAM_ID;
                                                iError     : TML_INT32;
                                                pCBData    : TML_POINTER); cdecl;
  PTML_OnStrmError                  = ^TTML_OnStrmError;

  TTML_DownloadCallback             = function(iStreamID   : TML_STREAM_ID;
                                               pCBData     : TML_POINTER;
                                               buffer      : TML_POINTER;
                                               bytesRead   : TML_INT32;
                                               totalBytesRead: TML_INT64;
                                               streamSize  : TML_INT64): TML_INT32; cdecl;

  TTML_DownloadFinishedCallback     = procedure(iStreamID  : TML_STREAM_ID;
                                                errCode    : TML_INT32;
                                                pCBDataRet : TML_POINTER); cdecl;

  { Function declarations }
  Ttml_Core_Get_Version             = procedure(out iApi   : TML_INT32;
                                                out iLib   : TML_INT32;
                                                out sLib   : PSIDEX_STRING); cdecl;
  Ttml_Core_Get_Copyright           = procedure(out sValue : PSIDEX_STRING;
                                                out iLength: TML_INT32); cdecl;

  Ttml_Core_Get_Version_old         = procedure(out main  : TML_INT32;
                                                out sub   : TML_INT32); cdecl;
  Ttml_Core_Get_Version_Ext_old     = procedure(out main  : TML_INT32;
                                                out sub1  : TML_INT32;
                                                out sub2  : TML_INT32); cdecl;

  Ttml_Core_Set_Password            = function(pUserName  : PSIDEX_STRING;
                                               pPassWord  : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Configure_Thread_Pool_Handling = procedure(iInitialThreadPoolSize: TML_INT32;
                                                  iThreadPoolMaxSize    : TML_INT32;
                                                  iThreadAddSteps       : TML_INT32;
                                                  iThreadPoolAddPeriod  : TML_INT32;
                                                  bThreadAutoRemove     : TML_BOOL);
  Ttml_Core_Open                    = function(out coreHandle : TML_CORE_HANDLE;
                                                   iLogValue  : TML_INT32) : TML_INT32; cdecl;
  Ttml_Core_Close                   = function(var coreHandle : TML_CORE_HANDLE) : TML_INT32; cdecl;
  Ttml_Core_GeneralDeregistration   = function(coreHandle : TML_CORE_HANDLE) : TML_INT32; cdecl;
  Ttml_Profile_Register             = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Profile_Unregister           = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING) : TML_INT32; cdecl;

  Ttml_Profile_Get_Registered_Count = function(coreHandle : TML_CORE_HANDLE;
                                           out iSize      : TML_INT32) : TML_INT32; cdecl;
  Ttml_Profile_Get_Registered       = function(coreHandle : TML_CORE_HANDLE;
                                           out profiles   : SIDEX_VARIANT) : TML_INT32; cdecl;
  Ttml_Profile_Get_RegisterState    = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING;
                                           out bRegistered: TML_BOOL) : TML_INT32; cdecl;
  Ttml_Profile_Register_Cmd         = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING;
                                               cmd        : TML_COMMAND_ID;
                                               pCBfunc    : TTML_OnCommandCall;
                                               pCBData    : TML_POINTER): TML_INT32; cdecl;
  Ttml_Profile_Set_OnDeleteCmd      = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING;
                                               pCBFunc    : TTML_OnDeleteCommand;
                                               pCBData    : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Profile_Set_OnCustomDispatch = function(coreHandle : TML_CORE_HANDLE;
                                               profile    : PSIDEX_STRING;
                                               pCBfunc    : TTML_OnCustomDispatch;
                                               pCBData    : TML_POINTER): TML_INT32; cdecl;
  Ttml_Core_Set_ListenerPort        = function(coreHandle : TML_CORE_HANDLE;
                                               sPort      : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Core_Get_ListenerPort        = function(coreHandle : TML_CORE_HANDLE;
                                           out sPort      : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Core_Set_ListenerIP          = function(coreHandle : TML_CORE_HANDLE;
                                               sIP        : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Core_Get_ListenerIP          = function(coreHandle : TML_CORE_HANDLE;
                                           out sIP        : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Core_Set_ListenerEnabled     = function(coreHandle : TML_CORE_HANDLE;
                                               bEnable    : TML_BOOL) : TML_INT32; cdecl;
  Ttml_Core_Get_ListenerEnabled     = function(coreHandle : TML_CORE_HANDLE;
                                           out bEnable    : TML_BOOL) : TML_INT32; cdecl;
  Ttml_Core_Set_WindowSize          = function(coreHandle : TML_CORE_HANDLE;
                                               iWindowSize: TML_INT32) : TML_INT32; cdecl;
  Ttml_Core_Get_WindowSize          = function(coreHandle : TML_CORE_HANDLE;
                                           out iWindowSize: TML_INT32) : TML_INT32; cdecl;
  Ttml_Core_Set_LoggingValue        = function(coreHandle : TML_CORE_HANDLE;
                                               iLogValue  : TML_INT32) : TML_INT32; cdecl;
  Ttml_Core_Get_LoggingValue        = function(coreHandle : TML_CORE_HANDLE;
                                           out iLogValue  : TML_INT32) : TML_INT32; cdecl;
  Ttml_Send_SyncMessage             = function(coreHandle : TML_CORE_HANDLE;
                                               cmdhandle  : TML_COMMAND_HANDLE;
                                               profile    : PSIDEX_STRING;
                                               sHost      : PSIDEX_STRING;
                                               sPort      : PSIDEX_STRING;
                                               iTimeout   : TML_UINT32) : TML_INT32; cdecl;

  Ttml_Send_AsyncMessage            = function(coreHandle : TML_CORE_HANDLE;
                                               cmdhandle  : TML_COMMAND_HANDLE;
                                               profile    : PSIDEX_STRING;
                                               sHost      : PSIDEX_STRING;
                                               sPort      : PSIDEX_STRING;
                                               iTimeout   : TML_UINT32) : TML_INT32; cdecl;
  Ttml_Send_AsyncProgressReply      = function(cmdhandle  : TML_COMMAND_HANDLE;
                                               progress   : TML_INT32) : TML_INT32; cdecl;
  Ttml_Send_AsyncStatusReply        = function(cmdhandle  : TML_COMMAND_HANDLE;
                                               iType      : TML_INT32;
                                               sStatus    : PSIDEX_STRING) : TML_INT32; cdecl;

  // using different thread context
  Ttml_Core_Thread_Set_OnCreate     = procedure(pCBCreate  : Pointer); cdecl;
  Ttml_Core_Thread_Set_OnDestroy    = procedure(pCBDestroy : Pointer); cdecl;
  Ttml_Sys_Free                     = procedure(aptr       : Pointer); cdecl;

  // eventIO
  Ttml_Evt_Subscribe_MessageDestination   = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sHost       : PSIDEX_STRING;
                                                     sPort       : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Evt_Unsubscribe_MessageDestination = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sHost       : PSIDEX_STRING;
                                                     sPort       : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Evt_Unsubscribe_All_MessageDestinations = function(coreHandle : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Evt_Get_Subscribed_MessageDestinations = function(coreHandle: TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                 var subscriptions: SIDEX_VARIANT): TML_INT32; cdecl;
  Ttml_Evt_Set_OnError                    = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnEventError;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Evt_Set_OnQueueOverflow            = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnEventQueueOverflow;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Evt_Set_OnPopulate                 = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnPopulateReceiver;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Evt_Send_Message                   = function(coreHandle  : TML_CORE_HANDLE;
                                                     cmdhandle   : TML_COMMAND_HANDLE;
                                                     profile     : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Evt_Send_SubscriptionRequest       = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sSourceHost : PSIDEX_STRING;
                                                     sSourcePort : PSIDEX_STRING;
                                                     sDestHost   : PSIDEX_STRING;
                                                     sDestPort   : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Evt_Send_UnsubscriptionRequest     = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sSourceHost : PSIDEX_STRING;
                                                     sSourcePort : PSIDEX_STRING;
                                                     sDestHost   : PSIDEX_STRING;
                                                     sDestPort   : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Evt_Set_MaxConnectionFailCount     = function(coreHandle  : TML_CORE_HANDLE;
                                                     iCount      : TML_UINT32): TML_INT32; cdecl;
  Ttml_Evt_Get_MaxConnectionFailCount     = function(coreHandle  : TML_CORE_HANDLE;
                                                     out iCount  : TML_UINT32): TML_INT32; cdecl;
  Ttml_Evt_Set_MaxQueuedEventMessages     = function(coreHandle  : TML_CORE_HANDLE;
                                                     iMaximum    : TML_UINT32): TML_INT32; cdecl;
  Ttml_Evt_Get_MaxQueuedEventMessages     = function(coreHandle  : TML_CORE_HANDLE;
                                                     out iMaximum: TML_UINT32): TML_INT32; cdecl;
  Ttml_Evt_Set_OnPeerRegister             = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnPeerRegister;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;

  // TML Load Balancing Interface
  Ttml_Bal_Subscribe_MessageDestination   = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sHost       : PSIDEX_STRING;
                                                     sPort       : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Bal_Unsubscribe_MessageDestination = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sHost       : PSIDEX_STRING;
                                                     sPort       : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Bal_Unsubscribe_All_MessageDestinations = function(coreHandle : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Bal_Get_Subscribed_MessageDestinations = function(coreHandle: TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                 var subscriptions: SIDEX_VARIANT): TML_INT32; cdecl;
  Ttml_Bal_Set_OnBusyStatusRequest        = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnBalBusyStatusRequest;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Bal_Set_OnPopulate                 = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnPopulateReceiver;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Bal_Set_OnCalculation              = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnBalCalculation;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Bal_Send_SyncMessage               = function(coreHandle  : TML_CORE_HANDLE;
                                                     cmdhandle   : TML_COMMAND_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Bal_Send_AsyncMessage              = function(coreHandle  : TML_CORE_HANDLE;
                                                     cmdhandle   : TML_COMMAND_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Bal_Send_SubscriptionRequest       = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sSourceHost : PSIDEX_STRING;
                                                     sSourcePort : PSIDEX_STRING;
                                                     sDestHost   : PSIDEX_STRING;
                                                     sDestPort   : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Bal_Send_UnsubscriptionRequest     = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     sSourceHost : PSIDEX_STRING;
                                                     sSourcePort : PSIDEX_STRING;
                                                     sDestHost   : PSIDEX_STRING;
                                                     sDestPort   : PSIDEX_STRING;
                                                     iTimeout    : TML_INT32) : TML_INT32; cdecl;
  Ttml_Bal_Set_MaxConnectionFailCount     = function(coreHandle  : TML_CORE_HANDLE;
                                                     iCount      : TML_UINT32): TML_INT32; cdecl;
  Ttml_Bal_Get_MaxConnectionFailCount     = function(coreHandle  : TML_CORE_HANDLE;
                                                     out iCount  : TML_UINT32): TML_INT32; cdecl;
  Ttml_Bal_Set_OnPeerRegister             = function(coreHandle  : TML_CORE_HANDLE;
                                                     profile     : PSIDEX_STRING;
                                                     pCBFunc     : TTML_OnPeerRegister;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;

  // TML Command interface
  Ttml_Cmd_Create                         = function(out tmlhandle : TML_COMMAND_HANDLE) : TML_INT32; cdecl;
  Ttml_Cmd_Free                           = function(var tmlhandle : TML_COMMAND_HANDLE) : TML_INT32; cdecl;
  Ttml_Cmd_Acquire_Sidex_Handle           = function(tmlhandle     : TML_COMMAND_HANDLE;
                                                     out shandle   : SIDEX_HANDLE) : TML_INT32; cdecl;
  Ttml_Cmd_Release_Sidex_Handle           = function(tmlhandle     : TML_COMMAND_HANDLE) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetCommand              = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out cmd   : TML_UINT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetCommand              = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     cmd       : TML_UINT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetCreationTime         = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out time  : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetCreationTime         = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     time      : PSIDEX_STRING) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetError                = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out error : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetError                = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     error     : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetErrorMessage         = function(tmlhandle      : TML_COMMAND_HANDLE;
                                                     out msg        : PSIDEX_STRING;
                                                     out iMsgLength : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetErrorMessage         = function(tmlhandle      : TML_COMMAND_HANDLE;
                                                     msg            : PSIDEX_STRING;
                                                     iMsgLength     : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetState                = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out state : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetState                = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     state     : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetMode                 = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out mode  : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetMode                 = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     mode      : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetReplyType            = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     out rtype : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetReplyType            = function(tmlhandle : TML_COMMAND_HANDLE;
                                                     rtype     : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Header_GetReplyMessage         = function(tmlhandle      : TML_COMMAND_HANDLE;
                                                     out msg        : PSIDEX_STRING;
                                                     out iMsgLength : TML_INT32) : TML_INT32; cdecl;
  Ttml_Cmd_Header_SetReplyMessage         = function(tmlhandle      : TML_COMMAND_HANDLE;
                                                     msg            : PSIDEX_STRING;
                                                     iMsgLength     : TML_INT32) : TML_INT32; cdecl;

  Ttml_Cmd_Register_Progress              = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     pCBFunc     : TTML_OnCmdProgress;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Cmd_Registered_Progress            = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     out pCBFunc : TTML_OnCmdProgress;
                                                     out pCBData : TML_POINTER) : TML_INT32; cdecl;

  Ttml_Cmd_Register_StatusReply           = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     pCBFunc     : TTML_OnCmdStatusRpy;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Cmd_Registered_StatusReply         = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     out pCBFunc : TTML_OnCmdStatusRpy;
                                                     out pCBData : TML_POINTER) : TML_INT32; cdecl;

  Ttml_Cmd_Register_CommandReady          = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     pCBFunc     : TTML_OnCmdReady;
                                                     pCBData     : TML_POINTER) : TML_INT32; cdecl;
  Ttml_Cmd_Registered_CommandReady        = function(tmlhandle   : TML_COMMAND_HANDLE;
                                                     out pCBFunc : TTML_OnCmdReady;
                                                     out pCBData : TML_POINTER) : TML_INT32; cdecl;

  // Stream Interface
  Ttml_RecStream_Open                     = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     profile       : PSIDEX_STRING;
                                                     sHost         : PSIDEX_STRING;
                                                     sPort         : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_RecStream_Close                    = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     bRetainOpen   : TML_BOOL) : TML_INT32; cdecl;
  Ttml_RecStream_DownloadData             = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     buffersize    : TML_UINT32;
                                                     pCBFuncDld    : TTML_DownloadCallback;
                                                     pCBDatadld    : TML_POINTER;
                                                     pCBFuncRet    : TTML_DownloadFinishedCallback;
                                                     pCBDataRet    : TML_POINTER) : TML_INT32; cdecl;
  Ttml_RecStream_GetPosition              = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     out rposition : TML_INT64) : TML_INT32; cdecl;
  Ttml_RecStream_GetSize                  = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     out rsize     : TML_INT64) : TML_INT32; cdecl;
  Ttml_RecStream_Read                     = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     buffer        : TML_POINTER;
                                                     count         : TML_INT32;
                                                     out bytesRead : TML_INT32) : TML_INT32; cdecl;
  Ttml_RecStream_ReadBuffer               = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     buffer        : TML_POINTER;
                                                     count         : TML_INT32) : TML_INT32; cdecl;
  Ttml_RecStream_Write                    = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     buffer        : TML_POINTER;
                                                     count         : TML_INT32) : TML_INT32; cdecl;
  Ttml_RecStream_Seek                     = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     seekPos       : TML_INT64;
                                                     origin        : TML_SEEK_ORIGIN) : TML_INT32; cdecl;

  Ttml_SndStream_Open                     = function(coreHandle    : TML_CORE_HANDLE;
                                                     out iStreamID : TML_STREAM_ID;
                                                     profile       : PSIDEX_STRING;
                                                     sHost         : PSIDEX_STRING;
                                                     sPort         : PSIDEX_STRING) : TML_INT32; cdecl;
  Ttml_SndStream_Close                    = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID) : TML_INT32; cdecl;
  Ttml_SndStream_Register_GetPosition     = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmGetPos;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_GetSize         = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmGetSize;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_Read            = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmRead;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_Write           = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmWrite;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_Seek            = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmSeek;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_Close           = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmClose;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;
  Ttml_SndStream_Register_OnError         = function(coreHandle    : TML_CORE_HANDLE;
                                                     iStreamID     : TML_STREAM_ID;
                                                     pCBFunc       : TTML_OnStrmError;
                                                     pCBData       : TML_POINTER) : TML_INT32; cdecl;

//------------------------------------------------------------------------------

var
  tml_Core_Get_Version                    : Ttml_Core_Get_Version                   = nil;
  tml_Core_Get_Copyright                  : Ttml_Core_Get_Copyright                 = nil;
  tml_Core_Get_Version_old                : Ttml_Core_Get_Version_old               = nil;
  tml_Core_Get_Version_Ext_old            : Ttml_Core_Get_Version_Ext_old           = nil;
  tml_Core_Set_Password                   : Ttml_Core_Set_Password                  = nil;
  tml_Configure_Thread_Pool_Handling      : Ttml_Configure_Thread_Pool_Handling     = nil;
  tml_Core_Open                           : Ttml_Core_Open                          = nil;
  tml_Core_Close                          : Ttml_Core_Close                         = nil;
  tml_Core_GeneralDeregistration          : Ttml_Core_GeneralDeregistration         = nil;
  tml_Profile_Register                    : Ttml_Profile_Register                   = nil;
  tml_Profile_Unregister                  : Ttml_Profile_Unregister                 = nil;
  tml_Profile_Get_Registered_Count        : Ttml_Profile_Get_Registered_Count       = nil;
  tml_Profile_Get_Registered              : Ttml_Profile_Get_Registered             = nil;
  tml_Profile_Get_RegisterState           : Ttml_Profile_Get_RegisterState          = nil;
  tml_Profile_Register_Cmd                : Ttml_Profile_Register_Cmd               = nil;
  tml_Profile_Set_OnDeleteCmd             : Ttml_Profile_Set_OnDeleteCmd            = nil;
  tml_Profile_Set_OnCustomDispatch        : Ttml_Profile_Set_OnCustomDispatch       = nil;
  tml_Core_Set_ListenerPort               : Ttml_Core_Set_ListenerPort              = nil;
  tml_Core_Get_ListenerPort               : Ttml_Core_Get_ListenerPort              = nil;
  tml_Core_Set_ListenerIP                 : Ttml_Core_Set_ListenerIP                = nil;
  tml_Core_Get_ListenerIP                 : Ttml_Core_Get_ListenerIP                = nil;
  tml_Core_Set_ListenerEnabled            : Ttml_Core_Set_ListenerEnabled           = nil;
  tml_Core_Get_ListenerEnabled            : Ttml_Core_Get_ListenerEnabled           = nil;
  tml_Core_Set_WindowSize                 : Ttml_Core_Set_WindowSize                = nil;
  tml_Core_Get_WindowSize                 : Ttml_Core_Get_WindowSize                = nil;
  tml_Core_Set_LoggingValue               : Ttml_Core_Set_LoggingValue              = nil;
  tml_Core_Get_LoggingValue               : Ttml_Core_Get_LoggingValue              = nil;
  tml_Send_SyncMessage                    : Ttml_Send_SyncMessage                   = nil;
  tml_Send_AsyncMessage                   : Ttml_Send_AsyncMessage                  = nil;
  tml_Send_AsyncProgressReply             : Ttml_Send_AsyncProgressReply            = nil;
  tml_Send_AsyncStatusReply               : Ttml_Send_AsyncStatusReply              = nil;

  // using different thread context
  tml_Core_Thread_Set_OnCreate            : Ttml_Core_Thread_Set_OnCreate           = nil;
  tml_Core_Thread_Set_OnDestroy           : Ttml_Core_Thread_Set_OnDestroy          = nil;
  tml_Sys_Free                            : Ttml_Sys_Free                           = nil;

  // TML Event Interface
  tml_Evt_Subscribe_MessageDestination    : Ttml_Evt_Subscribe_MessageDestination   = nil;
  tml_Evt_Unsubscribe_MessageDestination  : Ttml_Evt_Unsubscribe_MessageDestination = nil;
  tml_Evt_Unsubscribe_all_MessageDestinations : Ttml_Evt_Unsubscribe_All_MessageDestinations = nil;
  tml_Evt_Get_Subscribed_MessageDestinations  : Ttml_Evt_Get_Subscribed_MessageDestinations  = nil;
  tml_Evt_Set_OnError                     : Ttml_Evt_Set_OnError                    = nil;
  tml_Evt_Set_OnQueueOverflow             : Ttml_Evt_Set_OnQueueOverflow            = nil;
  tml_Evt_Set_OnPopulate                  : Ttml_Evt_Set_OnPopulate                 = nil;
  tml_Evt_Send_Message                    : Ttml_Evt_Send_Message                   = nil;
  tml_Evt_Send_SubscriptionRequest        : Ttml_Evt_Send_SubscriptionRequest       = nil;
  tml_Evt_Send_UnsubscriptionRequest      : Ttml_Evt_Send_UnsubscriptionRequest     = nil;
  tml_Evt_Set_MaxConnectionFailCount      : Ttml_Evt_Set_MaxConnectionFailCount     = nil;
  tml_Evt_Get_MaxConnectionFailCount      : Ttml_Evt_Get_MaxConnectionFailCount     = nil;
  tml_Evt_Set_MaxQueuedEventMessages      : Ttml_Evt_Set_MaxQueuedEventMessages     = nil;
  tml_Evt_Get_MaxQueuedEventMessages      : Ttml_Evt_Get_MaxQueuedEventMessages     = nil;
  tml_Evt_Set_OnPeerRegister              : Ttml_Evt_Set_OnPeerRegister             = nil;

  // TML Load Balancing Interface
  tml_Bal_Subscribe_MessageDestination    : Ttml_Bal_Subscribe_MessageDestination   = nil;
  tml_Bal_Unsubscribe_MessageDestination  : Ttml_Bal_Unsubscribe_MessageDestination = nil;
  tml_Bal_Unsubscribe_All_MessageDestinations : Ttml_Bal_Unsubscribe_All_MessageDestinations = nil;
  tml_Bal_Get_Subscribed_MessageDestinations  : Ttml_Bal_Get_Subscribed_MessageDestinations  = nil;
  tml_Bal_Set_OnBusyStatusRequest         : Ttml_Bal_Set_OnBusyStatusRequest        = nil;
  tml_Bal_Set_OnPopulate                  : Ttml_Bal_Set_OnPopulate                 = nil;
  tml_Bal_Set_OnCalculation               : Ttml_Bal_Set_OnCalculation              = nil;
  tml_Bal_Send_SyncMessage                : Ttml_Bal_Send_SyncMessage               = nil;
  tml_Bal_Send_AsyncMessage               : Ttml_Bal_Send_AsyncMessage              = nil;
  tml_Bal_Send_SubscriptionRequest        : Ttml_Bal_Send_SubscriptionRequest       = nil;
  tml_Bal_Send_UnsubscriptionRequest      : Ttml_Bal_Send_UnsubscriptionRequest     = nil;
  tml_Bal_Set_MaxConnectionFailCount      : Ttml_Bal_Set_MaxConnectionFailCount     = nil;
  tml_Bal_Get_MaxConnectionFailCount      : Ttml_Bal_Get_MaxConnectionFailCount     = nil;
  tml_Bal_Set_OnPeerRegister              : Ttml_Bal_Set_OnPeerRegister             = nil;

  // TML Command interface
  tml_Cmd_Create                            : Ttml_Cmd_Create                     = nil;
  tml_Cmd_Free                              : Ttml_Cmd_Free                       = nil;
  tml_Cmd_Acquire_Sidex_Handle              : Ttml_Cmd_Acquire_Sidex_Handle       = nil;
  tml_Cmd_Release_Sidex_Handle              : Ttml_Cmd_Release_Sidex_Handle       = nil;

  tml_Cmd_Header_GetCommand                 : Ttml_Cmd_Header_GetCommand          = nil;
  tml_Cmd_Header_SetCommand                 : Ttml_Cmd_Header_SetCommand          = nil;

  tml_Cmd_Header_GetCreationTime            : Ttml_Cmd_Header_GetCreationTime     = nil;
  tml_Cmd_Header_SetCreationTime            : Ttml_Cmd_Header_SetCreationTime     = nil;

  tml_Cmd_Header_GetError                   : Ttml_Cmd_Header_GetError            = nil;
  tml_Cmd_Header_SetError                   : Ttml_Cmd_Header_SetError            = nil;

  tml_Cmd_Header_GetErrorMessage            : Ttml_Cmd_Header_GetErrorMessage     = nil;
  tml_Cmd_Header_SetErrorMessage            : Ttml_Cmd_Header_SetErrorMessage     = nil;

  tml_Cmd_Header_GetState                   : Ttml_Cmd_Header_GetState            = nil;
  tml_Cmd_Header_SetState                   : Ttml_Cmd_Header_SetState            = nil;

  tml_Cmd_Header_GetMode                    : Ttml_Cmd_Header_GetMode             = nil;
  tml_Cmd_Header_SetMode                    : Ttml_Cmd_Header_SetMode             = nil;

  tml_Cmd_Header_GetReplyType               : Ttml_Cmd_Header_GetReplyType        = nil;
  tml_Cmd_Header_SetReplyType               : Ttml_Cmd_Header_SetReplyType        = nil;

  tml_Cmd_Header_GetReplyMessage            : Ttml_Cmd_Header_GetReplyMessage     = nil;
  tml_Cmd_Header_SetReplyMessage            : Ttml_Cmd_Header_SetReplyMessage     = nil;

  tml_Cmd_Register_Progress                 : Ttml_Cmd_Register_Progress          = nil;
  tml_Cmd_Registered_Progress               : Ttml_Cmd_Registered_Progress        = nil;

  tml_Cmd_Register_StatusReply              : Ttml_Cmd_Register_StatusReply       = nil;
  tml_Cmd_Registered_StatusReply            : Ttml_Cmd_Registered_StatusReply     = nil;

  tml_Cmd_Register_CommandReady             : Ttml_Cmd_Register_CommandReady      = nil;
  tml_Cmd_Registered_CommandReady           : Ttml_Cmd_Registered_CommandReady    = nil;

  // Streaming Interface
  tml_RecStream_Close                       : Ttml_RecStream_Close                = nil;
  tml_RecStream_DownloadData                : Ttml_RecStream_DownloadData         = nil;
  tml_RecStream_GetPosition                 : Ttml_RecStream_GetPosition          = nil;
  tml_RecStream_GetSize                     : Ttml_RecStream_GetSize              = nil;
  tml_RecStream_Open                        : Ttml_RecStream_Open                 = nil;
  tml_RecStream_Read                        : Ttml_RecStream_Read                 = nil;
  tml_RecStream_ReadBuffer                  : Ttml_RecStream_ReadBuffer           = nil;
  tml_RecStream_Write                       : Ttml_RecStream_Write                = nil;
  tml_RecStream_Seek                        : Ttml_RecStream_Seek                 = nil;
  tml_SndStream_Close                       : Ttml_SndStream_Close                = nil;
  tml_SndStream_Open                        : Ttml_SndStream_Open                 = nil;

  tml_SndStream_Register_GetPosition        : Ttml_SndStream_Register_GetPosition = nil;
  tml_SndStream_Register_GetSize            : Ttml_SndStream_Register_GetSize     = nil;
  tml_SndStream_Register_Read               : Ttml_SndStream_Register_Read        = nil;
  tml_SndStream_Register_Write              : Ttml_SndStream_Register_Write       = nil;
  tml_SndStream_Register_Seek               : Ttml_SndStream_Register_Seek        = nil;
  tml_SndStream_Register_Close              : Ttml_SndStream_Register_Close       = nil;
  tml_SndStream_Register_OnError            : Ttml_SndStream_Register_OnError     = nil;

//------------------------------------------------------------------------------

type
  TtmlDesignInfo = record
    ListenerIP      : string;
    ListenerPort    : string;
    ListenerEnabled : Boolean;
    MaxEvtFailCount : Cardinal;
    MaxQueuedEvents : Cardinal;
    MaxBalFailCount : Cardinal;
    LogLevel        : TML_INT32;
    WindowSize      : TML_INT32;
  end;

//------------------------------------------------------------------------------

var
  tmlUseExceptions   : Boolean;

  tmlDesignCopyright : string;
  tmlDesignVersion   : string;

  tmlDesignInfos     : Array of TtmlDesignInfo;

//------------------------------------------------------------------------------

procedure tml_init;
function  tml_loaded    : Boolean;

function  tml_Copyright : string;
function  tml_Version   : string;

//------------------------------------------------------------------------------

implementation

uses
  {$if defined(Unix)}
    dl,
  {$elseif defined(ANDROID)}
    Posix.Dlfcn,
  {$ifend}
  {$if defined(FPC)}
    LCLType,
  {$ifend}
  {$if defined(USE_NAMESPACES)}
    {$if defined(ANDROID)}
      FMX.Dialogs,
      FMX.Forms,
    {$else}
      Vcl.Dialogs,
      Vcl.Forms,
    {$ifend}
    System.UITypes;
  {$else}
    Dialogs,
    Forms;
  {$ifend}

//------------------------------------------------------------------------------

{$if not defined(ANDROID)}
const
  // keep the same list in uSidexLib
  tmlExeDesigners : array[0..2] of string = ( 'bds.exe',
                                              'lazarus.exe',
                                              'lazarus' );
{$ifend}

var
  TMLCore_dllHandle : TTML_CORE_DLLHANDLE;
  tmlLoaded         : Boolean;
  tmlDesigning      : Boolean;

  {$if defined(ANDROID)}
    sidex_dllHandle_Vortex : TSIDEX_DLLHANDLE;
  {$else}
    tmlLoopIdx     : Integer;
    tmlExeFileName : string;
  {$ifend}

//------------------------------------------------------------------------------

function UnloadDll_TML: Boolean;
begin
  if TMLCore_dllHandle <> TML_CORE_DLLHANDLE_NULL then
  begin
    {$if defined(Unix)}
      Result := (dlclose(TMLCore_dllHandle) = 0);
    {$elseif defined(FPC)}
      Result := UnloadLibrary(TMLCore_dllHandle);
    {$else}
      Result := FreeLibrary(TMLCore_dllHandle);
    {$ifend}
    TMLCore_dllHandle := TML_CORE_DLLHANDLE_NULL;

    {$if defined(ANDROID)}
      if sidex_dllHandle_Vortex <> SIDEX_DLLHANDLE_NULL then
      begin
        FreeLibrary(sidex_dllHandle_Vortex);
        sidex_dllHandle_Vortex := SIDEX_DLLHANDLE_NULL;
      end;
    {$ifend}
  end
  else Result := true;
end;

function LoadDll_TML(adllname : string): Boolean;
var
  errMsg: string;
begin
  UnloadDll_TML;
  {$if defined(Unix)}
    TMLCore_dllHandle := dlopen(PChar(adllname), RTLD_LAZY);
  {$else}
    {$if defined(ANDROID)}
      sidex_dllHandle_Vortex := LoadLibrary(PChar(TPath.Combine(TPath.GetLibraryPath, SidexVortexLibName)));
      adllname               := TPath.Combine(TPath.GetLibraryPath, adllname);
    {$ifend}
    TMLCore_dllHandle := LoadLibrary(PChar(adllname));
  {$ifend}
  Result := (TMLCore_dllHandle <> TML_CORE_DLLHANDLE_NULL);
  if not Result then
  begin
    if tmlDesigning then
    begin
      tmlDesignCopyright := 'TML-DLL not found!';
      tmlDesignVersion   := tmlDesignCopyright;
    end
    else if tmlUseExceptions then
    begin
      {$if defined(Unix) or defined(ANDROID)}
        errMsg := #13 + string(dlerror());
      {$else}
        errMsg := '';
      {$ifend}
      raise Exception.Create('cannot load ' + adllname + errMsg);
    end;
  end;
end;

function ImportDllFunction(dllhandle : TTML_CORE_DLLHANDLE; funcname : string): Pointer;
var
  E: EDLLImportError;
  errMsg: string;
begin
  {$if defined(Unix)}
    Result := dlsym(dllhandle, PChar(funcname));
  {$else}
    Result := GetProcAddress(dllhandle, PChar(funcname));
  {$ifend}
  if (Result = nil) and not tmlDesigning then
  begin
    {$if defined(Unix) or defined(ANDROID)}
      errMsg := #13 + string(dlerror());
    {$else}
      errMsg := '';
    {$ifend}
    E := EDllImportError.CreateFmt('could not find dll function "%s"%s',
                                   [ funcname, errMsg ]);
    E.WrongFunc := funcname;
    raise E;
  end;
end;

function LoadFunctions_Info_TML(dllhandle : TTML_CORE_DLLHANDLE;
                                useExceptions : Boolean = false): Boolean;
begin
  Result := false;
  try
    try
      tml_Core_Get_Version := Ttml_Core_Get_Version(ImportDllFunction(dllhandle,'tml_Core_Get_Version'+tmlStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        try
          tml_Core_Get_Version_old := Ttml_Core_Get_Version_old(ImportDllFunction(dllhandle,'tml_Core_Get_Version'));
          try
            tml_Core_Get_Version_Ext_old := Ttml_Core_Get_Version_Ext_old(ImportDllFunction(dllhandle,'tml_Core_Get_Version_Ext'));
          except
            on E: EDLLImportError do
            begin
              tml_Core_Get_Version_Ext_old := nil;
            end;
          end;
        except
          on E: EDLLImportError do
          begin
            tml_Core_Get_Version_old     := nil;
            tml_Core_Get_Version_Ext_old := nil;
          end;
        end;
        if not Assigned(tml_Core_Get_Version_old) then raise;
      end;
    end;
    if tmlDesigning then
    begin
      if not Assigned(tml_Core_Get_Version) then
      begin
        tmlDesignVersion := 'Error: DLL function "tml_Core_Get_Version' +
                            tmlStringTypeSuffix + '" not found!';
      end
      else tmlDesignVersion := tml_Version();
    end;

    try
      tml_Core_Get_Copyright := Ttml_Core_Get_Copyright(ImportDllFunction(dllhandle,'tml_Core_Get_Copyright'+tmlStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        tml_Core_Get_Copyright := nil;
      end;
    end;
    if tmlDesigning then
    begin
      if not Assigned(tml_Core_Get_Copyright) then
      begin
        tmlDesignCopyright := 'Error: DLL function "tml_Core_Get_Copyright' +
                              tmlStringTypeSuffix + '" not found!';
      end
      else tmlDesignCopyright := tml_Copyright();
    end;

    Result := true;
  except
    on E: EDLLImportError do
    begin
      if useExceptions then raise;
    end;
  end;
end;

function LoadFunctions_TML(dllhandle : TTML_CORE_DLLHANDLE): Boolean;
begin
  Result := false;
  try
    tml_Core_Set_Password                     := Ttml_Core_Set_Password(ImportDllFunction(dllhandle,'tml_Core_Set_Password'+tmlStringTypeSuffix));
    tml_Configure_Thread_Pool_Handling        := Ttml_Configure_Thread_Pool_Handling(ImportDllFunction(dllhandle,'tml_Configure_Thread_Pool_Handling'));
    tml_Core_Open                             := Ttml_Core_Open(ImportDllFunction(dllhandle,'tml_Core_Open'));
    tml_Core_Close                            := Ttml_Core_Close(ImportDllFunction(dllhandle,'tml_Core_Close'));
    tml_Core_GeneralDeregistration            := Ttml_Core_GeneralDeregistration(ImportDllFunction(dllhandle,'tml_Core_GeneralDeregistration'));
    tml_Profile_Register                      := Ttml_Profile_Register(ImportDllFunction(dllhandle,'tml_Profile_Register'+tmlStringTypeSuffix));
    tml_Profile_Unregister                    := Ttml_Profile_Unregister(ImportDllFunction(dllhandle,'tml_Profile_Unregister'+tmlStringTypeSuffix));
    tml_Profile_Get_Registered_Count          := Ttml_Profile_Get_Registered_Count(ImportDllFunction(dllhandle,'tml_Profile_Get_Registered_Count'));
    tml_Profile_Get_Registered                := Ttml_Profile_Get_Registered(ImportDllFunction(dllhandle,'tml_Profile_Get_Registered'));
    tml_Profile_Get_RegisterState             := Ttml_Profile_Get_RegisterState(ImportDllFunction(dllhandle,'tml_Profile_Get_RegisterState'+tmlStringTypeSuffix));

    tml_Profile_Register_Cmd                  := Ttml_Profile_Register_Cmd(ImportDllFunction(dllhandle,'tml_Profile_Register_Cmd'+tmlStringTypeSuffix));
    tml_Profile_Set_OnDeleteCmd               := Ttml_Profile_Set_OnDeleteCmd(ImportDllFunction(dllhandle,'tml_Profile_Set_OnDeleteCmd'+tmlStringTypeSuffix));
    tml_Profile_Set_OnCustomDispatch          := Ttml_Profile_Set_OnCustomDispatch(ImportDllFunction(dllhandle,'tml_Profile_Set_OnCustomDispatch'+tmlStringTypeSuffix));
    tml_Core_Set_ListenerPort                 := Ttml_Core_Set_ListenerPort(ImportDllFunction(dllhandle,'tml_Core_Set_ListenerPort'+tmlStringTypeSuffix));
    tml_Core_Get_ListenerPort                 := Ttml_Core_Get_ListenerPort(ImportDllFunction(dllhandle,'tml_Core_Get_ListenerPort'+tmlStringTypeSuffix));
    tml_Core_Set_ListenerIP                   := Ttml_Core_Set_ListenerIP(ImportDllFunction(dllhandle,'tml_Core_Set_ListenerIP'+tmlStringTypeSuffix));
    tml_Core_Get_ListenerIP                   := Ttml_Core_Get_ListenerIP(ImportDllFunction(dllhandle,'tml_Core_Get_ListenerIP'+tmlStringTypeSuffix));

    tml_Core_Set_ListenerEnabled              := Ttml_Core_Set_ListenerEnabled(ImportDllFunction(dllhandle,'tml_Core_Set_ListenerEnabled'));
    tml_Core_Get_ListenerEnabled              := Ttml_Core_Get_ListenerEnabled(ImportDllFunction(dllhandle,'tml_Core_Get_ListenerEnabled'));

    tml_Core_Set_WindowSize                   := Ttml_Core_Set_WindowSize(ImportDllFunction(dllhandle,'tml_Core_Set_WindowSize'));
    tml_Core_Get_WindowSize                   := Ttml_Core_Get_WindowSize(ImportDllFunction(dllhandle,'tml_Core_Get_WindowSize'));
    tml_Core_Set_LoggingValue                 := Ttml_Core_Set_LoggingValue(ImportDllFunction(dllhandle,'tml_Core_Set_LoggingValue'));
    tml_Core_Get_LoggingValue                 := Ttml_Core_Get_LoggingValue(ImportDllFunction(dllhandle,'tml_Core_Get_LoggingValue'));
    tml_Send_SyncMessage                      := Ttml_Send_SyncMessage(ImportDllFunction(dllhandle,'tml_Send_SyncMessage'+tmlStringTypeSuffix));
    tml_Send_AsyncMessage                     := Ttml_Send_AsyncMessage(ImportDllFunction(dllhandle,'tml_Send_AsyncMessage'+tmlStringTypeSuffix));
    tml_Send_AsyncProgressReply               := Ttml_Send_AsyncProgressReply(ImportDllFunction(dllhandle,'tml_Send_AsyncProgressReply'));
    tml_Send_AsyncStatusReply                 := Ttml_Send_AsyncStatusReply(ImportDllFunction(dllhandle,'tml_Send_AsyncStatusReply'+tmlStringTypeSuffix));

    // using different thread context
    tml_Core_Thread_Set_OnCreate              := Ttml_Core_Thread_Set_OnCreate(ImportDllFunction(dllhandle,'tml_Core_Thread_Set_OnCreate'));
    tml_Core_Thread_Set_OnDestroy             := Ttml_Core_Thread_Set_OnDestroy(ImportDllFunction(dllhandle,'tml_Core_Thread_Set_OnDestroy'));
    tml_Sys_Free                              := Ttml_Sys_Free(ImportDllFunction(dllhandle,'tml_Sys_Free'));

    // TML Event Interface
    tml_Evt_Subscribe_MessageDestination      := Ttml_Evt_Subscribe_MessageDestination (ImportDllFunction(dllhandle,'tml_Evt_Subscribe_MessageDestination'+tmlStringTypeSuffix));
    tml_Evt_Unsubscribe_MessageDestination    := Ttml_Evt_Unsubscribe_MessageDestination(ImportDllFunction(dllhandle,'tml_Evt_Unsubscribe_MessageDestination'+tmlStringTypeSuffix));
    tml_Evt_Unsubscribe_All_MessageDestinations := Ttml_Evt_Unsubscribe_All_MessageDestinations(ImportDllFunction(dllhandle,'tml_Evt_Unsubscribe_All_MessageDestinations'+tmlStringTypeSuffix));
    tml_Evt_Get_Subscribed_MessageDestinations  := Ttml_Evt_Get_Subscribed_MessageDestinations(ImportDllFunction(dllhandle,'tml_Evt_Get_Subscribed_MessageDestinations'+tmlStringTypeSuffix));
    try
      tml_Evt_Set_OnError                     := Ttml_Evt_Set_OnError(ImportDllFunction(dllhandle,'tml_Evt_Set_OnError'+tmlStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        tml_Evt_Set_OnError := nil;
      end;
    end;
    tml_Evt_Set_OnQueueOverflow               := Ttml_Evt_Set_OnQueueOverflow(ImportDllFunction(dllhandle,'tml_Evt_Set_OnQueueOverflow'+tmlStringTypeSuffix));
    tml_Evt_Set_OnPopulate                    := Ttml_Evt_Set_OnPopulate(ImportDllFunction(dllhandle,'tml_Evt_Set_OnPopulate'+tmlStringTypeSuffix));
    tml_Evt_Send_Message                      := Ttml_Evt_Send_Message(ImportDllFunction(dllhandle,'tml_Evt_Send_Message'+tmlStringTypeSuffix));

    tml_Evt_Send_SubscriptionRequest          := Ttml_Evt_Send_SubscriptionRequest(ImportDllFunction(dllhandle,'tml_Evt_Send_SubscriptionRequest'+tmlStringTypeSuffix));
    tml_Evt_Send_UnsubscriptionRequest        := Ttml_Evt_Send_UnsubscriptionRequest(ImportDllFunction(dllhandle,'tml_Evt_Send_UnsubscriptionRequest'+tmlStringTypeSuffix));
    tml_Evt_Set_MaxConnectionFailCount        := Ttml_Evt_Set_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Evt_Set_MaxConnectionFailCount'));
    tml_Evt_Get_MaxConnectionFailCount        := Ttml_Evt_Get_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Evt_Get_MaxConnectionFailCount'));
    tml_Evt_Set_MaxQueuedEventMessages        := Ttml_Evt_Set_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Evt_Set_MaxQueuedEventMessages'));
    tml_Evt_Get_MaxQueuedEventMessages        := Ttml_Evt_Get_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Evt_Get_MaxQueuedEventMessages'));
    tml_Evt_Set_OnPeerRegister                := Ttml_Evt_Set_OnPeerRegister(ImportDllFunction(dllhandle,'tml_Evt_Set_OnPeerRegister'+tmlStringTypeSuffix));

    // TML Load Balancing Interface
    tml_Bal_Subscribe_MessageDestination      := Ttml_Bal_Subscribe_MessageDestination(ImportDllFunction(dllhandle,'tml_Bal_Subscribe_MessageDestination'+tmlStringTypeSuffix));
    tml_Bal_Unsubscribe_MessageDestination    := Ttml_Bal_Unsubscribe_MessageDestination(ImportDllFunction(dllhandle,'tml_Bal_Unsubscribe_MessageDestination'+tmlStringTypeSuffix));
    tml_Bal_Unsubscribe_All_MessageDestinations := Ttml_Bal_Unsubscribe_All_MessageDestinations(ImportDllFunction(dllhandle,'tml_Bal_Unsubscribe_All_MessageDestinations'+tmlStringTypeSuffix));
    tml_Bal_Get_Subscribed_MessageDestinations  := Ttml_Bal_Get_Subscribed_MessageDestinations(ImportDllFunction(dllhandle,'tml_Bal_Get_Subscribed_MessageDestinations'+tmlStringTypeSuffix));
    tml_Bal_Set_OnBusyStatusRequest           := Ttml_Bal_Set_OnBusyStatusRequest(ImportDllFunction(dllhandle,'tml_Bal_Set_OnBusyStatusRequest'+tmlStringTypeSuffix));
    tml_Bal_Set_OnPopulate                    := Ttml_Bal_Set_OnPopulate(ImportDllFunction(dllhandle,'tml_Bal_Set_OnPopulate'+tmlStringTypeSuffix));
    tml_Bal_Set_OnCalculation                 := Ttml_Bal_Set_OnCalculation(ImportDllFunction(dllhandle,'tml_Bal_Set_OnCalculation'+tmlStringTypeSuffix));
    tml_Bal_Send_SyncMessage                  := Ttml_Bal_Send_SyncMessage(ImportDllFunction(dllhandle,'tml_Bal_Send_SyncMessage'+tmlStringTypeSuffix));
    tml_Bal_Send_AsyncMessage                 := Ttml_Bal_Send_AsyncMessage(ImportDllFunction(dllhandle,'tml_Bal_Send_AsyncMessage'+tmlStringTypeSuffix));
    tml_Bal_Send_SubscriptionRequest          := Ttml_Bal_Send_SubscriptionRequest(ImportDllFunction(dllhandle,'tml_Bal_Send_SubscriptionRequest'+tmlStringTypeSuffix));
    tml_Bal_Send_UnsubscriptionRequest        := Ttml_Bal_Send_UnsubscriptionRequest(ImportDllFunction(dllhandle,'tml_Bal_Send_UnsubscriptionRequest'+tmlStringTypeSuffix));
    tml_Bal_Set_MaxConnectionFailCount        := Ttml_Bal_Set_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Bal_Set_MaxConnectionFailCount'));
    tml_Bal_Get_MaxConnectionFailCount        := Ttml_Bal_Get_MaxConnectionFailCount(ImportDllFunction(dllhandle,'tml_Bal_Get_MaxConnectionFailCount'));
    tml_Bal_Set_OnPeerRegister                := Ttml_Bal_Set_OnPeerRegister(ImportDllFunction(dllhandle,'tml_Bal_Set_OnPeerRegister'+tmlStringTypeSuffix));

    // TML Command interface
    tml_Cmd_Free                              := Ttml_Cmd_Free(ImportDllFunction(dllhandle,'tml_Cmd_Free'));
    tml_Cmd_Create                            := Ttml_Cmd_Create(ImportDllFunction(dllhandle,'tml_Cmd_Create'));
    tml_Cmd_Acquire_Sidex_Handle              := Ttml_Cmd_Acquire_Sidex_Handle(ImportDllFunction(dllhandle,'tml_Cmd_Acquire_Sidex_Handle'));
    tml_Cmd_Release_Sidex_Handle              := Ttml_Cmd_Release_Sidex_Handle(ImportDllFunction(dllhandle,'tml_Cmd_Release_Sidex_Handle'));
    tml_Cmd_Header_GetCommand                 := Ttml_Cmd_Header_GetCommand(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetCommand'));
    tml_Cmd_Header_SetCommand                 := Ttml_Cmd_Header_SetCommand(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetCommand'));
    tml_Cmd_Header_GetCreationTime            := Ttml_Cmd_Header_GetCreationTime(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetCreationTime'+tmlStringTypeSuffix));
    tml_Cmd_Header_SetCreationTime            := Ttml_Cmd_Header_SetCreationTime(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetCreationTime'+tmlStringTypeSuffix));
    tml_Cmd_Header_GetError                   := Ttml_Cmd_Header_GetError(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetError'));
    tml_Cmd_Header_SetError                   := Ttml_Cmd_Header_SetError(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetError'));
    tml_Cmd_Header_GetErrorMessage            := Ttml_Cmd_Header_GetErrorMessage(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetErrorMessage'+tmlStringTypeSuffix));
    tml_Cmd_Header_SetErrorMessage            := Ttml_Cmd_Header_SetErrorMessage(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetErrorMessage'+tmlStringTypeSuffix));
    tml_Cmd_Header_GetState                   := Ttml_Cmd_Header_GetState(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetState'));
    tml_Cmd_Header_SetState                   := Ttml_Cmd_Header_SetState(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetState'));
    tml_Cmd_Header_GetMode                    := Ttml_Cmd_Header_GetMode(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetMode'));
    tml_Cmd_Header_SetMode                    := Ttml_Cmd_Header_SetMode(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetMode'));
    tml_Cmd_Header_GetReplyType               := Ttml_Cmd_Header_GetReplyType(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetReplyType'));
    tml_Cmd_Header_SetReplyType               := Ttml_Cmd_Header_SetReplyType(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetReplyType'));
    tml_Cmd_Header_GetReplyMessage            := Ttml_Cmd_Header_GetReplyMessage(ImportDllFunction(dllhandle,'tml_Cmd_Header_GetReplyMessage'+tmlStringTypeSuffix));
    tml_Cmd_Header_SetReplyMessage            := Ttml_Cmd_Header_SetReplyMessage(ImportDllFunction(dllhandle,'tml_Cmd_Header_SetReplyMessage'+tmlStringTypeSuffix));
    tml_Cmd_Register_Progress                 := Ttml_Cmd_Register_Progress(ImportDllFunction(dllhandle,'tml_Cmd_Register_Progress'));
    tml_Cmd_Registered_Progress               := Ttml_Cmd_Registered_Progress(ImportDllFunction(dllhandle,'tml_Cmd_Registered_Progress'));
    tml_Cmd_Register_StatusReply              := Ttml_Cmd_Register_StatusReply(ImportDllFunction(dllhandle,'tml_Cmd_Register_StatusReply'+tmlStringTypeSuffix));
    tml_Cmd_Registered_StatusReply            := Ttml_Cmd_Registered_StatusReply(ImportDllFunction(dllhandle,'tml_Cmd_Registered_StatusReply'));
    tml_Cmd_Register_CommandReady             := Ttml_Cmd_Register_CommandReady(ImportDllFunction(dllhandle,'tml_Cmd_Register_CommandReady'));
    tml_Cmd_Registered_CommandReady           := Ttml_Cmd_Registered_CommandReady(ImportDllFunction(dllhandle,'tml_Cmd_Registered_CommandReady'));

    // Streaming Interface
    tml_RecStream_Open                        := Ttml_RecStream_Open(ImportDllFunction(dllhandle,'tml_RecStream_Open'+tmlStringTypeSuffix));
    tml_RecStream_Close                       := Ttml_RecStream_Close(ImportDllFunction(dllhandle,'tml_RecStream_Close'));
    tml_RecStream_GetPosition                 := Ttml_RecStream_GetPosition(ImportDllFunction(dllhandle,'tml_RecStream_GetPosition'));
    tml_RecStream_GetSize                     := Ttml_RecStream_GetSize(ImportDllFunction(dllhandle,'tml_RecStream_GetSize'));
    tml_RecStream_Read                        := Ttml_RecStream_Read(ImportDllFunction(dllhandle,'tml_RecStream_Read'));
    tml_RecStream_ReadBuffer                  := Ttml_RecStream_ReadBuffer(ImportDllFunction(dllhandle,'tml_RecStream_ReadBuffer'));
    tml_RecStream_Write                       := Ttml_RecStream_Write(ImportDllFunction(dllhandle,'tml_RecStream_Write'));
    tml_RecStream_Seek                        := Ttml_RecStream_Seek(ImportDllFunction(dllhandle,'tml_RecStream_Seek'));
    tml_RecStream_DownloadData                := Ttml_RecStream_DownloadData(ImportDllFunction(dllhandle,'tml_RecStream_DownloadData'));

    tml_SndStream_Open                        := Ttml_SndStream_Open(ImportDllFunction(dllhandle,'tml_SndStream_Open'+tmlStringTypeSuffix));
    tml_SndStream_Close                       := Ttml_SndStream_Close(ImportDllFunction(dllhandle,'tml_SndStream_Close'));
    tml_SndStream_Register_GetPosition        := Ttml_SndStream_Register_GetPosition(ImportDllFunction(dllhandle,'tml_SndStream_Register_GetPosition'));
    tml_SndStream_Register_GetSize            := Ttml_SndStream_Register_GetSize(ImportDllFunction(dllhandle,'tml_SndStream_Register_GetSize'));
    tml_SndStream_Register_Read               := Ttml_SndStream_Register_Read(ImportDllFunction(dllhandle,'tml_SndStream_Register_Read'));
    tml_SndStream_Register_Write              := Ttml_SndStream_Register_Write(ImportDllFunction(dllhandle,'tml_SndStream_Register_Write'));
    tml_SndStream_Register_Seek               := Ttml_SndStream_Register_Seek(ImportDllFunction(dllhandle,'tml_SndStream_Register_Seek'));
    tml_SndStream_Register_Close              := Ttml_SndStream_Register_Close(ImportDllFunction(dllhandle,'tml_SndStream_Register_Close'));
    try
      tml_SndStream_Register_OnError          := Ttml_SndStream_Register_OnError(ImportDllFunction(dllhandle,'tml_SndStream_Register_OnError'));
    except
      on E: EDLLImportError do
      begin
        tml_SndStream_Register_OnError := nil;
      end;
    end;

    Result := LoadFunctions_Info_TML(dllhandle, tmlUseExceptions);
  except
    on E: EDLLImportError do
    begin
      if tmlUseExceptions then raise;
    end;
  end;
end;

procedure UnloadFunctions_TML;
begin
  tml_Core_Get_Version                        := nil;
  tml_Core_Get_Copyright                      := nil;
  tml_Core_Get_Version_old                    := nil;
  tml_Core_Get_Version_Ext_old                := nil;
  tml_Core_Set_Password                       := nil;
  tml_Configure_Thread_Pool_Handling          := nil;
  tml_Core_Open                               := nil;
  tml_Core_Close                              := nil;
  tml_Core_GeneralDeregistration              := nil;
  tml_Profile_Register                        := nil;
  tml_Profile_Unregister                      := nil;
  tml_Profile_Get_Registered_Count            := nil;
  tml_Profile_Get_Registered                  := nil;
  tml_Profile_Get_RegisterState               := nil;

  tml_Profile_Register_Cmd                    := nil;
  tml_Profile_Set_OnDeleteCmd                 := nil;
  tml_Profile_Set_OnCustomDispatch            := nil;
  tml_Core_Set_ListenerPort                   := nil;
  tml_Core_Get_ListenerPort                   := nil;
  tml_Core_Set_ListenerIP                     := nil;
  tml_Core_Get_ListenerIP                     := nil;

  tml_Core_Set_ListenerEnabled                := nil;
  tml_Core_Get_ListenerEnabled                := nil;

  tml_Core_Set_WindowSize                     := nil;
  tml_Core_Get_WindowSize                     := nil;
  tml_Core_Set_LoggingValue                   := nil;
  tml_Core_Get_LoggingValue                   := nil;
  tml_Send_SyncMessage                        := nil;
  tml_Send_AsyncMessage                       := nil;
  tml_Send_AsyncProgressReply                 := nil;
  tml_Send_AsyncStatusReply                   := nil;

  tml_Evt_Subscribe_MessageDestination        := nil;
  tml_Evt_Unsubscribe_MessageDestination      := nil;
  tml_Evt_Unsubscribe_All_MessageDestinations := nil;
  tml_Evt_Get_Subscribed_MessageDestinations  := nil;
  tml_Evt_Set_OnError                         := nil;
  tml_Evt_Set_OnQueueOverflow                 := nil;
  tml_Evt_Set_OnPopulate                      := nil;
  tml_Evt_Send_Message                        := nil;

  tml_Evt_Send_SubscriptionRequest            := nil;
  tml_Evt_Send_UnsubscriptionRequest          := nil;
  tml_Evt_Set_MaxConnectionFailCount          := nil;
  tml_Evt_Get_MaxConnectionFailCount          := nil;
  tml_Evt_Set_MaxQueuedEventMessages          := nil;
  tml_Evt_Get_MaxQueuedEventMessages          := nil;
  tml_Evt_Set_OnPeerRegister                  := nil;

  tml_Bal_Subscribe_MessageDestination        := nil;
  tml_Bal_Unsubscribe_MessageDestination      := nil;
  tml_Bal_Unsubscribe_All_MessageDestinations := nil;
  tml_Bal_Get_Subscribed_MessageDestinations  := nil;
  tml_Bal_Set_OnBusyStatusRequest             := nil;
  tml_Bal_Set_OnPopulate                      := nil;
  tml_Bal_Set_OnCalculation                   := nil;
  tml_Bal_Send_SyncMessage                    := nil;
  tml_Bal_Send_AsyncMessage                   := nil;
  tml_Bal_Send_SubscriptionRequest            := nil;
  tml_Bal_Send_UnsubscriptionRequest          := nil;
  tml_Bal_Set_MaxConnectionFailCount          := nil;
  tml_Bal_Get_MaxConnectionFailCount          := nil;
  tml_Bal_Set_OnPeerRegister                  := nil;

  tml_Cmd_Free                                := nil;
  tml_Cmd_Create                              := nil;
  tml_Cmd_Acquire_Sidex_Handle                := nil;
  tml_Cmd_Release_Sidex_Handle                := nil;
  tml_Cmd_Header_GetCommand                   := nil;
  tml_Cmd_Header_SetCommand                   := nil;

  tml_Cmd_Header_GetCreationTime              := nil;
  tml_Cmd_Header_SetCreationTime              := nil;

  tml_Cmd_Header_GetError                     := nil;
  tml_Cmd_Header_SetError                     := nil;

  tml_Cmd_Header_GetErrorMessage              := nil;
  tml_Cmd_Header_SetErrorMessage              := nil;

  tml_Cmd_Header_GetState                     := nil;
  tml_Cmd_Header_SetState                     := nil;

  tml_Cmd_Header_GetMode                      := nil;
  tml_Cmd_Header_SetMode                      := nil;

  tml_Cmd_Header_GetReplyType                 := nil;
  tml_Cmd_Header_SetReplyType                 := nil;

  tml_Cmd_Header_GetReplyMessage              := nil;
  tml_Cmd_Header_SetReplyMessage              := nil;

  tml_Cmd_Register_Progress                   := nil;
  tml_Cmd_Registered_Progress                 := nil;

  tml_Cmd_Register_StatusReply                := nil;
  tml_Cmd_Registered_StatusReply              := nil;

  tml_Cmd_Register_CommandReady               := nil;
  tml_Cmd_Registered_StatusReply              := nil;

  tml_RecStream_Close                         := nil;
  tml_RecStream_DownloadData                  := nil;
  tml_RecStream_GetPosition                   := nil;
  tml_RecStream_GetSize                       := nil;
  tml_RecStream_Open                          := nil;
  tml_RecStream_Read                          := nil;
  tml_RecStream_ReadBuffer                    := nil;
  tml_RecStream_Write                         := nil;
  tml_RecStream_Seek                          := nil;
  tml_SndStream_Close                         := nil;
  tml_SndStream_Open                          := nil;

  tml_SndStream_Register_GetPosition          := nil;
  tml_SndStream_Register_GetSize              := nil;
  tml_SndStream_Register_Read                 := nil;
  tml_SndStream_Register_Write                := nil;
  tml_SndStream_Register_Seek                 := nil;
  tml_SndStream_Register_Close                := nil;
  tml_SndStream_Register_OnError              := nil;

  tml_Core_Thread_Set_OnCreate                := nil;
  tml_Core_Thread_Set_OnDestroy               := nil;
  tml_Sys_Free                                := nil;
end;

function LoadTML: Boolean;
begin
  if not tmlLoaded then
  begin
    if LoadDll_TML(tmlcoredllname) then
    begin
      if tmlDesigning then
      begin
        LoadFunctions_Info_TML(TMLCore_dllHandle, false);
      end
      else
      begin
        tmlLoaded := LoadFunctions_TML(TMLCore_dllHandle);
      end;
    end;
  end;
  Result := tmlLoaded;
end;

function UnloadTML: Boolean;
begin
  UnloadFunctions_TML;
  Result := UnloadDll_TML();
  tmlLoaded := false;
end;

//----------------------------------------------
// show a message and terminate the application
//----------------------------------------------
procedure InitError(amsg : string);
begin
  {$if defined(CONSOLE)}
    try
      WriteLn('TML: library error: ' + tmlcoredllname);
      WriteLn(amsg);
    except
      // ignore
    end;
    Sleep(3000);
  {$else}
    MessageDlg('TML: library error:' + tmlcoredllname + #13#13 + amsg,
               {$if defined(FPC)}
                 mtError, [mbOK], 0);
               {$else}
                 {$if CompilerVersion >= 21}
                    TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
                 {$else}
                    mtError, [mbOK], 0);
                 {$ifend}
               {$ifend}
  {$ifend}
  Halt(1); // terminate application
end;

//------------------------------------------------------------------------------

{$if defined(FPC)}
// Record to pass the thread info to the wrapper function
type
  PTMLThread = Pointer;
  TTMLThreadFunc = function(user_data: Pointer): Pointer; cdecl;

  TCdeclThreadFunc = record
    Func: TTMLThreadFunc;  //cdecl function
    Data: Pointer;         //orig data
  end;
  PCdeclThreadFunc = ^TCdeclThreadFunc;

{ C to pascal wrapper function.
  BeginThread expects a 'register' function.
  Vortex supplies a 'cdecl' function. }
function C2P_Translator(FuncData: Pointer) : ptrint;
var
  ThreadData: TCdeclThreadFunc;
begin
  //Copy the supplied info
  ThreadData := PCdeclThreadFunc(FuncData)^;

  //Release memory allocated in tml_thread_create()
  Dispose(PCdeclThreadFunc(FuncData));

  //Finally we run the thread function
  Result := ptrint(ThreadData.Func(ThreadData.Data));
end;

{ FPC specific thread create function to replace Vortex's thread create.
  This is required because FPC doesn't work when C libraries create their
  own threads. }
{$WARNINGS OFF} //For "cdecl'd functions have no high parameter"
function tml_thread_create(thread_def : PTMLThread;
                           func       : TTMLThreadFunc;
                           user_data  : Pointer;
                           args       : Pointer): longint; cdecl;
var
  ThreadData: PCdeclThreadFunc;
  TID:        TThreadID;
begin
  if Assigned(thread_def) and Assigned(func) then
  begin
    // Pass thread information to C to Pascal wrapping function
    New(ThreadData);
    ThreadData^.Func := func;
    ThreadData^.Data := user_data;

    // Start thread
    TID := TThreadID(BeginThread(@C2P_Translator, ThreadData));

    if TID <> TThreadID(0) then
    begin
      // Don't free memory here, it will be done in the thread function
      Result := 1;
      TThreadID(thread_def^) := TID;
    end
    else
    begin
      // Free memory
      dispose(ThreadData);
      Result := 0;
      TThreadID(thread_def^) := TThreadID(0);
    end;
  end
  else Result := 0;
end;
{$WARNINGS ON}

{ FPC specific thread destroy function to replace Vortex's thread destroy.
  This is required because FPC doesn't work when C libraries create their
  own threads. }
function tml_thread_destroy(thread_def: PTMLThread;
                            free_data:  longint): longint; cdecl;
var
  TID: TThreadID;
  err: Integer;
begin
  if Assigned(thread_def) then
  begin
    TID := TThreadID(thread_def^);

    // Wait for thread
    err := WaitForThreadTerminate(TID, 2000);

    // Free resources
    if free_data = 1 then tml_Sys_Free(thread_def);

    if err = 0 then Result := 1
               else Result := 0;
  end;
end;

//------------------------------------------------------------------------------

type
  TTMLDummyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  procedure TTMLDummyThread.Execute;
  begin
    // do nothing here
  end;
{$ifend}

//------------------------------------------------------------------------------

procedure tml_init;
begin
  {$if defined(FPC)}
    // intialize the threading system
    with TTMLDummyThread.Create(false) do
    begin
      WaitFor;
      Free;
    end;

    if not tmlDesigning then
    begin
      // Set pascal specific thread creators
      tml_Core_Thread_Set_OnCreate(@tml_thread_create);
      tml_Core_Thread_Set_OnDestroy(@tml_thread_destroy);
    end;
  {$ifend}
end;

//------------------------------------------------------------------------------

function tml_loaded : Boolean;
begin
  Result := tmlLoaded;
end;

function tml_Copyright : string;
var
  iLen: TML_INT32;
  pVal: PSIDEX_STRING;
begin
  Result := '';
  if Assigned(tml_Core_Get_Copyright) then
  begin
    iLen := 0;
    pVal := nil;
    tml_Core_Get_Copyright(pVal, iLen);
    if Assigned(pVal) then Result := pVal;
  end;
end;

function tml_Version : string;
var
  v1, v2, v3: TML_INT32;
  psLib: PSIDEX_STRING;
begin
  Result := '';
  v1 := 0;
  v2 := 0;
  v3 := 0;
  if Assigned(tml_Core_Get_Version) then
  begin
    psLib := nil;
    tml_Core_Get_Version(v1, v2, psLib);
    if Assigned(psLib) then Result := psLib;
  end
  else if Assigned(tml_Core_Get_Version_Ext_old) then
  begin
    tml_Core_Get_Version_Ext_old(v1, v2, v3);
    Result := Format('%d.%d.%d', [v1, v2, v3]);
  end
  else if Assigned(tml_Core_Get_Version_old) then
  begin
    tml_Core_Get_Version_old(v1, v2);
    Result := Format('%d.%d.0', [v1, v2]);
  end;
end;

//------------------------------------------------------------------------------

initialization

TMLCore_dllHandle  := TML_CORE_DLLHANDLE_NULL;
tmlLoaded          := false;
tmlUseExceptions   := true;
tmlDesigning       := false;
tmlDesignCopyright := '';
tmlDesignVersion   := '';
SetLength(tmlDesignInfos, 0);

try
  {$if not defined(ANDROID)}
    if Assigned(Application) then
    begin
      tmlExeFileName := ExtractFileName(Application.ExeName);
      for tmlLoopIdx := Low(tmlExeDesigners) to High(tmlExeDesigners) do
      begin
        if AnsiCompareText(tmlExeFileName, tmlExeDesigners[tmlLoopIdx]) = 0 then
        begin
          tmlDesigning := true;
          Break;
        end;
      end;
    end;
  {$ifend}
  LoadTML();
  if tmlDesigning then UnloadTML()
                  else tml_init;
except
  on E: EDLLImportError do
  begin
    InitError(E.Message);
  end;
  on E: Exception do
  begin
    InitError(E.Message);
  end;
  else
  begin
    InitError('Unknown error!');
  end;
end;

//------------------------------------------------------------------------------

finalization

UnloadTML();
SetLength(tmlDesignInfos, 0);

//------------------------------------------------------------------------------

end.

