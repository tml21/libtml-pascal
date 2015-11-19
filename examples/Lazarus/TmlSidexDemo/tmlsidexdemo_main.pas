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

unit TmlSidexDemo_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, CheckLst, Grids, Buttons,
  thrDataProcessing, uTMLClasses, uTMLCore, uTMLTypes;

type
  {$ifdef CPU64}
    TID_Type = Int64;
  {$else}
    TID_Type = Integer;
  {$endif}


  { TTmlSidexDemo_Form }

  TTmlSidexDemo_Form = class(TForm)
    AliveCheck: TTMLCommand;
    ClearMessages_MenuItem: TMenuItem;
    CloseAll_Button: TButton;
    GetStreamID: TTMLCommand;
    ListFiles: TTMLCommand;
    Files_Page: TTabSheet;
    Files_PopupMenu: TPopupMenu;
    DownloadFile_MenuItem: TMenuItem;
    Downloads_Page: TTabSheet;
    Downloads_StringGrid: TStringGrid;
    Downloads_PopupMenu: TPopupMenu;
    CancelDownload_MenuItem: TMenuItem;
    CancelAllDownloads_MenuItem: TMenuItem;
    Logging_Button: TButton;
    RemoveFinishedDownloads_MenuItem: TMenuItem;
    RefreshFileList_MenuItem: TMenuItem;
    SaveFile_Dialog: TSaveDialog;
    SaveStorage_Button: TButton;
    SendMessage: TTMLCommand;
    Messages_StringGrid: TStringGrid;
    Messages_PopupMenu: TPopupMenu;
    SendMessage_Button: TButton;
    Message_Edit: TEdit;
    Message_Label: TLabel;
    Main_Notebook: TPageControl;
    Progress_Page: TTabSheet;
    Messages_Page: TTabSheet;
    ProcessingSomething: TTMLCommand;
    ProcessingSomething_Shape: TShape;
    ClearProgress_MenuItem: TMenuItem;
    Progress_PopupMenu: TPopupMenu;
    Progress_StringGrid: TStringGrid;
    Slider_Label: TLabel;
    Slider_TrackBar: TTrackBar;
    StartProcessingSomething_Button: TButton;
    NumberOfPortions_Edit: TEdit;
    DurationForPortion_Edit: TEdit;
    NumberOfProgress_Edit: TEdit;
    ProcessingSomething_GroupBox: TGroupBox;
    NumberOfPortions_Label: TLabel;
    Duration_Label: TLabel;
    DurationUnit_Label: TLabel;
    NumberOfProgress_Label: TLabel;
    CloseAll: TTMLCommand;
    ChangingName: TTMLCommand;
    ProcessingMode_RadioGroup: TRadioGroup;
    SliderDemo: TTMLCommand;
    SayHelloReceived_Shape: TShape;
    AskForFriendsReceived_Shape: TShape;
    SayGoodbyeReceived_Shape: TShape;
    SaveStorage_Dialog: TSaveDialog;
    SayBye: TTMLCommand;
    AskForFriends: TTMLCommand;
    SayHello: TTMLCommand;
    Connections_ListBox: TCheckListBox;
    Connection_GroupBox: TGroupBox;
    ConnectionIP_Edit: TEdit;
    ConnectionIP_Label: TLabel;
    ConnectionPort_Edit: TEdit;
    ConnectionPort_Label: TLabel;
    ConnectionStatus_Shape: TShape;
    Connections_Label: TLabel;
    StartStopListener_Button: TButton;
    ListenerIP_Edit: TEdit;
    ListenerPort_Edit: TEdit;
    Caption_Edit: TEdit;
    Listener_GroupBox: TGroupBox;
    ListenerIP_Label: TLabel;
    ListenerPort_Label: TLabel;
    Caption_Label: TLabel;
    ListenerStatus_Shape: TShape;
    Connect_Button: TButton;
    StatusBar: TStatusBar;
    ConnectionStatus_Timer: TTimer;
    SayHelloReceived_Timer: TTimer;
    AskForFriendsReceived_Timer: TTimer;
    SayGoodbyeReceived_Timer: TTimer;
    GUI_Timer: TTimer;
    StopProcessingSomething_Button: TButton;
    AliveCheck_Timer: TTimer;
    Files_StringGrid: TStringGrid;
    Extras_Page: TTabSheet;
    TMLCore: TTMLCore;
    TMLProfile: TTMLProfile;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    // Network connection...
    procedure ConnectionIP_EditChange(Sender: TObject);
    procedure ConnectionIP_EditClick(Sender: TObject);
    procedure ConnectionPort_EditChange(Sender: TObject);
    procedure ConnectionPort_EditClick(Sender: TObject);
    procedure Connections_ListBoxClick(Sender: TObject);
    procedure Connections_ListBoxClickCheck(Sender: TObject);
    procedure ListenerIP_EditChange(Sender: TObject);
    procedure ListenerIP_EditClick(Sender: TObject);
    procedure ListenerPort_EditChange(Sender: TObject);
    procedure ListenerPort_EditClick(Sender: TObject);
    procedure ConnectionStatus_TimerTimer(Sender: TObject);
    procedure Connect_ButtonClick(Sender: TObject);
    procedure StartStopListener_ButtonClick(Sender: TObject);

    // Friend finder...
    procedure SayByeCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure SayGoodbyeReceived_TimerTimer(Sender: TObject);
    procedure SayHelloCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure SayHelloReceived_TimerTimer(Sender: TObject);
    procedure AskForFriendsCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure AskForFriendsReceived_TimerTimer(Sender: TObject);
    procedure AliveCheckCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure AliveCheck_TimerTimer(Sender: TObject);

    // TML processing...
    procedure TMLCoreCommandReady(Sender: TObject; acmd: TTMLCmdMsg);
    procedure TMLCoreProgress(Sender: TObject; acmd: TTMLCmdMsg;
      aprogress: TML_INT32);
    procedure TMLCoreStatusReply(Sender: TObject; acmd: TTMLCmdMsg;
      atype: TML_INT32; amsg: string);
    procedure TMLProfileCommandReady(Sender: TObject; acmd: TTMLCmdMsg);
    procedure TMLProfileProgress(Sender: TObject; acmd: TTMLCmdMsg;
      aprogress: TML_INT32);
    procedure TMLProfileStatusReply(Sender: TObject; acmd: TTMLCmdMsg;
      atype: TML_INT32; amsg: string);

    // Changing name...
    procedure Caption_EditChange(Sender: TObject);
    procedure ChangingNameCmdCall(Sender: TObject; acmd: TTMLCmdMsg);

    // Processing something demo...
    procedure DurationForPortion_EditChange(Sender: TObject);
    procedure DurationForPortion_EditClick(Sender: TObject);
    procedure NumberOfPortions_EditChange(Sender: TObject);
    procedure NumberOfPortions_EditClick(Sender: TObject);
    procedure NumberOfProgress_EditChange(Sender: TObject);
    procedure NumberOfProgress_EditClick(Sender: TObject);
    procedure ProcessingMode_RadioGroupClick(Sender: TObject);
    procedure ProcessingSomethingCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure StartProcessingSomething_ButtonClick(Sender: TObject);
    procedure StopProcessingSomething_ButtonClick(Sender: TObject);

    // Progress for "Processing something demo"...
    procedure ClearProgress_MenuItemClick(Sender: TObject);
    procedure Progress_StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);

    // Send messages demo...
    procedure Message_EditChange(Sender: TObject);
    procedure Message_EditClick(Sender: TObject);
    procedure ClearMessages_MenuItemClick(Sender: TObject);
    procedure SendMessageCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure SendMessage_ButtonClick(Sender: TObject);

    // Misc
    procedure Main_NotebookPageChanged(Sender: TObject);
    procedure SilenceEnterKeyPress(Sender: TObject; var Key: char);
    procedure GUI_TimerTimer(Sender: TObject);
    procedure Logging_ButtonClick(Sender: TObject);
    procedure SaveStorage_ButtonClick(Sender: TObject);

    // Close all...
    procedure CloseAllCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure CloseAll_ButtonClick(Sender: TObject);

    // Slider demo...
    procedure SliderDemoCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure Slider_TrackBarChange(Sender: TObject);

    // Download demo (file sender)...
    procedure ListFilesCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
    procedure GetStreamIDCmdCall(Sender: TObject; acmd: TTMLCmdMsg);

    // Download demo (file receiver)...
    procedure RefreshFileList_MenuItemClick(Sender: TObject);
    procedure DownloadFile_MenuItemClick(Sender: TObject);
    procedure CancelDownload_MenuItemClick(Sender: TObject);
    procedure CancelAllDownloads_MenuItemClick(Sender: TObject);
    procedure RemoveFinishedDownloads_MenuItemClick(Sender: TObject);
    procedure Downloads_StringGridDblClick(Sender: TObject);
    procedure Downloads_StringGridDrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);

  private
    FClosingInProgress: Boolean;
    FData:              TDataStorage;
    FProcessingCount:   Integer;
    FStopProcessing:    Boolean;
    FGridProgressBar:   TProgressBar;
    FSliderLocked:      Boolean;
    FFileStreamList:    TThreadList;  // Sender
    FDownloadList:      TThreadList;  // Receiver

    procedure ListFriends(ALocking: Boolean = true);
    procedure ClearProgress;
    procedure ClearMessages;
    procedure ClearFileList;
    procedure ClearDownloads;

    procedure SetDefaultButton(AButton: TObject);

    procedure AsyncCall(pCall: PtrInt); // 'AsyncCall' must free 'pCall'
    procedure OnIdle(Sender: TObject; var Done: Boolean);

    // Download demo (file sender)...
    procedure ReleaseStreamID(StreamID: TML_STREAM_ID);
    procedure ReleaseAllFileStreams;
    procedure ReceiverStreamClosed(Sender: TTMLStreamTypeBase;
                             StreamID:  TML_STREAM_ID);
    procedure ReceiverStreamFault(Sender: TTMLStreamTypeBase;
                             StreamID:  TML_STREAM_ID;
                             ErrorCode: TML_INT32);

    // Download demo (file receiver)...
    procedure ReleaseAllDownloads;
    function  StreamProgress(Sender:    TTMLStreamTypeBase;
                             StreamID:  TML_STREAM_ID;
                             iBuffer, nBuffers: TML_INT64;
                             Percent:   TML_INT32): TML_BOOL;
    procedure StreamFinished(Sender:    TTMLStreamTypeBase;
                             StreamID:  TML_STREAM_ID;
                             errCode:   TML_INT32);
  end;

var
  TmlSidexDemo_Form: TTmlSidexDemo_Form;

implementation

uses
  LclIntf, FileUtil, TmlSidexDemo_Logging,
  uSidexLib, uTMLErrors, uSidexVariant, uSidexTypes, Variants;

type
  TFileStreamListEntry = record // Sender
    ID:         TML_STREAM_ID;
    Stream:     TTMLStreamTypeBase;
    StreamType: TTMLStreamTypeClass;
    Name:       string;
  end;

  TDownloadListEntry = record   // Receiver
    Stream:     TTMLStreamTypeBase;
    StreamType: TTMLStreamTypeClass;
    dd:         TDownloadData;  // data is owned by StringGrid
  end;

{ TTmlSidexDemo_Form }

procedure TTmlSidexDemo_Form.FormCreate(Sender: TObject);
{$ifdef Unix}
var
  i, n: Integer;
{$endif}
begin
  FClosingInProgress := false;
  FProcessingCount   := 0;
  FStopProcessing    := false;
  FGridProgressBar   := nil;
  FSliderLocked      := false;
  FData              := TDataStorage.Create(TMLCore, TMLProfile, @AsyncCall);
  FFileStreamList    := TThreadList.Create;
  if Assigned(FFileStreamList) then FFileStreamList.Duplicates := dupIgnore;
  FDownloadList      := TThreadList.Create;
  if Assigned(FDownloadList) then FDownloadList.Duplicates := dupIgnore;

  Main_Notebook.PageIndex := 0;

  (*
  {$ifdef Unix}
    // necessary on OSX to keep Fonts readable...
    {$ifdef Darwin}
      Font.Size := 11;
      n := Progress_StringGrid.Columns.Count;
      for i := 0 to n - 1 do
      begin
        Progress_StringGrid.Columns[i].Title.Font.Size := Font.Size;
      end;
      n := Messages_StringGrid.Columns.Count;
      for i := 0 to n - 1 do
      begin
        Messages_StringGrid.Columns[i].Title.Font.Size := Font.Size;
      end;
      n := Files_StringGrid.Columns.Count;
      for i := 0 to n - 1 do
      begin
        Files_StringGrid.Columns[i].Title.Font.Size := Font.Size;
      end;
      n := Downloads_StringGrid.Columns.Count;
      for i := 0 to n - 1 do
      begin
        Downloads_StringGrid.Columns[i].Title.Font.Size := Font.Size;
      end;
    {$endif}
  {$endif}
  *)
end;

procedure TTmlSidexDemo_Form.FormDestroy(Sender: TObject);
begin
  ClearProgress;
  ClearMessages;
  ClearFileList;
  ClearDownloads;
  ReleaseAllDownloads;
  ReleaseAllFileStreams;
  FreeAndNil(FDownloadList);
  FreeAndNil(FFileStreamList);
  FreeAndNil(FGridProgressBar);
  FreeAndNil(FData);
end;

procedure TTmlSidexDemo_Form.FormShow(Sender: TObject);
var
  HN, IP, WSAErr: string;
begin
  Application.OnIdle := @OnIdle;

  Application.Title := Caption;
  FData.MyName      := Caption;
  Caption_Edit.Text := Caption;
  if Caption_Edit.CanFocus then Caption_Edit.SetFocus;
  Caption_Edit.SelectAll;

  ListenerIP_Edit.Text   := TMLCore.ListenerIP;
  ListenerPort_Edit.Text := TMLCore.ListenerPort;

  HN     := '';
  IP     := '';
  WSAErr := '';
  if GetIPFromHost(HN, IP, WSAErr) then
  begin
    ConnectionIP_Edit.Text   := IP;
    StatusBar.Panels[1].Text := IP;
    StatusBar.Panels[2].Text := 'Hostname: ' + HN;
    FData.HostName := HN;
    FData.HostIP   := IP;
  end
  else
  begin
    StatusBar.Panels[1].Text := '???.???.???.???';
    StatusBar.Panels[2].Text := WSAErr;
  end;
  ListFriends;

  SetDefaultButton(StartStopListener_Button);
  StartStopListener_ButtonClick(StartStopListener_Button);
end;

procedure TTmlSidexDemo_Form.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FClosingInProgress := true;
  if TMLCore.ListenerEnabled then
  begin
    StartStopListener_ButtonClick(StartStopListener_Button);
  end;
  TMLCore.GeneralDeregistration;
  Application.ProcessMessages;
  ClearProgress;
  ClearMessages;
  ClearFileList;
  ClearDownloads;
  CloseAction := caFree;
end;

procedure TTmlSidexDemo_Form.OnIdle(Sender: TObject; var Done: Boolean);
begin
  CheckSynchronize; // necessary for synchronized calls
  Done := true;
end;

procedure TTmlSidexDemo_Form.GUI_TimerTimer(Sender: TObject);
begin
  CheckSynchronize;             // necessary for synchronized calls
  Application.ProcessMessages;  // necessary for processing async calls
end;

procedure TTmlSidexDemo_Form.Logging_ButtonClick(Sender: TObject);
begin
  LoggingDlg := TLoggingDlg.Create(Self);
  if Assigned(LoggingDlg) then
  begin
    LoggingDlg.TMLCore := TMLCore;
    LoggingDlg.ShowModal;
    LoggingDlg.Release;
    LoggingDlg := nil;
  end;
end;

procedure TTmlSidexDemo_Form.StartStopListener_ButtonClick(Sender: TObject);
var
  cmd:         TTMLCmdMsg;
  AutoConnect: Boolean;
  IP:          string;
begin
  StartStopListener_Button.Enabled := false;
  if TMLCore.ListenerEnabled then
  begin
    AliveCheck_Timer.Enabled := false;
    cmd := TTMLCmdMsg.Create(CMD_FINDER_SAY_GOODBYE);
    if Assigned(cmd) then
    begin
      try
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
        TMLProfile.SendEvent(cmd);
        StatusBar.Panels[2].Text := '';
      except
        StatusBar.Panels[2].Text := 'Error sending event! (SayGoodbye)';
      end;
      FreeAndNil(cmd);
    end;
    try
      if not FClosingInProgress then
      begin
        FData.RemoveAllFriends;
        ListFriends;
        ClearProgress;
      end;
      TMLCore.ListenerEnabled := false;
      ListenerStatus_Shape.Brush.Color := clSilver;
      ListenerStatus_Shape.Repaint;
    except
      on E: Exception do
      begin
        ListenerStatus_Shape.Brush.Color := clRed;
        ListenerStatus_Shape.Repaint;
        StatusBar.Panels[2].Text := E.Message;
        ShowMessage(E.Message);
      end;
    end;
  end
  else
  begin
    try
      ListenerStatus_Shape.Brush.Color := clYellow;
      ListenerStatus_Shape.Repaint;
      AutoConnect             := false;
      TMLCore.ListenerIP      := FormatIP(ListenerIP_Edit.Text);
      TMLCore.ListenerPort    := FormatPort(ListenerPort_Edit.Text);
      TMLCore.ListenerEnabled := true;
      if not TMLCore.ListenerEnabled then
      begin
        // let TML find a free port...
        TMLCore.ListenerPort    := '0';
        TMLCore.ListenerEnabled := true;
        AutoConnect             := true;
      end;
      if TMLCore.ListenerEnabled then
      begin
        if AutoConnect then
        begin
          IP := FormatIP(ListenerIP_Edit.Text);
          if IP = '0.0.0.0' then IP := FData.HostIP;
          ConnectionIP_Edit.Text   := IP;
          ConnectionPort_Edit.Text := ListenerPort_Edit.Text;
        end;
        ListenerPort_Edit.Text := TMLCore.ListenerPort;
        ListenerStatus_Shape.Brush.Color := clLime;
        ListenerStatus_Shape.Repaint;
        StatusBar.Panels[2].Text := Format('Listening on %s Port %s',
                                           [TMLCore.ListenerIP,
                                            TMLCore.ListenerPort]);
        FData.ListenerIP   := TMLCore.ListenerIP;
        FData.ListenerPort := TMLCore.ListenerPort;
        if AutoConnect then Connect_ButtonClick(Sender);
      end
      else
      begin
        ListenerStatus_Shape.Brush.Color := clRed;
        ListenerStatus_Shape.Repaint;
        StatusBar.Panels[2].Text := 'Error: unable to find a free listener port!';
      end;
    except
      on E: Exception do
      begin
        ListenerStatus_Shape.Brush.Color := clRed;
        ListenerStatus_Shape.Repaint;
        StatusBar.Panels[2].Text := E.Message;
        ShowMessage(E.Message);
      end;
    end;
  end;
  if TMLCore.ListenerEnabled then
  begin
    StartStopListener_Button.Caption := 'Stop';
    ListenerIP_Edit.Enabled          := false;
    ListenerPort_Edit.Enabled        := false;
    Connection_GroupBox.Enabled      := true;
    SetDefaultButton(Connect_Button);
  end
  else
  begin
    StartStopListener_Button.Caption := 'Start';
    ListenerIP_Edit.Enabled          := true;
    ListenerPort_Edit.Enabled        := true;
    Connection_GroupBox.Enabled      := false;
    FData.ListenerIP   := '';
    FData.ListenerPort := '';
  end;
  AliveCheck_Timer.Enabled := TMLCore.ListenerEnabled;
  StartStopListener_Button.Enabled := true;
end;

procedure TTmlSidexDemo_Form.Connect_ButtonClick(Sender: TObject);
var
  cmd: TTMLCmdMsg;
  err: TML_INT32;
begin
  if not FData.IsItMe(ConnectionIP_Edit.Text,
                      ConnectionPort_Edit.Text,
                      ConnectionIP_Edit.Text) then
  begin
    ConnectionStatus_Timer.Enabled := false;
    cmd := TTMLCmdMsg.Create(CMD_FINDER_SAY_HELLO);
    if Assigned(cmd) then
    begin
      try
        ConnectionStatus_Shape.Brush.Color := clYellow;
        ConnectionStatus_Shape.Repaint;
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
        err := TMLCore.CallSync(TMLProfile.Profile,
                                ConnectionIP_Edit.Text,
                                ConnectionPort_Edit.Text,
                                cmd, 10000);
        if err = TML_SUCCESS then
        begin
          StatusBar.Panels[2].Text := cmd.Data.ReadString(GROUP_NAME_ANSWER, PARAM_NAME_MESSAGE, '');
          if cmd.Data.ReadBoolean(GROUP_NAME_ANSWER, PARAM_NAME_ACCEPT, false) then
          begin
            if FData.AddFriendToList(ConnectionIP_Edit.Text,
                                     ConnectionPort_Edit.Text,
                                     cmd.Data.ReadString(GROUP_NAME_ANSWER, PARAM_NAME_HOST,
                                                         ConnectionIP_Edit.Text),
                                     cmd.Data.ReadString(GROUP_NAME_ANSWER, PARAM_NAME_NAME, '')) then
            begin
              ConnectionStatus_Shape.Brush.Color := clLime;
              ConnectionStatus_Shape.Repaint;

              if not FSliderLocked then
              begin
                FSliderLocked := true;
                Slider_TrackBar.Position := cmd.Data.ReadInt(GROUP_NAME_SLIDER,
                                                             PARAM_NAME_VALUE,
                                                             Slider_TrackBar.Position);
                FSliderLocked := false;
              end;
            end;
            ListFriends;
          end;
        end
        else
        begin
          StatusBar.Panels[2].Text := Format('TMLCore.CallSync Error %d', [err]);
          ConnectionStatus_Shape.Brush.Color := clRed;
          ConnectionStatus_Shape.Repaint;
        end;
      except
        StatusBar.Panels[2].Text := 'Error connecting friend!';
        ConnectionStatus_Shape.Brush.Color := clRed;
        ConnectionStatus_Shape.Repaint;
      end;
      FreeAndNil(cmd);
    end;
    ConnectionStatus_Timer.Enabled := true;
  end;
end;

procedure TTmlSidexDemo_Form.SaveStorage_ButtonClick(Sender: TObject);
begin
  if Assigned(FData) then
  begin
    SaveStorage_Dialog.FileName := FData.StorageFileName;
    if SaveStorage_Dialog.Execute then
    begin
      if not FData.SaveStorage(SaveStorage_Dialog.FileName) then
      begin
        StatusBar.Panels[2].Text := 'Error saving storage!';
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ConnectionStatus_TimerTimer(Sender: TObject);
begin
  ConnectionStatus_Timer.Enabled     := false;
  ConnectionStatus_Shape.Brush.Color := clSilver;
  ConnectionStatus_Shape.Repaint;
end;

procedure TTmlSidexDemo_Form.SayHelloCmdCall(Sender: TObject;
                                             acmd:   TTMLCmdMsg);
var
  IP, Port, Host, AName: string;
  QueryOK: Boolean;
begin
  if not FClosingInProgress then
  begin
    SayHelloReceived_Timer.Enabled     := false;
    SayHelloReceived_Shape.Brush.Color := clYellow;
    SayHelloReceived_Shape.Repaint;

    IP    := FormatIP      (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP,   ''));
    Port  := FormatPort    (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, ''));
    Host  := FormatHostName(acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_HOST, ''));
    AName :=                acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_NAME, '');
    QueryOK := IsIPAddress(IP) and (Port <> '');
    if QueryOK then
    begin
      try
        StatusBar.Panels[2].Text := Format('Hello from %s:%s', [Host, Port]);
        if not FClosingInProgress then
        begin
          QueryOK := FData.AddFriendToList(IP, Port, Host, AName);
          FData.SignalDataUpdate;
        end
        else QueryOK := false;
      except
        QueryOK := false;
        SayHelloReceived_Shape.Brush.Color := clRed;
        SayHelloReceived_Shape.Repaint;
      end;
    end;
  end
  else QueryOK := false;
  acmd.Data.WriteBoolean(GROUP_NAME_ANSWER, PARAM_NAME_ACCEPT, QueryOK);
  acmd.Data.WriteString (GROUP_NAME_ANSWER, PARAM_NAME_HOST,   FData.HostName);
  acmd.Data.WriteString (GROUP_NAME_ANSWER, PARAM_NAME_NAME,   FData.MyName);
  acmd.Data.WriteString (GROUP_NAME_ANSWER, PARAM_NAME_MESSAGE,
                         Format('Welcome to %s:%s!',
                                [FData.HostName, FData.ListenerPort]));
  acmd.Data.WriteInt(GROUP_NAME_SLIDER, PARAM_NAME_VALUE,
                     Slider_TrackBar.Position);
  if QueryOK then
  begin
    SayHelloReceived_Shape.Brush.Color := clLime;
    SayHelloReceived_Shape.Repaint;
    SayHelloReceived_Timer.Enabled := true;
  end;
end;

procedure TTmlSidexDemo_Form.SayHelloReceived_TimerTimer(Sender: TObject);
begin
  if not FClosingInProgress then
  begin
    SayHelloReceived_Timer.Enabled     := false;
    SayHelloReceived_Shape.Brush.Color := clSilver;
    SayHelloReceived_Shape.Repaint;
  end
  else SayHelloReceived_Timer.Enabled := false;
end;

procedure TTmlSidexDemo_Form.SayByeCmdCall(Sender: TObject;
                                           acmd:   TTMLCmdMsg);
var
  IP, Port, Host: string;
  QueryOK:        Boolean;
begin
  if not FClosingInProgress then
  begin
    SayGoodbyeReceived_Timer.Enabled     := false;
    SayGoodbyeReceived_Shape.Brush.Color := clYellow;
    SayGoodbyeReceived_Shape.Repaint;

    IP   := FormatIP      (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP,   ''));
    Port := FormatPort    (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, ''));
    Host := FormatHostName(acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_HOST, ''));
    QueryOK := IsIPAddress(IP) and (Port <> '');
    if QueryOK then
    begin
      try
        StatusBar.Panels[2].Text := Format('Goodby from %s:%s', [Host, Port]);
        if FData.IsMyFriend(IP, Port) then
        begin
          QueryOK := FData.RemoveFriendFromList(IP, Port);
          FData.SignalDataUpdate;
        end
        else QueryOK := true;
      except
        QueryOK := false;
        SayGoodbyeReceived_Shape.Brush.Color := clRed;
        SayGoodbyeReceived_Shape.Repaint;
      end;
    end;
  end
  else QueryOK := false;
  acmd.Data.WriteString(GROUP_NAME_ANSWER, PARAM_NAME_MESSAGE,
                        Format('Goodbye from %s:%s!',
                               [FData.HostName, FData.ListenerPort]));
  if QueryOK then
  begin
    SayGoodbyeReceived_Shape.Brush.Color := clLime;
    SayGoodbyeReceived_Shape.Repaint;
    SayGoodbyeReceived_Timer.Enabled := true;
  end;
end;

procedure TTmlSidexDemo_Form.SayGoodbyeReceived_TimerTimer(Sender: TObject);
begin
  if not FClosingInProgress then
  begin
    SayGoodbyeReceived_Timer.Enabled     := false;
    SayGoodbyeReceived_Shape.Brush.Color := clSilver;
    SayGoodbyeReceived_Shape.Repaint;
  end
  else SayGoodbyeReceived_Timer.Enabled := false;
end;

procedure TTmlSidexDemo_Form.AskForFriendsCmdCall(Sender: TObject;
                                                  acmd:   TTMLCmdMsg);
var
  Table:  SIDEX_VARIANT;
  Result: Boolean;
begin
  Result := false;
  if Assigned(FData.Storage) and not FClosingInProgress then
  begin
    AskForFriendsReceived_Timer.Enabled     := false;
    AskForFriendsReceived_Shape.Brush.Color := clYellow;
    AskForFriendsReceived_Shape.Repaint;

    FData.GetLock;
    try
      try
        Table := FData.Storage.ReadSdxTable(GROUP_NAME_MYSELF, TABLE_NAME_FRIENDS);
      except
        Table := SIDEX_VARIANT_NULL;
      end;
      if Table <> SIDEX_VARIANT_NULL then
      begin
        if sidex_Variant_Table_Check(Table) <> TML_FALSE then
        begin
          try
            acmd.Data.WriteSdxTable(GROUP_NAME_ANSWER, TABLE_NAME_FRIENDS, Table);
            Result := true;
          except
            Result := false;
            AskForFriendsReceived_Shape.Brush.Color := clRed;
            AskForFriendsReceived_Shape.Repaint;
          end;
        end;
        sidex_Variant_DecRef(Table);
      end;
    finally
      FData.ReleaseLock;
    end;
  end;

  if Result then
  begin
    AskForFriendsReceived_Shape.Brush.Color := clLime;
    AskForFriendsReceived_Shape.Repaint;
    AskForFriendsReceived_Timer.Enabled := true;
  end;
end;

procedure TTmlSidexDemo_Form.AskForFriendsReceived_TimerTimer(Sender: TObject);
begin
  if not FClosingInProgress then
  begin
    AskForFriendsReceived_Timer.Enabled     := false;
    AskForFriendsReceived_Shape.Brush.Color := clSilver;
    AskForFriendsReceived_Shape.Repaint;
  end
  else AskForFriendsReceived_Timer.Enabled := false;
end;

procedure TTmlSidexDemo_Form.AliveCheckCmdCall(Sender: TObject;
  acmd: TTMLCmdMsg);
begin
  // Note: this is not a synchonized call, do not use the GUI directly!
  acmd.Data.WriteBoolean(GROUP_NAME_ANSWER, PARAM_NAME_ACCEPT, true);
end;

procedure TTmlSidexDemo_Form.AliveCheck_TimerTimer(Sender: TObject);
var
  Call: TAsyncCallData;
begin
  if not FClosingInProgress then
  begin
    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID := ASYNC_ALIVE_CHECK;
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end
  else AliveCheck_Timer.Enabled := false;
end;

procedure TTmlSidexDemo_Form.ListFriends(ALocking: Boolean);
var
  Exists:         Boolean;
  i, j, n, m:     Integer;
  ID:             Int64;
  IDs, CheckIDs:  Array of TID_Type;
  fi:             TFriendInfo;
begin
  // keep current friend IDs in mind...
  n := Connections_ListBox.Count;
  SetLength(IDs, n);
  SetLength(CheckIDs, n);
  for i := 0 to n - 1 do
  begin
    IDs[i] := TID_Type(Connections_ListBox.Items.Objects[i]);
    if Connections_ListBox.Checked[i] then
      CheckIDs[i] := IDs[i] else CheckIDs[i] := 0;
  end;

  if Assigned(FData.Storage) and not FClosingInProgress then
  begin
    if ALocking then FData.GetLock;
    try
      n := FData.FriendCount(false);
      for i := 0 to n - 1 do
      begin
        fi := FData.GetFriendByIndex(i, false);

        if fi.ID <> 0 then
        begin
          // check, if this friend is already in the list...
          Exists := false;
          m := Length(IDs);
          for j := 0 to m - 1 do
          begin
            if IDs[j] = fi.ID then
            begin
              Exists := true;
              IDs[j] := 0;    // mark as updated
              Break;
            end;
          end;

          if Exists then
          begin
            // update existing list box entry...
            Exists := false;
            m := Connections_ListBox.Count;
            for j := 0 to m - 1 do
            begin
              if fi.ID = TID_Type(Connections_ListBox.Items.Objects[j]) then
              begin
                Exists := true;
                Connections_ListBox.Items.Strings[j] :=
                  FormatFriendInfo(fi.IP, fi.Port, fi.Host, fi.Name);
                Break;
              end;
            end;
          end;

          if not Exists then
          begin
            // add a new entry to list box...
            m := Connections_ListBox.Items.AddObject(
              FormatFriendInfo(fi.IP, fi.Port, fi.Host, fi.Name),
              TObject(PtrInt(fi.ID)));
            Connections_ListBox.Checked[m] := true;
          end;
        end;
      end;
    finally
      if ALocking then FData.ReleaseLock;
    end;
  end;

  // delete entries of removed friends...
  n := Connections_ListBox.Count;
  m := Length(IDs);
  for i := n - 1 downto 0 do
  begin
    ID := TID_Type(Connections_ListBox.Items.Objects[i]);
    Exists := true;
    for j := 0 to m - 1 do
    begin
      if IDs[j] = ID then
      begin
        Exists := false;
        Connections_ListBox.Items.Delete(i);
        Break;
      end;
    end;
    if Exists then
    begin
      for j := 0 to m - 1 do
      begin
        if CheckIDs[j] = ID then
        begin
          Connections_ListBox.Checked[i] := true;
          Break;
        end;
      end;
    end;
  end;

  if Main_Notebook.ActivePage = Files_Page then
  begin
    // maybe the selected item is removed
    RefreshFileList_MenuItemClick(Self);
  end;
end;

procedure TTmlSidexDemo_Form.CloseAll_ButtonClick(Sender: TObject);
var
  cmd:       TTMLCmdMsg;
  Call:      TAsyncCallData;
{  iLogValue: TML_INT32;
  bSuccess:  Boolean;}
begin
{
  iLogValue := TML_LOG_OFF;
  bSuccess := (tml_Core_Get_LoggingValue(TMLCore.TMLCoreHandle,
                                         iLogValue) = TML_SUCCESS);
  if bSuccess then
  begin
    iLogValue := iLogValue or TML_LOG_VORTEX_INTERN;
    tml_Core_Set_LoggingValue(TMLCore.TMLCoreHandle, iLogValue);
  end;
}
  cmd := TTMLCmdMsg.Create(CMD_FINDER_DO_CLOSE);
  if Assigned(cmd) then
  begin
    try
      TMLProfile.SendEvent(cmd);
    except
      StatusBar.Panels[2].Text := 'Error sending event! (CloseAll)';
    end;
    FreeAndNil(cmd);
  end;

  Call := TAsyncCallData.Create;
  if Assigned(Call) then
  begin
    Call.ID := ASYNC_DO_CLOSE;
    Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
  end;
end;

procedure TTmlSidexDemo_Form.CloseAllCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
var
  Call:      TAsyncCallData;
{  iLogValue: TML_INT32;
  bSuccess:  Boolean;}
begin
  if not FClosingInProgress then
  begin
{
    iLogValue := TML_LOG_OFF;
    bSuccess := (tml_Core_Get_LoggingValue(TMLCore.TMLCoreHandle,
                                           iLogValue) = TML_SUCCESS);
    if bSuccess then
    begin
      iLogValue := iLogValue or TML_LOG_VORTEX_INTERN;
      tml_Core_Set_LoggingValue(TMLCore.TMLCoreHandle, iLogValue);
    end;
}
    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID := ASYNC_DO_CLOSE;
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end;
end;

procedure TTmlSidexDemo_Form.Caption_EditChange(Sender: TObject);
var
  cmd: TTMLCmdMsg;
begin
  if (Caption_Edit.Text <> FData.MyName) and not FClosingInProgress then
  begin
    FData.MyName      := Caption_Edit.Text;
    Application.Title := FData.MyName;
    Caption           := FData.MyName;

    cmd := TTMLCmdMsg.Create(CMD_FINDER_CHANGING_NAME);
    if Assigned(cmd) then
    begin
      try
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
        cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
        TMLProfile.SendEvent(cmd);
      except
        StatusBar.Panels[2].Text := 'Error sending event! (ChangingName)';
      end;
      FreeAndNil(cmd);
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ChangingNameCmdCall(Sender: TObject;
                                                 acmd: TTMLCmdMsg);
var
  IP, Port, NewName: string;
begin
  if not FClosingInProgress then
  begin
    IP      := FormatIP      (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP,   ''));
    Port    := FormatPort    (acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, ''));
    NewName :=                acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_NAME, '');
    if IsIPAddress(IP) and (Port <> '') then
    begin
      try
        if not FClosingInProgress then
        begin
          FData.ChangeNameOfFriend(IP, Port, NewName);
          FData.SignalDataUpdate;
        end;
      except
        // do nothing
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.TMLCoreProgress(Sender: TObject; acmd: TTMLCmdMsg;
  aprogress: TML_INT32);
var
  Call: TAsyncCallData;
begin
  if not FClosingInProgress then
  begin
    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.Info.IP   := acmd.Data.ReadString(GROUP_NAME_PROGRESS, PARAM_NAME_IP,   '');
      Call.Info.Port := acmd.Data.ReadString(GROUP_NAME_PROGRESS, PARAM_NAME_PORT, '');
      Call.Info.Host := acmd.Data.ReadString(GROUP_NAME_PROGRESS, PARAM_NAME_HOST, '');
      Call.Info.Name := acmd.Data.ReadString(GROUP_NAME_PROGRESS, PARAM_NAME_NAME, '');

      Call.ID         := ASYNC_UPDATE_PROGRESS;
      Call.CommandUID := acmd.UID;
      Call.Progress   := aprogress;
      Call.Text       := FormatFriendInfo(Call.Info.IP, Call.Info.Port,
                                          Call.Info.Host, Call.Info.Name);
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end;
end;

procedure TTmlSidexDemo_Form.TMLCoreCommandReady(Sender: TObject;
  acmd: TTMLCmdMsg);
var
  Call: TAsyncCallData;
begin
  if not FClosingInProgress then
  begin
    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID         := ASYNC_UPDATE_PROGRESS;
      Call.CommandUID := acmd.UID;
      Call.Progress   := -1;  // remove progress
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end;
end;

procedure TTmlSidexDemo_Form.TMLCoreStatusReply(Sender: TObject;
  acmd: TTMLCmdMsg; atype: TML_INT32; amsg: string);
var
  Call: TAsyncCallData;
begin
  if not FClosingInProgress then
  begin
    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID   := ASYNC_SHOW_MESSAGE;
      Call.Text := amsg;
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end;
end;

procedure TTmlSidexDemo_Form.TMLProfileProgress(Sender: TObject;
  acmd: TTMLCmdMsg; aprogress: TML_INT32);
begin
  TMLCoreProgress(Sender, acmd, aprogress);
end;

procedure TTmlSidexDemo_Form.TMLProfileCommandReady(Sender: TObject;
  acmd: TTMLCmdMsg);
begin
  TMLCoreCommandReady(Sender, acmd);
end;

procedure TTmlSidexDemo_Form.TMLProfileStatusReply(Sender: TObject;
  acmd: TTMLCmdMsg; atype: TML_INT32; amsg: string);
begin
  TMLCoreStatusReply(Sender, acmd, atype, amsg);
end;

procedure TTmlSidexDemo_Form.Progress_StringGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  pd: TProgressData;
  rcProgress: TRect;
begin
  if (aCol = 0) and (aRow > 0) then
  begin
    pd := TProgressData(Progress_StringGrid.Objects[aCol, aRow]);
    if Assigned(pd) then
    begin
      rcProgress.Left   := aRect.Left + 2;
      rcProgress.Top    := aRect.Top  + 2;
      rcProgress.Right  := rcProgress.Left + ((pd.Progress *
                           (aRect.Right - aRect.Left - 4)) div 100);
      rcProgress.Bottom := aRect.Bottom - 2;

      Progress_StringGrid.Canvas.Brush.Style := bsSolid;
      Progress_StringGrid.Canvas.Brush.Color := clBlue;
      Progress_StringGrid.Canvas.FillRect(rcProgress);

      rcProgress.Right  := aRect.Right - 2;
      Progress_StringGrid.Canvas.Brush.Style := bsClear;
      Progress_StringGrid.Canvas.Pen.Style   := psSolid;
      Progress_StringGrid.Canvas.Pen.Mode    := pmCopy;
      Progress_StringGrid.Canvas.Pen.Width   := 1;
      Progress_StringGrid.Canvas.Pen.Color   := clBlack;
      Progress_StringGrid.Canvas.Rectangle(rcProgress);

      Progress_StringGrid.Canvas.Brush.Style := bsSolid;

{
      if not Assigned(FGridProgressBar) then
      begin
        FGridProgressBar := TProgressBar.Create(Progress_StringGrid);
        if Assigned(FGridProgressBar) then
        begin
          FGridProgressBar.Left        := 0;
          FGridProgressBar.Top         := 0;
          FGridProgressBar.Width       := 1;
          FGridProgressBar.Height      := 1;
          FGridProgressBar.Min         := 0;
          FGridProgressBar.Max         := 100;
          FGridProgressBar.Position    := 0;
          FGridProgressBar.Orientation := pbHorizontal;
          FGridProgressBar.Parent      := Progress_StringGrid;
          FGridProgressBar.Visible     := false;
          FGridProgressBar.HandleNeeded;
        end;
      end;
      if Assigned(FGridProgressBar) then
      begin
        FGridProgressBar.Left     := aRect.Left + 2;
        FGridProgressBar.Top      := aRect.Top  + 2;
        FGridProgressBar.Width    := aRect.Right  - aRect.Left - 4;
        FGridProgressBar.Height   := aRect.Bottom - aRect.Top  - 4;
        FGridProgressBar.Position := pd.Progress;
        FGridProgressBar.PaintTo(Progress_StringGrid.Canvas,
                                 aRect.Left + 2, aRect.Top + 2);
      end;
}
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ClearProgress;
var
  i, n: Integer;
  pd:   TProgressData;
begin
  n := Progress_StringGrid.RowCount;
  for i := n - 1 downto 1 do
  begin
    pd := TProgressData(Progress_StringGrid.Objects[0, i]);
    Progress_StringGrid.Objects[0, i] := nil;
    Progress_StringGrid.DeleteColRow(false, i);
    FreeAndNil(pd);
  end;
end;

procedure TTmlSidexDemo_Form.ClearMessages;
var
  i, n: Integer;
begin
  n := Messages_StringGrid.RowCount;
  for i := n - 1 downto 1 do
  begin
    Messages_StringGrid.DeleteColRow(false, i);
  end;
end;

procedure TTmlSidexDemo_Form.ClearFileList;
var
  i, n: Integer;
begin
  n := Files_StringGrid.RowCount;
  for i := n - 1 downto 1 do
  begin
    Files_StringGrid.DeleteColRow(false, i);
  end;
end;

procedure TTmlSidexDemo_Form.ClearDownloads;
var
  i, n:    Integer;
  dd:      TDownloadData;
  dlEntry: ^TDownloadListEntry;
begin
  n := Downloads_StringGrid.RowCount;
  for i := n - 1 downto 1 do
  begin
    dd := TDownloadData(Downloads_StringGrid.Objects[0, i]);
    Downloads_StringGrid.Objects[0, i] := nil;
    Downloads_StringGrid.DeleteColRow(false, i);
    if Assigned(dd) then
    begin
      dlEntry := dd.dlEntry;
      if Assigned(dlEntry) then dlEntry^.dd := nil;
      FreeAndNil(dd);
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ClearProgress_MenuItemClick(Sender: TObject);
begin
  ClearProgress;
end;

procedure TTmlSidexDemo_Form.ClearMessages_MenuItemClick(Sender: TObject);
begin
  ClearMessages;
end;

procedure TTmlSidexDemo_Form.RefreshFileList_MenuItemClick(Sender: TObject);
var
  FriendID: TID_Type;
  iFriend, i, n, iRow: Integer;
  fi:       TFriendInfo;
  cmd:      TTMLCmdMsg;
  err:      TML_INT32;
  sdxList:  SIDEX_VARIANT;
  FileList, Entry: variant;
begin
  ClearFileList;
  iFriend := Connections_ListBox.ItemIndex;
  if iFriend >= 0 then
  begin
    FriendID := TID_Type(Connections_ListBox.Items.Objects[iFriend]);
    if FriendID > 0 then
    begin
      fi := FData.GetFriendById(FriendID, true);
      if fi.ID >= 0 then
      begin
        cmd := TTMLCmdMsg.Create(CMD_DEMO_LIST_FILES);
        if Assigned(cmd) then
        begin
          try
            cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
            cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
            cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
            cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
            err := TMLCore.CallSync(TMLProfile.Profile, fi.IP, fi.Port, cmd, 5000);
            if err = TML_SUCCESS then
            begin
              sdxList := cmd.Data.ReadSdxList(GROUP_NAME_ANSWER, PARAM_NAME_LIST);
              FileList := SidexVariantAsVariant(sdxList);
              if VarIsSidexList(FileList) then
              begin
                n := VarSidexListCount(FileList);
                for i := 0 to n - 1 do
                begin
                  Entry := VarSidexListGet(FileList, i);
                  if VarIsStr(Entry) then
                  begin
                    iRow := Files_StringGrid.RowCount;
                    Files_StringGrid.RowCount := iRow + 1;
                    Files_StringGrid.Cells[0, iRow] := Entry;
                  end;
                end;
              end;
            end
            else StatusBar.Panels[2].Text := Format('TMLCore.CallSync Error %d', [err]);
          except
            StatusBar.Panels[2].Text := Format('Error %d on listing files!', [TMLCore.LastError]);
          end;
          FreeAndNil(cmd);
        end;
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ListFilesCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
var
  FileList: variant;
  FileInfo: TSearchRec;
begin
  if not FClosingInProgress then
  begin
    FileList := VarSidexListCreate;
    if VarIsSidexList(FileList) then
    begin
      if FindFirstUTF8(IncludeTrailingPathDelimiter(ProgramDirectory) +
                       GetAllFilesMask, 0, FileInfo) = 0 then
      begin
        repeat
          if (FileInfo.Attr and faDirectory) = 0 then
          begin
            VarSidexListAdd(FileList, FileInfo.Name);
          end;
        until FindNextUTF8(FileInfo) <> 0;
      end;
      FindCloseUTF8(FileInfo);
      acmd.Data.WriteSdxList(GROUP_NAME_ANSWER, PARAM_NAME_LIST,
                             VariantAsSidexVariant(FileList));
    end;
  end;
end;

procedure TTmlSidexDemo_Form.DownloadFile_MenuItemClick(Sender: TObject);
var
  FriendID:   TID_Type;
  i, n, iFriend: Integer;
  RemoteFileName, LocalFileName: string;
  err:        TML_INT32;
  fi:         TFriendInfo;
  cmd:        TTMLCmdMsg;
  StreamID:   TML_STREAM_ID;
  dlEntry:    ^TDownloadListEntry;
begin
  i := Files_StringGrid.Row;
  if i >= 0 then
  begin
    RemoteFileName := Files_StringGrid.Cells[0, i];
    if RemoteFileName <> '' then
    begin
      iFriend := Connections_ListBox.ItemIndex;
      if iFriend >= 0 then
      begin
        FriendID := TID_Type(Connections_ListBox.Items.Objects[iFriend]);
        if FriendID > 0 then
        begin
          fi := FData.GetFriendById(FriendID, true);
          if fi.ID >= 0 then
          begin
            SaveFile_Dialog.FileName := RemoteFileName;
            if SaveFile_Dialog.Execute then
            begin
              LocalFileName := SaveFile_Dialog.FileName;
              Screen.Cursor := crHourGlass;
              StreamID      := TML_STREAM_ID_NULL;
              cmd := TTMLCmdMsg.Create(CMD_DEMO_GET_STREAM_ID);
              if Assigned(cmd) then
              begin
                try
                  cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
                  cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
                  cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
                  cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
                  cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_FILENAME, RemoteFileName);
                  err := TMLCore.CallSync(TMLProfile.Profile, fi.IP, fi.Port, cmd, 10000);
                  if err = TML_SUCCESS then
                  begin
                    StreamID := cmd.Data.ReadInt(GROUP_NAME_ANSWER, PARAM_NAME_ID, TML_STREAM_ID_NULL);
                  end
                  else StatusBar.Panels[2].Text := Format('TMLCore.CallSync Error %d', [err]);
                except
                  StatusBar.Panels[2].Text := Format('Error %d on getting file "%s"!',
                                                     [TMLCore.LastError, RemoteFileName]);
                end;
                FreeAndNil(cmd);
              end;

              if StreamID <> TML_STREAM_ID_NULL then
              begin
                // store stream in list...
                dlEntry := nil;
                try
                  New(dlEntry);
                except
                  dlEntry := nil;
                end;
                if Assigned(dlEntry) then
                begin
                  dlEntry^.dd := TDownloadData.Create;
                  if Assigned(dlEntry^.dd) then
                  begin
                    dlEntry^.dd.dlEntry   := dlEntry;
                    dlEntry^.dd.StreamID  := StreamID;
                    dlEntry^.dd.Name      := LocalFileName;
                    dlEntry^.dd.Progress  := 0;
                    dlEntry^.dd.Finished  := false;
                    dlEntry^.dd.ErrorCode := TML_SUCCESS;
                    dlEntry^.StreamType   := TTMLStreamTypeFile;
                    dlEntry^.Stream       := TTMLStreamTypeFile(
                                               TMLProfile.OpenStream(StreamID,
                                                 TTMLStreamTypeFile,
                                                 fi.IP, fi.Port));
                    if Assigned(dlEntry^.Stream) then
                    begin
                      n := Downloads_StringGrid.RowCount;
                      Downloads_StringGrid.RowCount      := n + 1;
                      Downloads_StringGrid.Objects[0, n] := dlEntry^.dd;
                      Downloads_StringGrid.Cells[1, n]   := dlEntry^.dd.Name;
                      FDownloadList.Add(dlEntry);

                      TTMLStreamTypeFile(dlEntry^.Stream).SaveStreamToFile(
                        LocalFileName, true, @StreamProgress,
                        @StreamFinished, false);
                      // 'dlEntry^.Stream' will be freed in 'StreamFinished'

                      Main_Notebook.ActivePage := Downloads_Page;
                    end
                    else
                    begin
                      FreeAndNil(dlEntry^.dd);
                      Dispose(dlEntry);
                    end;
                  end
                  else Dispose(dlEntry);
                end;
              end;
              Screen.Cursor := crDefault;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.CancelDownload_MenuItemClick(Sender: TObject);
var
  i:       Integer;
  dd:      TDownloadData;
  dlEntry: ^TDownloadListEntry;
begin
  i := Downloads_StringGrid.Row;
  if i >= 0 then
  begin
    dd := TDownloadData(Downloads_StringGrid.Objects[0, i]);
    Downloads_StringGrid.Objects[0, i] := nil;
    Downloads_StringGrid.DeleteColRow(false, i);
    if Assigned(dd) then
    begin
      dlEntry := dd.dlEntry;
      if Assigned(dlEntry) then dlEntry^.dd := nil;
      FreeAndNil(dd);
    end;
  end;
end;

procedure TTmlSidexDemo_Form.CancelAllDownloads_MenuItemClick(Sender: TObject);
begin
  ClearDownloads;
  ReleaseAllDownloads;
end;

procedure TTmlSidexDemo_Form.RemoveFinishedDownloads_MenuItemClick(Sender: TObject);
var
  i, n:    Integer;
  dd:      TDownloadData;
  dlEntry: ^TDownloadListEntry;
begin
  n := Downloads_StringGrid.RowCount;
  for i := n - 1 downto 1 do
  begin
    dd := TDownloadData(Downloads_StringGrid.Objects[0, i]);
    if Assigned(dd) then
    begin
      if dd.Finished then
      begin
        Downloads_StringGrid.Objects[0, i] := nil;
        Downloads_StringGrid.DeleteColRow(false, i);
        dlEntry := dd.dlEntry;
        if Assigned(dlEntry) then dlEntry^.dd := nil;
        FreeAndNil(dd);
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.Downloads_StringGridDblClick(Sender: TObject);
begin
  CancelDownload_MenuItemClick(Sender);
end;

procedure TTmlSidexDemo_Form.Downloads_StringGridDrawCell(Sender: TObject;
  aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  dd:         TDownloadData;
  rcProgress: TRect;
  Progress:   Integer;
  clrProgess: TColor;
  Style:      TTextStyle;
begin
  if (aCol = 0) and (aRow > 0) then
  begin
    dd := TDownloadData(Downloads_StringGrid.Objects[aCol, aRow]);
    if Assigned(dd) then
    begin
      if dd.Finished then Progress := 100
                     else Progress := dd.Progress;

      rcProgress.Left   := aRect.Left + 2;
      rcProgress.Top    := aRect.Top  + 2;
      rcProgress.Right  := rcProgress.Left + ((Progress *
                           (aRect.Right - aRect.Left - 4)) div 100);
      rcProgress.Bottom := aRect.Bottom - 2;

      Downloads_StringGrid.Canvas.Brush.Style := bsSolid;
      if dd.Finished then
      begin
        if dd.ErrorCode <> TML_SUCCESS then clrProgess := clRed
                                       else clrProgess := clLime;
      end
      else clrProgess := clBlue;
      Downloads_StringGrid.Canvas.Brush.Color := clrProgess;
      Downloads_StringGrid.Canvas.FillRect(rcProgress);

      rcProgress.Right := aRect.Right - 2;
      Downloads_StringGrid.Canvas.Brush.Style := bsClear;
      Downloads_StringGrid.Canvas.Pen.Style   := psSolid;
      Downloads_StringGrid.Canvas.Pen.Mode    := pmCopy;
      Downloads_StringGrid.Canvas.Pen.Width   := 1;
      Downloads_StringGrid.Canvas.Pen.Color   := clBlack;
      Downloads_StringGrid.Canvas.Rectangle(rcProgress);

      if dd.Finished and (dd.ErrorCode <> TML_SUCCESS) then
      begin
        Style.Alignment   := taCenter;
        Style.Layout      := tlCenter;
        Style.SingleLine  := true;
        Style.Clipping    := true;
        Style.ExpandTabs  := false;
        Style.ShowPrefix  := false;
        Style.Wordbreak   := false;
        Style.Opaque      := false;
        Style.SystemFont  := false;
        Style.RightToLeft := false;
        Downloads_StringGrid.Canvas.TextRect(rcProgress, 0, 0,
                                             Format('%d', [dd.ErrorCode]),
                                             Style);
      end;

      Downloads_StringGrid.Canvas.Brush.Style := bsSolid;
    end;
  end;
end;

function TTmlSidexDemo_Form.StreamProgress(Sender:   TTMLStreamTypeBase;
                                           StreamID: TML_STREAM_ID;
                                           iBuffer, nBuffers: TML_INT64;
                                           Percent:  TML_INT32): TML_BOOL;
var
  dlList:   TList;
  i, n:     Integer;
  dlEntry:  ^TDownloadListEntry;
  Call:     TAsyncCallData;
  bRepaint: Boolean;
begin
  Result := TML_TRUE; // TML_FALSE = continue, TML_TRUE = Cancel
  if Assigned(Sender) and Assigned(FDownloadList) and
     not FClosingInProgress then
  begin
    bRepaint := false;

    dlList := FDownloadList.LockList;
    if Assigned(dlList) then
    begin
      n := dlList.Count;
      for i := 0 to n - 1 do
      begin
        dlEntry := dlList.Items[i];
        if Assigned(dlEntry) then
        begin
          if dlEntry^.Stream = Sender then
          begin
            if Assigned(dlEntry^.dd) then
            begin
              Result := TML_FALSE;
              dlEntry^.dd.Progress := Percent;
              bRepaint := (Percent <> Sender.LastPercent) or
                          (iBuffer = nBuffers);
            end;
            Break;
          end;
        end;
      end;
    end;
    FDownloadList.UnlockList;

    if bRepaint then
    begin
      // repaint download list...
      Call := TAsyncCallData.Create;
      if Assigned(Call) then
      begin
        Call.ID := ASYNC_REFRESH_DOWNLOADS;
        Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.StreamFinished(Sender:   TTMLStreamTypeBase;
                                            StreamID: TML_STREAM_ID;
                                            errCode:  TML_INT32);
var
  dlList:      TList;
  i, n:        Integer;
  dlEntry:     ^TDownloadListEntry;
  Call:        TAsyncCallData;
  bRepaint:    Boolean;
  bFreeSender: Boolean;
begin
  if Assigned(Sender) then
  begin
    bFreeSender := false;
    if Assigned(FDownloadList) and not FClosingInProgress then
    begin
      bRepaint := false;

      dlList := FDownloadList.LockList;
      if Assigned(dlList) then
      begin
        n := dlList.Count;
        for i := 0 to n - 1 do
        begin
          dlEntry := dlList.Items[i];
          if Assigned(dlEntry) then
          begin
            if dlEntry^.Stream = Sender then
            begin
              // Note: 'Sender' will be freed at the end of this procedure
              bFreeSender         := true;
              dlEntry^.Stream     := nil;
              dlEntry^.StreamType := nil;
              if Assigned(dlEntry^.dd) then
              begin
                dlEntry^.dd.Progress  := 100;
                dlEntry^.dd.Finished  := true;
                dlEntry^.dd.ErrorCode := errCode;
                dlEntry^.dd.dlEntry   := nil;
                dlEntry^.dd           := nil;
                bRepaint := true;
              end;
              Dispose(dlEntry);
              dlList.Delete(i);
              Break;
            end;
          end;
        end;
      end;
      FDownloadList.UnlockList;

      if bRepaint then
      begin
        // repaint download list...
        Call := TAsyncCallData.Create;
        if Assigned(Call) then
        begin
          Call.ID := ASYNC_REFRESH_DOWNLOADS;
          Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
        end;
      end;

      if bFreeSender then
      begin
        // close and free receiver stream...
        FreeAndNil(Sender);
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ReleaseAllDownloads;
var
  dlList:  TList;
  i, n:    Integer;
  dlEntry: ^TDownloadListEntry;
begin
  if Assigned(FDownloadList) then
  begin
    dlList := FDownloadList.LockList;
    if Assigned(dlList) then
    begin
      n := dlList.Count;
      for i := 0 to n - 1 do
      begin
        dlEntry := dlList.Items[i];
        if Assigned(dlEntry) then
        begin
          if Assigned(dlEntry^.dd) then dlEntry^.dd.dlEntry := nil;
          FreeAndNil(dlEntry^.Stream);
          Dispose(dlEntry);
        end;
      end;
      dlList.Clear;
    end;
    FDownloadList.UnlockList;
  end;
end;

procedure TTmlSidexDemo_Form.GetStreamIDCmdCall(Sender: TObject; acmd: TTMLCmdMsg);
var
  RemoteFileName, LocalFileName: string;
  FileStream: TTMLStreamTypeFile;
  fsEntry:    ^TFileStreamListEntry;
  ReceiverHost, ReceiverPort: string;
begin
  if Assigned(FFileStreamList) and not FClosingInProgress then
  begin
    ReceiverHost   := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP, '');
    ReceiverPort   := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, '');
    RemoteFileName := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_FILENAME, '');
    if RemoteFileName <> '' then
    begin
      // Note: This is a simple solution in this demo for building the filename,
      //       real applications should be more carefully with it, because this
      //       simple path appending may cause security vulnerability!
      LocalFileName := IncludeTrailingPathDelimiter(ProgramDirectory) +
                       RemoteFileName;
      FileStream := TTMLStreamTypeFile(TMLProfile.CreateStream(TTMLStreamTypeFile,
                                                               ReceiverHost,
                                                               ReceiverPort,
                                                               @ReceiverStreamClosed,
                                                               @ReceiverStreamFault));
      if Assigned(FileStream) then
      begin
        FileStream.WritePermission := false;
        FileStream.FileName := LocalFileName;

        // store stream in list...
        fsEntry := nil;
        try
          New(fsEntry);
        except
          fsEntry := nil;
        end;
        if Assigned(fsEntry) then
        begin
          fsEntry^.ID         := FileStream.StreamID;
          fsEntry^.Name       := RemoteFileName;
          fsEntry^.Stream     := FileStream;
          fsEntry^.StreamType := TTMLStreamTypeFile;
          FFileStreamList.Add(fsEntry);
          acmd.Data.WriteInt(GROUP_NAME_ANSWER, PARAM_NAME_ID, FileStream.StreamID);
        end
        else FreeAndNil(FileStream);
      end;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ReleaseStreamID(StreamID: TML_STREAM_ID);
var
  fsList:  TList;
  i, n:    Integer;
  fsEntry: ^TFileStreamListEntry;
begin
  if Assigned(FFileStreamList) then
  begin
    if StreamID <> TML_STREAM_ID_NULL then
    begin
      fsList := FFileStreamList.LockList;
      if Assigned(fsList) then
      begin
        n := fsList.Count;
        for i := 0 to n - 1 do
        begin
          fsEntry := fsList.Items[i];
          if Assigned(fsEntry) then
          begin
            if fsEntry^.ID = StreamID then
            begin
              fsEntry^.ID := TML_STREAM_ID_NULL;
              FreeAndNil(fsEntry^.Stream);
              Dispose(fsEntry);
              fsList.Delete(i);
              Break;
            end;
          end;
        end;
      end;
      FFileStreamList.UnlockList;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.ReleaseAllFileStreams;
var
  fsList:  TList;
  i, n:    Integer;
  fsEntry: ^TFileStreamListEntry;
begin
  if Assigned(FFileStreamList) then
  begin
    fsList := FFileStreamList.LockList;
    if Assigned(fsList) then
    begin
      n := fsList.Count;
      for i := 0 to n - 1 do
      begin
        fsEntry := fsList.Items[i];
        if Assigned(fsEntry) then
        begin
          fsEntry^.ID := TML_STREAM_ID_NULL;
          FreeAndNil(fsEntry^.Stream);
          Dispose(fsEntry);
        end;
      end;
      fsList.Clear;
    end;
    FFileStreamList.UnlockList;
  end;
end;

procedure TTmlSidexDemo_Form.ReceiverStreamClosed(Sender:   TTMLStreamTypeBase;
                                                  StreamID: TML_STREAM_ID);
begin
  // close and free sender stream...
  ReleaseStreamID(StreamID);
end;

procedure TTmlSidexDemo_Form.ReceiverStreamFault(Sender:    TTMLStreamTypeBase;
                                                 StreamID:  TML_STREAM_ID;
                                                 ErrorCode: TML_INT32);
begin
  ReceiverStreamClosed(Sender, StreamID);
end;

procedure TTmlSidexDemo_Form.Main_NotebookPageChanged(Sender: TObject);
begin
  if Main_Notebook.ActivePage = Files_Page then
  begin
    RefreshFileList_MenuItemClick(Sender);
  end;
end;

procedure TTmlSidexDemo_Form.Connections_ListBoxClick(Sender: TObject);
begin
  if Main_Notebook.ActivePage = Files_Page then
  begin
    RefreshFileList_MenuItemClick(Sender);
  end;
end;

procedure TTmlSidexDemo_Form.Connections_ListBoxClickCheck(Sender: TObject);
var
  i, n:    Integer;
  CanSend: Boolean;
begin
  CanSend := (Message_Edit.Text <> '');
  if CanSend then
  begin
    CanSend := false;
    n := Connections_ListBox.Count;
    for i := 0 to n - 1 do
    begin
      if Connections_ListBox.Checked[i] then
      begin
        CanSend := true;
        Break;
      end;
    end;
  end;
  SendMessage_Button.Enabled := CanSend;
end;

procedure TTmlSidexDemo_Form.Message_EditChange(Sender: TObject);
begin
  Connections_ListBoxClickCheck(Sender);
  SetDefaultButton(SendMessage_Button);
end;

procedure TTmlSidexDemo_Form.Message_EditClick(Sender: TObject);
begin
  SetDefaultButton(SendMessage_Button);
end;

procedure TTmlSidexDemo_Form.SilenceEnterKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then Key := #0;
end;

procedure TTmlSidexDemo_Form.ListenerIP_EditChange(Sender: TObject);
begin
  SetDefaultButton(StartStopListener_Button);
end;

procedure TTmlSidexDemo_Form.ListenerIP_EditClick(Sender: TObject);
begin
  SetDefaultButton(StartStopListener_Button);
end;

procedure TTmlSidexDemo_Form.ListenerPort_EditChange(Sender: TObject);
begin
  SetDefaultButton(StartStopListener_Button);
end;

procedure TTmlSidexDemo_Form.ListenerPort_EditClick(Sender: TObject);
begin
  SetDefaultButton(StartStopListener_Button);
end;

procedure TTmlSidexDemo_Form.ConnectionIP_EditChange(Sender: TObject);
begin
  SetDefaultButton(Connect_Button);
end;

procedure TTmlSidexDemo_Form.ConnectionIP_EditClick(Sender: TObject);
begin
  SetDefaultButton(Connect_Button);
end;

procedure TTmlSidexDemo_Form.ConnectionPort_EditChange(Sender: TObject);
begin
  SetDefaultButton(Connect_Button);
end;

procedure TTmlSidexDemo_Form.ConnectionPort_EditClick(Sender: TObject);
begin
  SetDefaultButton(Connect_Button);
end;

procedure TTmlSidexDemo_Form.NumberOfPortions_EditChange(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.NumberOfPortions_EditClick(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.DurationForPortion_EditChange(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.DurationForPortion_EditClick(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.NumberOfProgress_EditChange(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.NumberOfProgress_EditClick(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.ProcessingMode_RadioGroupClick(Sender: TObject);
begin
  SetDefaultButton(StartProcessingSomething_Button);
end;

procedure TTmlSidexDemo_Form.SetDefaultButton(AButton: TObject);
begin
  StartStopListener_Button.Default        := (AButton = StartStopListener_Button);
  Connect_Button.Default                  := (AButton = Connect_Button);
  StartProcessingSomething_Button.Default := (AButton = StartProcessingSomething_Button);
  StopProcessingSomething_Button.Default  := (AButton = StopProcessingSomething_Button);
  SendMessage_Button.Default              := (AButton = SendMessage_Button);
end;

procedure TTmlSidexDemo_Form.AsyncCall(pCall: PtrInt);
var
  Call:          TAsyncCallData;
  FreeCallAtEnd: Boolean;
  Found:         Boolean;
  i, n:          Integer;
  pd:            TProgressData;
begin
  // 'AsyncCall' must free 'Call' after use
  Call := TAsyncCallData(pCall);
  if Assigned(Call) then
  begin
    FreeCallAtEnd := true;
    if not FClosingInProgress then
    begin
      case Call.ID of
        ASYNC_LIST_FRIENDS:
        begin
          ListFriends(true);
        end;
        ASYNC_ASK_FOR_FRIENDS,
        ASYNC_ASK_TO_BE_MY_FRIEND,
        ASYNC_ALIVE_CHECK:
        begin
          try
            // the thread will free 'Call' after use
            FreeCallAtEnd := false;
            TDataProcessingThread.Create(FData, Call);
            // the thread will be freed automatically
          except
            on E: Exception do
            begin
              StatusBar.Panels[2].Text := E.Message;
              FreeAndNil(Call);
            end;
          end;
        end;
        ASYNC_DO_CLOSE:
        begin
          FClosingInProgress := true;
          Close;
        end;
        ASYNC_BEGIN_DATA_PROCESSING:
        begin
          FProcessingCount := FProcessingCount + 1;
          ProcessingSomething_Shape.Brush.Color := clBlue;
          ProcessingSomething_Shape.Repaint;
          StatusBar.Panels[2].Text := Call.Text;
        end;
        ASYNC_END_DATA_PROCESSING:
        begin
          FProcessingCount := FProcessingCount - 1;
          if FProcessingCount < 0 then FProcessingCount := 0;
          if (FProcessingCount = 0) and
             (ProcessingSomething_Shape.Brush.Color = clBlue) then
          begin
            ProcessingSomething_Shape.Brush.Color := clSilver;
            ProcessingSomething_Shape.Repaint;
            StatusBar.Panels[2].Text := '';
          end;
        end;
        ASYNC_SHOW_MESSAGE:
        begin
          StatusBar.Panels[2].Text := Call.Text;
        end;
        ASYNC_UPDATE_PROGRESS:
        begin
          Found := false;
          n := Progress_StringGrid.RowCount;
          for i := 1 to n - 1 do
          begin
            pd := TProgressData(Progress_StringGrid.Objects[0, i]);
            if Assigned(pd) then
            begin
              if pd.CommandUID = Call.CommandUID then
              begin
                Found := true;
                if Call.Progress >= 0 then
                begin
                  pd.Progress := Call.Progress;
                  Progress_StringGrid.Cells[1, i] := Call.Text;
                  Progress_StringGrid.Invalidate;
                end
                else
                begin
                  FreeAndNil(pd);
                  Progress_StringGrid.Objects[0, i] := nil;
                  Progress_StringGrid.DeleteColRow(false, i);
                end;
                Break;
              end;
            end;
          end;
          if (Call.Progress >= 0) and not Found then
          begin
            pd := TProgressData.Create;
            if Assigned(pd) then
            begin
              pd.CommandUID := Call.CommandUID;
              pd.Progress   := Call.Progress;
              Progress_StringGrid.RowCount := n + 1;
              Progress_StringGrid.Objects[0, n] := pd;  // stored in grid
              Progress_StringGrid.Cells[1, n] := Call.Text;
            end;
          end;
        end;
        ASYNC_APPEND_MESSAGE:
        begin
          n := Messages_StringGrid.RowCount;
          Messages_StringGrid.RowCount := n + 1;
          Messages_StringGrid.Cells[0, n] := Call.Info.Name;
          Messages_StringGrid.Cells[1, n] := Call.Text;
          Messages_StringGrid.Row := n; // scroll to bottom
          Main_Notebook.ActivePage := Messages_Page;
        end;
        ASYNC_REFRESH_DOWNLOADS:
        begin
          Downloads_StringGrid.Repaint;
        end;
      end;
    end;
    if FreeCallAtEnd then FreeAndNil(Call);
  end;
end;

//----- Send Message Demo ------------------------------------------------------

procedure TTmlSidexDemo_Form.SendMessage_ButtonClick(Sender: TObject);
var
  ID:           TID_Type;
  i, n:         Integer;
  fi:           TFriendInfo;
  EntryWritten: Boolean;
  cmd:          TTMLCmdMsg;
  Call:         TAsyncCallData;
begin
  if Message_Edit.Text <> '' then
  begin
    EntryWritten := false;
    n := Connections_ListBox.Count;
    for i := 0 to n - 1 do
    begin
      if Connections_ListBox.Checked[i] then
      begin
        if not EntryWritten then
        begin
          Call := TAsyncCallData.Create;
          if Assigned(Call) then
          begin
            Call.ID        := ASYNC_APPEND_MESSAGE;
            Call.Text      := Message_Edit.Text;
            Call.Info.Name := FData.MyName;
            Call.Info.IP   := FData.ListenerIP;
            Call.Info.Port := FData.ListenerPort;
            Call.Info.Host := FData.HostName;
            Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
            EntryWritten := true;
          end;
        end;

        ID := TID_Type(Connections_ListBox.Items.Objects[i]);
        if ID > 0 then
        begin
          fi := FData.GetFriendById(ID, true);
          if fi.ID >= 0 then
          begin
            cmd := TTMLCmdMsg.Create(CMD_DEMO_SEND_MESSAGE);
            if Assigned(cmd) then
            begin
              try
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
                cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_MESSAGE, Message_Edit.Text);
                TMLCore.CallAsync(TMLProfile.Profile, fi.IP, fi.Port, cmd, 10000);
                // CallAsync will return immediately, therefore we cannot receive a result!
                // do not free cmd here, it will be automatically freed on CommandReady!
                cmd := nil;
              except
                StatusBar.Panels[2].Text := 'Error sending message!';
              end;
              FreeAndNil(cmd);
            end;
          end;
        end;
      end;
    end;
    if EntryWritten then
    begin
      Message_Edit.Text := '';
      if Message_Edit.CanFocus then Message_Edit.SetFocus;
    end;
  end;
end;

procedure TTmlSidexDemo_Form.SendMessageCmdCall(Sender: TObject;
  acmd: TTMLCmdMsg);
var
  Call: TAsyncCallData;
begin
  Call := TAsyncCallData.Create;
  if Assigned(Call) then
  begin
    Call.Text      := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_MESSAGE, '');
    Call.Info.Name := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_NAME, '');
    if (Call.Text <> '') and (Call.Info.Name <> '') then
    begin
      Call.ID        := ASYNC_APPEND_MESSAGE;
      Call.Info.IP   := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP, '');
      Call.Info.Port := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, '');
      Call.Info.Host := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_HOST, '');
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end
    else FreeAndNil(Call);
  end;
end;

//----- Slider Demo ------------------------------------------------------------

procedure TTmlSidexDemo_Form.Slider_TrackBarChange(Sender: TObject);
var
  cmd: TTMLCmdMsg;
begin
  if not FSliderLocked and not FClosingInProgress then
  begin
    FSliderLocked := true;
    cmd := TTMLCmdMsg.Create(CMD_DEMO_SLIDER);
    if Assigned(cmd) then
    begin
      try
        cmd.Data.WriteInt(GROUP_NAME_SLIDER, PARAM_NAME_VALUE,
                          Slider_TrackBar.Position);
        TMLProfile.SendEvent(cmd);
      except
        StatusBar.Panels[2].Text := 'Error sending event! (Slider)';
      end;
      FreeAndNil(cmd);
    end;
    FSliderLocked := false;
  end;
end;

procedure TTmlSidexDemo_Form.SliderDemoCmdCall(Sender: TObject;
  acmd: TTMLCmdMsg);
begin
  if not FSliderLocked and not FClosingInProgress then
  begin
    FSliderLocked := true;
    Slider_TrackBar.Position := acmd.Data.ReadInt(GROUP_NAME_SLIDER,
                                                  PARAM_NAME_VALUE,
                                                  Slider_TrackBar.Position);
    FSliderLocked := false;
  end;
end;

//----- Processing Data Demo ---------------------------------------------------

procedure TTmlSidexDemo_Form.StartProcessingSomething_ButtonClick(Sender: TObject);
var
  nPortions, Duration, nProgresses, iPortion,
  nFriends, iFriend, Timeout: Integer;
  cmd:           TTMLCmdMsg;
  fi:            TFriendInfo;
  ErrorOccurred: Boolean;
  ShapeColor:    TColor;
  Call:          TAsyncCallData;

procedure MarkError(AEdit: TEdit);
begin
  if Assigned(AEdit) then
  begin
    AEdit.Color := clRed;
    if AEdit.CanFocus then AEdit.SetFocus;
    AEdit.SelectAll;
  end;
  ProcessingSomething_Shape.Brush.Color := clRed;
  ProcessingSomething_Shape.Repaint;
end;

begin
  StopProcessingSomething_Button.Visible  := true;
  StopProcessingSomething_Button.Left     := StartProcessingSomething_Button.Left;
  StopProcessingSomething_Button.Top      := StartProcessingSomething_Button.Top;
  StopProcessingSomething_Button.Width    := StartProcessingSomething_Button.Width;
  StopProcessingSomething_Button.Height   := StartProcessingSomething_Button.Height;
  SetDefaultButton(StopProcessingSomething_Button);
  StartProcessingSomething_Button.Visible := false;
  StopProcessingSomething_Button.Repaint;

  ProcessingSomething_Shape.Brush.Color := clYellow;
  ProcessingSomething_Shape.Repaint;

  FStopProcessing := false;

  if TryStrToInt(NumberOfPortions_Edit.Text, nPortions) and
     (nPortions > 0) then
  begin
    NumberOfPortions_Edit.Color := clWindow;
    if TryStrToInt(DurationForPortion_Edit.Text, Duration) and
       (Duration >= 0) then
    begin
      DurationForPortion_Edit.Color := clWindow;
      if TryStrToInt(NumberOfProgress_Edit.Text, nProgresses) and
         (nProgresses >= 0) then
      begin
        NumberOfProgress_Edit.Color := clWindow;

        Main_Notebook.ActivePage := Progress_Page;

        ErrorOccurred := false;
        nFriends := FData.FriendCount;
        if nFriends > 0 then
        begin
          ProcessingSomething_Shape.Brush.Color := clLime;
          ProcessingSomething_Shape.Repaint;

          iFriend := Random(nFriends);    // choose one friend by fortune
          fi := FData.GetFriendByIndex(iFriend);

          if nProgresses <= 0 then Timeout := (2 * Duration)
          else Timeout := (2 * Duration) div nProgresses;
          if Timeout < 1000 then Timeout := 1000;

          case ProcessingMode_RadioGroup.ItemIndex of
            0, 2: // (Balancer) synchron
            begin
              // we do not want to freeze the GUI,
              // therefore we use a thread here...

              Call := TAsyncCallData.Create;
              if Assigned(Call) then
              begin
                Call.ID        := ASYNC_SYNC_DATA_PROCESS;
                Call.Info      := fi;
                Call.iParam[0] := nPortions;
                Call.iParam[1] := Duration;
                Call.iParam[2] := nProgresses;
                Call.iParam[3] := ProcessingMode_RadioGroup.ItemIndex;
                Call.iParam[4] := Timeout;
                try
                  // the thread will free 'Call' after use
                  TDataProcessingThread.Create(FData, Call);
                  // the thread will be freed automatically
                except
                  on E: Exception do
                  begin
                    StatusBar.Panels[2].Text := E.Message;
                    FreeAndNil(Call);
                  end;
                end;
              end;
            end;
            1, 3: // (Balancer) asynchron
            begin
              for iPortion := 0 to nPortions - 1 do
              begin
                Application.ProcessMessages;
                if FStopProcessing then Break;

                cmd := TTMLCmdMsg.Create(CMD_DEMO_DATA_PROCESSING);
                if Assigned(cmd) then
                begin
                  try
                    cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_IP,   FData.HostIP);
                    cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_PORT, FData.ListenerPort);
                    cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_HOST, FData.HostName);
                    cmd.Data.WriteString(GROUP_NAME_QUERY, PARAM_NAME_NAME, FData.MyName);
                    cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_I_PORTION,    iPortion);
                    cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_N_PORTIONS,   nPortions);
                    cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_DURATION,     Duration);
                    cmd.Data.WriteInt(GROUP_NAME_QUERY, PARAM_NAME_N_PROGRESSES, nProgresses);
                    case ProcessingMode_RadioGroup.ItemIndex of
                      0:  // synchron
                      begin
                        TMLCore.CallSync(TMLProfile.Profile, fi.IP, fi.Port,
                                         cmd, Timeout);
                        // now, we could read a result from cmd
                        FreeAndNil(cmd);
                      end;
                      1:  // asynchron
                      begin
                        TMLCore.CallAsync(TMLProfile.Profile, fi.IP, fi.Port,
                                          cmd, Timeout);
                        // CallAsync will return immediately, therefore we cannot receive a result!
                        // do not free cmd here, it will be automatically freed on CommandReady!
                        cmd := nil;
                      end;
                      2:  // Balancer synchron
                      begin
                        TMLProfile.CallBalSync(cmd, Timeout);
                        // now, we could read a result from cmd
                        FreeAndNil(cmd);
                      end;
                      3:  // Balancer asynchron
                      begin
                        TMLProfile.CallBalAsync(cmd, Timeout);
                        // CallBalAsync will return immediately, therefore we cannot receive a result!
                        // do not free cmd here, it will be automatically freed on CommandReady!
                        cmd := nil;
                      end;
                      else
                      begin
                        FreeAndNil(cmd);
                      end;
                    end;
                  except
                    on E: Exception do
                    begin
                      ErrorOccurred := true;
                      StatusBar.Panels[2].Text := E.Message;
                      Break;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;

             if ErrorOccurred        then ShapeColor := clRed
        else if FProcessingCount > 0 then ShapeColor := clBlue
        else                              ShapeColor := clSilver;
        ProcessingSomething_Shape.Brush.Color := ShapeColor;
        ProcessingSomething_Shape.Repaint;
      end
      else MarkError(NumberOfProgress_Edit);
    end
    else MarkError(DurationForPortion_Edit);
  end
  else MarkError(NumberOfPortions_Edit);

  StartProcessingSomething_Button.Visible := true;
  SetDefaultButton(StartProcessingSomething_Button);
  StopProcessingSomething_Button.Visible  := false;
end;

procedure TTmlSidexDemo_Form.StopProcessingSomething_ButtonClick(Sender: TObject);
begin
  FStopProcessing := true;
end;

procedure TTmlSidexDemo_Form.ProcessingSomethingCmdCall(Sender: TObject;
  acmd: TTMLCmdMsg);
var
  iPortion, nPortions, Duration, nProgresses, i, dD: Integer;
  Call: TAsyncCallData;
begin
  // Note: this is not a synchonized call, do not use the GUI directly!

  acmd.Data.WriteString(GROUP_NAME_PROGRESS, PARAM_NAME_IP,   FData.HostIP);
  acmd.Data.WriteString(GROUP_NAME_PROGRESS, PARAM_NAME_PORT, FData.ListenerPort);
  acmd.Data.WriteString(GROUP_NAME_PROGRESS, PARAM_NAME_HOST, FData.HostName);
  acmd.Data.WriteString(GROUP_NAME_PROGRESS, PARAM_NAME_NAME, FData.MyName);

  if not FClosingInProgress then
  begin
    iPortion    := acmd.Data.ReadInt(GROUP_NAME_QUERY, PARAM_NAME_I_PORTION, 0);
    nPortions   := acmd.Data.ReadInt(GROUP_NAME_QUERY, PARAM_NAME_N_PORTIONS, 0);
    Duration    := acmd.Data.ReadInt(GROUP_NAME_QUERY, PARAM_NAME_DURATION, 0);
    nProgresses := acmd.Data.ReadInt(GROUP_NAME_QUERY, PARAM_NAME_N_PROGRESSES, 0);

    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID        := ASYNC_BEGIN_DATA_PROCESSING;
      Call.Info.IP   := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP,   '');
      Call.Info.Port := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, '');
      Call.Info.Host := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_HOST, '');
      Call.Info.Name := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_NAME, '');
      Call.Text := Format('Processing data %d/%d from %s',
                          [iPortion + 1, nPortions,
                           FormatFriendInfo(Call.Info.IP, Call.Info.Port,
                                            Call.Info.Host, Call.Info.Name)]);
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;

    try
      if nProgresses < 1 then nProgresses := 1;
      if Duration    < 0 then Duration    := 0;
      dD := Duration div nProgresses;
      acmd.Progress := 0;
      for i := 1 to nProgresses do
      begin
        if FClosingInProgress then Break;
        Sleep(dD);
        if FClosingInProgress then Break;
        acmd.Progress := (i * 100) div nProgresses;
      end;
    except
      // ignore errors
    end;

    Call := TAsyncCallData.Create;
    if Assigned(Call) then
    begin
      Call.ID        := ASYNC_END_DATA_PROCESSING;
      Call.Info.IP   := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_IP,   '');
      Call.Info.Port := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_PORT, '');
      Call.Info.Host := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_HOST, '');
      Call.Info.Name := acmd.Data.ReadString(GROUP_NAME_QUERY, PARAM_NAME_NAME, '');
      Application.QueueAsyncCall(@AsyncCall, PtrInt(Call));
    end;
  end;
end;

//------------------------------------------------------------------------------

initialization
  {$I tmlsidexdemo_main.lrs}

end.

