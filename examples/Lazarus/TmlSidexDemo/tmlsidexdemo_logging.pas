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

unit TmlSidexDemo_Logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTMLClasses, StdCtrls;

type

  { TLoggingDlg }

  TLoggingDlg = class(TForm)
    Options_GroupBox: TGroupBox;
    SetAll_Button: TButton;
    Clear_Button: TButton;
    Invert_Button: TButton;
    Close_Button: TButton;
    TML_LOG_CONNECTION_OBJ_HANDLING_CheckBox: TCheckBox;
    TML_LOG_CORE_API_CheckBox: TCheckBox;
    TML_LOG_CORE_IO_CheckBox: TCheckBox;
    TML_LOG_INTERNAL_DISPATCH_CheckBox: TCheckBox;
    TML_LOG_MULTY_SYNC_CMDS_CheckBox: TCheckBox;
    TML_LOG_STREAM_HANDLING_CheckBox: TCheckBox;
    TML_LOG_VORTEX_CH_POOL_CheckBox: TCheckBox;
    TML_LOG_VORTEX_CMD_CheckBox: TCheckBox;
    TML_LOG_VORTEX_FRAMES_CheckBox: TCheckBox;
    TML_LOG_VORTEX_HASHTABLE_CheckBox: TCheckBox;
    TML_LOG_VORTEX_INTERN_CheckBox: TCheckBox;
    TML_LOG_VORTEX_MUTEX_CheckBox: TCheckBox;
    TML_LOG_VORTEX_VERBOSE_CheckBox: TCheckBox;
    procedure Clear_ButtonClick(Sender: TObject);
    procedure Close_ButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Invert_ButtonClick(Sender: TObject);
    procedure SetAll_ButtonClick(Sender: TObject);

  private
    FTMLCore: TTMLCore;

    FCBs: Array[0..12] of TCheckBox;

  public
    property TMLCore: TTMLCore read FTMLCore write FTMLCore;
  end;

var
  LoggingDlg: TLoggingDlg;

implementation

uses
  uTMLCore, uTMLErrors;

{ TLoggingDlg }

procedure TLoggingDlg.FormShow(Sender: TObject);
var
  i, iLogValue: Integer;
  bSuccess:     Boolean;
begin
  bSuccess  := false;
  iLogValue := TML_LOG_OFF;
  if Assigned(FTMLCore) then
  begin
    bSuccess := (tml_Core_Get_LoggingValue(FTMLCore.TMLCoreHandle,
                                           iLogValue) = TML_SUCCESS);
  end;

  FCBs[ 0] := TML_LOG_VORTEX_INTERN_CheckBox;
  FCBs[ 1] := TML_LOG_VORTEX_VERBOSE_CheckBox;
  FCBs[ 2] := TML_LOG_VORTEX_CMD_CheckBox;
  FCBs[ 3] := TML_LOG_CORE_IO_CheckBox;
  FCBs[ 4] := TML_LOG_CORE_API_CheckBox;
  FCBs[ 5] := TML_LOG_VORTEX_FRAMES_CheckBox;
  FCBs[ 6] := TML_LOG_VORTEX_CH_POOL_CheckBox;
  FCBs[ 7] := TML_LOG_VORTEX_MUTEX_CheckBox;
  FCBs[ 8] := TML_LOG_MULTY_SYNC_CMDS_CheckBox;
  FCBs[ 9] := TML_LOG_VORTEX_HASHTABLE_CheckBox;
  FCBs[10] := TML_LOG_INTERNAL_DISPATCH_CheckBox;
  FCBs[11] := TML_LOG_CONNECTION_OBJ_HANDLING_CheckBox;
  FCBs[12] := TML_LOG_STREAM_HANDLING_CheckBox;

  for i := Low(FCBs) to High(FCBs) do
  begin
    FCBs[i].Checked := ((iLogValue and FCBs[i].Tag) <> 0);
    FCBs[i].Enabled := bSuccess;
  end;
end;

procedure TLoggingDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i, iLogValue: Integer;
begin
  if Assigned(FTMLCore) and FCBs[0].Enabled then
  begin
    iLogValue := TML_LOG_OFF;
    for i := Low(FCBs) to High(FCBs) do
    begin
      if FCBs[i].Checked then iLogValue := iLogValue or FCBs[i].Tag;
    end;
    tml_Core_Set_LoggingValue(FTMLCore.TMLCoreHandle, iLogValue);
  end;
  CloseAction := caHide;
end;

procedure TLoggingDlg.Close_ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TLoggingDlg.SetAll_ButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(FCBs) to High(FCBs) do FCBs[i].Checked := true;
end;

procedure TLoggingDlg.Clear_ButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(FCBs) to High(FCBs) do FCBs[i].Checked := false;
end;

procedure TLoggingDlg.Invert_ButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(FCBs) to High(FCBs) do FCBs[i].Checked := not FCBs[i].Checked;
end;

initialization
  {$I tmlsidexdemo_logging.lrs}

end.

