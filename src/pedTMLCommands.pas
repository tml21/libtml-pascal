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

unit pedTMLCommands;

{$if not defined(FPC)}
  {$IF CompilerVersion >= 25.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ifend}

//------------------------------------------------------------------------------

interface

uses
  SysUtils, Variants, Classes, Contnrs, uTMLClasses,
  {$IFDEF FPC}
    PropEdits, ComponentEditors, LResources,
  {$ELSE}
    DesignIntf, DesignEditors, VCLEditors,
    {$ifdef USE_NAMESPACES}
      Vcl.ToolWin,
    {$else}
      ToolWin,
    {$endif}
  {$ENDIF}
  {$ifdef USE_NAMESPACES}
    Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.Controls, Vcl.Graphics,
    Vcl.StdCtrls, Vcl.ComCtrls;
  {$else}
    Forms, Dialogs, Buttons, Controls, Graphics, StdCtrls, ComCtrls;
  {$endif}

//------------------------------------------------------------------------------

type

  { TTMLComponentEditor }

  TpropEdTMLCommands = class;

  { TTMLProfileCommandsEditor }

  TTMLProfileCommandsEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    {$IFDEF FPC}
    function GetValue: AnsiString; override;
    {$ELSE}
    function GetValue: string; override;
    {$ENDIF}

    procedure Edit; override;
  end;

  { TTMLComponentEditor }

  TTMLProfileComponentEditor = class(TComponentEditor)
  public
    {$IFDEF FPC}
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ENDIF}

    destructor Destroy; override;
    function   GetVerbCount: Integer; override;
    function   GetVerb(Index: Integer): string; override;
    procedure  ExecuteVerb(Index: Integer); override;
    procedure  Edit; override;
  end;

  { TpropEdTMLCommands }

  TpropEdTMLCommands = class(TForm{$IFNDEF FPC}, IDesignNotification{$ENDIF})
    frmCloseBtn: TBitBtn;
    lbItems:     TListBox;
    ToolBar:     TToolBar;
    delCmdBtn:   TSpeedButton;
    newCmdBtn:   TSpeedButton;

    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure frmCloseBtnClick(Sender: TObject);
    procedure newCmdBtnClick(Sender: TObject);
    procedure delCmdBtnClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);

    {$IFNDEF FPC}
    { IDesignNotification begin }
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
                               const ASelection: IDesignerSelections); overload;
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    { IDesignNotification end }
    {$ENDIF}

  private
    FProfile  : TTMLProfile;
    {$IFDEF FPC}
    FDesigner : TComponentEditorDesigner;
    {$ELSE}
    FDesigner : IDesigner;
    {$ENDIF}
    FSelCmds  : TObjectList;
    FIgnoreNextSelectionChanged: Boolean;

    procedure RefreshList;
    {$IFDEF FPC}
    function  FindCommand(ACommand : TObject; out AIndex : Integer) : boolean;
    {$ENDIF}
    procedure BuildCaption;
    procedure SelectionChanged; overload;

    {$IFDEF FPC}
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; ASelect: Boolean);
    procedure OnModified(Sender : TObject);
    {$ENDIF}

  protected
    {$IFNDEF FPC}
    procedure Notification(AComponent: TComponent; Operation : TOperation); override;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent; AProfile: TTMLProfile;
                       AComponentEditor: TComponentEditor;
                       APropertyEditor: TPropertyEditor); reintroduce;
    procedure ShowOnTop;
    property  Profile: TTMLProfile read FProfile;
  end;

//------------------------------------------------------------------------------

procedure Register;

//------------------------------------------------------------------------------

resourcestring
  sesCommandsEditorTitle = 'Edit commands';

//------------------------------------------------------------------------------

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

//------------------------------------------------------------------------------

var
  propEdTMLCommandsList: TObjectList;

//------------------------------------------------------------------------------

procedure Register;
begin
  // register custom design time editors
  RegisterPropertyEditor(TypeInfo(TTMLCommands), TTMLProfile, 'Commands', TTMLProfileCommandsEditor);
  RegisterComponentEditor(TTMLProfile, TTMLProfileComponentEditor);
end;

//------------------------------------------------------------------------------

procedure FreePropEdTMLCommandsListObjects;
var
  i:   Integer;
  frm: TpropEdTMLCommands;
begin
  if Assigned(propEdTMLCommandsList) then
  begin
    for i := 0 to propEdTMLCommandsList.Count - 1 do
    begin
      frm := TpropEdTMLCommands(propEdTMLCommandsList[i]);
      if Assigned(frm) then
      begin
        if frm.HandleAllocated then frm.Release;
        propEdTMLCommandsList[i] := nil;
      end;
    end;
    propEdTMLCommandsList.Clear;
  end;
end;

//------------------------------------------------------------------------------

{ TTMLProfileCommandsEditor }

procedure TTMLProfileCommandsEditor.Edit;
var
  lcomp    : TPersistent;
  lProfile : TTMLProfile;
  i        : Integer;
  frm      : TpropEdTMLCommands;
  EditorFrm: TpropEdTMLCommands;
begin
  lcomp := GetComponent(0);
  if Assigned(lcomp) and (lcomp is TTMLProfile) then
  begin
    lProfile := TTMLProfile(lcomp);

    EditorFrm := nil;
    if not Assigned(EditorFrm) then
    begin
      for i := 0 to propEdTMLCommandsList.Count - 1 do
      begin
        frm := TpropEdTMLCommands(propEdTMLCommandsList[i]);
        if Assigned(frm) then
        begin
          if frm.Profile = lProfile then
          begin
            EditorFrm := frm;
            Break;
          end;
        end;
      end;
    end;
    if not Assigned(EditorFrm) then
    begin
      EditorFrm := TpropEdTMLCommands.Create(Application, lProfile, nil, Self);
    end;
    if Assigned(EditorFrm) then EditorFrm.ShowOnTop;
  end;
end;

function TTMLProfileCommandsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{$IFDEF FPC}
function TTMLProfileCommandsEditor.GetValue: AnsiString;
begin
  with GetObjectValue as TTMLCommands do
  begin
    if Count = 1 then
      Result := '1 Command'
    else
      Result := IntToStr(Count) + ' Commands';
  end;
end;
{$ELSE}
function TTMLProfileCommandsEditor.GetValue: string;
begin
  with (GetComponent(0) as TTMLProfile).Commands do
  begin
    if Count = 1 then
      Result := '1 Command'
    else
      Result := IntToStr(Count) + ' Commands';
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

{ TpropEdTMLCommands }

procedure TpropEdTMLCommands.delCmdBtnClick(Sender: TObject);
var
  i    : Integer;
  cmd  : TTMLCommand;
begin
  for i := lbItems.Items.Count - 1 downto 0 do
  begin
    if lbItems.Selected[i] then
    begin
      {$IFDEF FPC}
      cmd := TTMLCommand(lbItems.Items.Objects[i]);
      lbItems.Items.Delete(i);
      FDesigner.PropertyEditorHook.PersistentDeleting(cmd);
      cmd.Free;
      {$ELSE}
      cmd := FProfile.Commands[i];
      FProfile.Commands.Delete(i);
      cmd.Free;
      {$ENDIF}
    end;
  end;
  RefreshList;
  SelectionChanged;
  FDesigner.Modified;
end;

procedure TpropEdTMLCommands.FormClose(Sender: TObject;
  var aAction: TCloseAction);
begin
  aAction := caFree;
end;

procedure TpropEdTMLCommands.frmCloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TpropEdTMLCommands.FormDestroy(Sender: TObject);
begin
  {$IFNDEF FPC}
  UnregisterDesignNotification(Self);
  {$ENDIF}
  propEdTMLCommandsList.Remove(Self);

  if Assigned(FProfile) and
     (not(csDestroying in FProfile.ComponentState)) then
  begin
    {$IFDEF FPC}
    GlobalDesignHook.SelectOnlyThis(FProfile);
    {$ELSE}
    FDesigner.SelectComponent(FProfile);
    {$ENDIF}
  end;

  {$IFDEF FPC}
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  end;
  {$ENDIF}

  FreeAndNil(FSelCmds);
end;

procedure TpropEdTMLCommands.lbItemsClick(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TpropEdTMLCommands.newCmdBtnClick(Sender: TObject);
var
  cmd : TTMLCommand;
begin
  //Add an item to the collection
  {$IFDEF FPC}
  cmd      := TTMLCommand.Create(FProfile.Owner);
  FProfile.Commands.Add(cmd);
  cmd.Name := FDesigner.CreateUniqueComponentName('cmd');
  FDesigner.PropertyEditorHook.PersistentAdded(cmd, true);
  RefreshList;
  {$ELSE}
  cmd := TTMLCommand(FDesigner.CreateComponent(TTMLCommand, FProfile, 0, 0, 0, 0));
  cmd.Name := FDesigner.UniqueName('cmd');
  FProfile.Commands.Add(cmd);

  RefreshList;
  lbitems.Selected[lbitems.Count-1];
  SelectionChanged;
  {$ENDIF}
  FDesigner.Modified;
end;

{$IFNDEF FPC}
procedure TpropEdTMLCommands.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    //If the owner component is destroyed
    //we should close our form
    if AComponent = FProfile then
    begin
      Close;
    end
    //If the component that is destroyed
    //we refresh our list just incase it affects our component
    else if AComponent is TTMLCommand then
    begin
      RefreshList;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.ItemDeleted(const ADesigner: IDesigner;
                                               AItem:     TPersistent);
begin
  // ignore
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.ItemInserted(const ADesigner: IDesigner;
                                                AItem:     TPersistent);
begin
  // ignore
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.ItemsModified(const ADesigner: IDesigner);
begin
  RefreshList;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.SelectionChanged(const ADesigner:  IDesigner;
                                              const ASelection: IDesignerSelections);
begin
  if not FIgnoreNextSelectionChanged then
  begin
    if Assigned(FSelCmds) then FSelCmds.Clear;
    lbItems.ClearSelection;
  end
  else FIgnoreNextSelectionChanged := false;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.DesignerOpened(const ADesigner:     IDesigner;
                                                  AResurrecting: Boolean);
begin
  // ignore
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TpropEdTMLCommands.DesignerClosed(const ADesigner:     IDesigner;
                                                  AGoingDormant: Boolean);
begin
  FreePropEdTMLCommandsListObjects;
end;
{$ENDIF}

constructor TpropEdTMLCommands.Create(AOwner: TComponent; AProfile : TTMLProfile;
  AComponentEditor: TComponentEditor; APropertyEditor: TPropertyEditor);
begin
  inherited Create(AOwner);

  FProfile := AProfile;
  if Assigned(AComponentEditor) then
  begin
    FDesigner := AComponentEditor.Designer;
  end
  else
  begin
    {$IFDEF FPC}
    FDesigner := FindRootDesigner(AProfile) as TComponentEditorDesigner;
    {$ELSE}
    FDesigner := APropertyEditor.Designer;
    {$ENDIF}
  end;

  if FDesigner = nil then
    raise Exception.Create('TpropEdTMLCommands.FDesigner=nil');

  propEdTMLCommandsList.Add(Self);

  FIgnoreNextSelectionChanged := false;
  FSelCmds := TObjectList.Create;
  if Assigned(FSelCmds) then FSelCmds.OwnsObjects := false;

  RefreshList;

  {$IFDEF FPC}
  GlobalDesignHook.AddHandlerComponentRenamed(OnComponentRenamed);
  GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
  GlobalDesignHook.AddHandlerGetSelection(OnGetSelection);
  GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
  GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
  GlobalDesignHook.AddHandlerModified(OnModified);
  {$ELSE}
  //First we need to remove notification for the current component
  if Assigned(FProfile) then
  begin
    FProfile.RemoveFreeNotification(Self);
    //Now we need to add notification for the current component
    FProfile.FreeNotification(Self);
  end;
  {$ENDIF}

  BuildCaption;

  {$IFNDEF FPC}
  RegisterDesignNotification(Self);
  {$ENDIF}
end;

procedure TpropEdTMLCommands.RefreshList;
var
  i           : Integer;
  cmd         : TTMLCommand;
  cid         : Cardinal;
  cids, syncs : string;
begin
  lbItems.Clear;
  for i := 0 to FProfile.Commands.Count - 1 do
  begin
    cmd := FProfile.Commands[i];
    if Assigned(cmd) then
    begin
      cid  := cmd.CommandId;
      cids := IntToHex(cid, 4) + ' (' + IntToStr(cid) + ')';
      if cmd.Synchronize then syncs := ' (sync)'
                         else syncs := '';
    end
    else
    begin
      cids  := 'n/a';
      syncs := '';
    end;
    lbItems.Items.AddObject(IntToStr(i) + ' - ' + cids + ' : ' +
                            FProfile.Commands[i].Name + syncs, cmd);
  end;
  if Assigned(FSelCmds) then
  begin
    for i := 0 to lbItems.Items.Count - 1 do
    begin
      lbItems.Selected[i] := (FSelCmds.IndexOf(lbItems.Items.Objects[i]) >= 0);
    end;
  end;
end;

{$IFDEF FPC}
function TpropEdTMLCommands.FindCommand(ACommand: TObject;
                                        out AIndex: Integer): Boolean;
begin
  if ACommand is TTMLCommand then
    AIndex := FProfile.Commands.IndexOf(ACommand as TTMLCommand)
  else
    AIndex := -1;
  Result := AIndex >= 0;
end;
{$ENDIF}

procedure TpropEdTMLCommands.BuildCaption;
begin
  Caption := 'Commands - ' + FProfile.Name;
end;

{$IFDEF FPC}
procedure TpropEdTMLCommands.SelectionChanged;
var
  sel:  TPersistentSelectionList;
  i, n: Integer;
begin
  GlobalDesignHook.RemoveHandlerSetSelection(OnSetSelection);
  try
    sel := TPersistentSelectionList.Create;
    try
      OnGetSelection(sel);
      if Assigned(FSelCmds) then
      begin
        FSelCmds.Clear;
        n := sel.Count;
        for i := 0 to n - 1 do FSelCmds.Add(sel.Items[i]);
      end;
      FDesigner.PropertyEditorHook.SetSelection(sel);
    finally
      sel.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
  end;
end;
{$ELSE}
procedure TpropEdTMLCommands.SelectionChanged;
var
  sel: IDesignerSelections;
  i:   Integer;
begin
  if Assigned(FDesigner) and Assigned(FProfile) then
  begin
    if Assigned(FSelCmds) then FSelCmds.Clear;
    if lbitems.SelCount = 1 then
    begin
      i := lbitems.ItemIndex;
      if i >= 0 then
      begin
        if Assigned(FSelCmds) then FSelCmds.Add(lbItems.Items.Objects[i]);
        if i < FProfile.Commands.Count then
        begin
          FIgnoreNextSelectionChanged := true;
          FDesigner.SelectComponent(FProfile.Commands[i]);
        end;
      end;
    end
    else
    begin
      // Mehrere Elemente gewählt
      sel := CreateSelectionList;
      for i := 0 to lbItems.Count - 1 do
      begin
        if lbItems.Selected[i] then
        begin
          if Assigned(FSelCmds) then FSelCmds.Add(lbItems.Items.Objects[i]);
          if i < FProfile.Commands.Count then
          begin
            sel.Add(FProfile.Commands[i]);
          end;
        end;
      end;
      FIgnoreNextSelectionChanged := true;
      FDesigner.SetSelections(sel);
    end;
  end;
end;
{$ENDIF}

procedure TpropEdTMLCommands.ShowOnTop;
begin
  Visible := True;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  BringToFront;
end;

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  if AComponent = nil then exit;

  if FindCommand(AComponent, i) then
  begin
    //lbItems.Items[i] := AComponent.Name;
    RefreshList;
  end
  else if AComponent = FProfile then
  begin
    BuildCaption;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnPersistentDeleting(APersistent: TPersistent);
var
  i: Integer;
begin
  if FindCommand(APersistent, i) then
  begin
    lbItems.Items.Delete(i);
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
begin
  if not Assigned(ASelection) then Exit;
  ASelection.Clear;
  with lbItems do
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if Selected[i] then
      begin
        ASelection.Add(TPersistent(Items.Objects[i]));
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if ASelection = nil then exit;
  lbItems.ClearSelection;
  for i := 0 to ASelection.Count - 1 do
    if FindCommand(ASelection.Items[i], j) then
      lbItems.Selected[j] := true;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnPersistentAdded(APersistent: TPersistent;
  ASelect: Boolean);
var
  i: Integer;
  c: TTMLCommand;
begin
  if (APersistent = nil) or not (APersistent is TTMLCommand) then exit;
  c := APersistent as TTMLCommand;
  if c.Profile <> FProfile then exit;
  RefreshList;
  i := lbItems.Items.IndexOfObject(c);
  lbItems.Selected[i] := ASelect;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TpropEdTMLCommands.OnModified(Sender: TObject);
var
   i : Integer;
begin
  if (Sender = nil) or not (Sender is TPropertyEditor) then exit;
  if FindCommand(TPropertyEditor(Sender).GetComponent(0),i) then
  begin
    RefreshList;
    lbItems.Selected[i] := True;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

{ TTMLProfileComponentEditor }

{$IFDEF FPC}
constructor TTMLProfileComponentEditor.Create(AComponent: TComponent;
                                              ADesigner:  TComponentEditorDesigner);
{$ELSE}
constructor TTMLProfileComponentEditor.Create(AComponent: TComponent;
                                              ADesigner:  IDesigner);
{$ENDIF}
begin
  inherited Create(AComponent, ADesigner);
end;

destructor TTMLProfileComponentEditor.Destroy;
begin
  inherited Destroy;
end;

function TTMLProfileComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTMLProfileComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sesCommandsEditorTitle;
    else Result := '';
  end;
end;

procedure TTMLProfileComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index <> 0 then Exit;
  Edit;
end;

procedure TTMLProfileComponentEditor.Edit;
var
  lcomp:     TComponent;
  lprofile:  TTMLProfile;
  i:         Integer;
  frm:       TpropEdTMLCommands;
  EditorFrm: TpropEdTMLCommands;
begin
  lcomp := GetComponent;
  if Assigned(lcomp) and (lcomp is TTMLProfile) then
  begin
    lprofile  := TTMLProfile(lcomp);
    EditorFrm := nil;
    if not Assigned(EditorFrm) then
    begin
      for i := 0 to propEdTMLCommandsList.Count - 1 do
      begin
        frm := TpropEdTMLCommands(propEdTMLCommandsList[i]);
        if Assigned(frm) then
        begin
          if frm.Profile = lProfile then
          begin
            EditorFrm := frm;
            Break;
          end;
        end;
      end;
    end;
    if not Assigned(EditorFrm) then
    begin
      EditorFrm := TpropEdTMLCommands.Create(Application, lprofile, Self, nil);
    end;
    if Assigned(EditorFrm) then EditorFrm.ShowOnTop;
  end;
end;

//------------------------------------------------------------------------------

initialization
  propEdTMLCommandsList := TObjectList.Create;
  if Assigned(propEdTMLCommandsList) then
  begin
    propEdTMLCommandsList.OwnsObjects := false;
  end;

{$IFDEF FPC}
  {$I pedTMLCommands.lrs}
{$ENDIF}

//------------------------------------------------------------------------------

finalization
  FreePropEdTMLCommandsListObjects;
  FreeAndNil(propEdTMLCommandsList);

//------------------------------------------------------------------------------

end.

