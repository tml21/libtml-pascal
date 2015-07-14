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

unit cSidexDocument;

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
  uSidexLib, SysUtils, Variants, uSidexTypes;

//------------------------------------------------------------------------------

type

ESidexError = class(Exception);

{ SIDEX Document class}
TSIDEXDocument = class(TObject)
private
  function  GetContent: string;
  function  GetContentLength: Integer;
  procedure SetContent(const Value: string);
  function  GetGroupByIndex(i: Integer): string;
  function  GetGroupCount: Integer;
  function  GetKeyByIndex(const group: string; i: Integer): string;
  function  GetKeyCount(const group: string): Integer;
  function  GetDocumentName: string;
  procedure SetDocumentName(const Value: string);

protected
  FSidexHandle : SIDEX_HANDLE;
  FIsOwner     : Boolean;
  FLastError   : Integer;

public
  constructor Create; overload;
  constructor Create(const aname : string); overload;
  constructor Create(ahandle : SIDEX_HANDLE; TakeOwnership: Boolean = false); overload;
  destructor  Destroy; override;

  { content managment }
  procedure LoadContent(const afilename : string);
  procedure SaveContent(const afilename : string);
  procedure Clear;

  { navigation }
  function  HasGroup(const group: string): Boolean;
  function  HasKey(const group: string; const key : string): Boolean;

  function  GetGroups: TSIDEXStringArray;
  function  GetKeys(const GroupName: string): TSIDEXStringArray;

  procedure DeleteGroup(const GroupName: string);
  procedure DeleteKey(const GroupName, KeyName: string);

  { reading and writing data with specific types }
  function  GetStringLength(const grp, key : string) : Integer;
  procedure WriteString(const grp, key : string; const value : string);
  function  ReadString(const grp, key : string; const adefault : string) : string;

  procedure WriteInt(const grp, key : string; const value : Int64);
  function  ReadInt(const grp, key : string; const adefault : Int64) : Int64;

  procedure WriteFloat(const grp, key : string; const value : SIDEX_DOUBLE);
  function  ReadFloat(const grp, key : string; const adefault : SIDEX_DOUBLE) : SIDEX_DOUBLE;

  procedure WriteBoolean(const grp, key : string; const value : Boolean);
  function  ReadBoolean(const grp, key : string; const adefault : Boolean) : Boolean;

  procedure WriteDateTime(const grp, key : string; const value : TDateTime);
  function  ReadDateTime(const grp, key : string; const adefault : TDateTime) : TDateTime;

  function  GetBinaryLength(const grp, key : string) : Integer;
  procedure WriteBinary(const grp, key : string; const data; dataLength : Integer);
  function  ReadBinary(const grp, key : string; var data; maxDataLength : Integer) : Integer;
  // ReadBinary returns the number of bytes stored in 'data'.
  // ReadBinary returns -1, if 'maxDataLength' is not big enough.
  // ReadBinary returns -2, if an error occurred.

  { SIDEX_HANDLE methods for direct access }
  procedure WriteSdxVariant(const grp, key : string; const value : SIDEX_VARIANT);
  function  ReadSdxVariant(const grp, key : string; const adefault : SIDEX_VARIANT) : SIDEX_VARIANT;

  procedure WriteSdxList(const grp, key : string; const value : SIDEX_VARIANT);
  function  ReadSdxList(const grp, key : string) : SIDEX_VARIANT;

  procedure WriteSdxDict(const grp, key : string; const value : SIDEX_VARIANT);
  function  ReadSdxDict(const grp, key : string) : SIDEX_VARIANT;

  procedure WriteSdxTable(const grp, key : string; const value : SIDEX_VARIANT);
  function  ReadSdxTable(const grp, key : string) : SIDEX_VARIANT;

  function  GetSdxTypeOfValue(const group, key: string): SIDEX_DATA_TYPE;

  { reading and writing variants }
  function  GetTypeOfValue(const group, key: string): TVarType;

  procedure WriteValue(const group, key: string; const val: Variant);
  function  ReadValue(const group, key: string; const def: Variant): Variant;

  { properties }
  property Name:                     string read GetDocumentName write SetDocumentName;
  property Content:                  string read GetContent write SetContent;
  property ContentLength:           Integer read GetContentLength;
  property Handle:             SIDEX_HANDLE read FSidexHandle;
  property LastError:               Integer read FLastError;
  property GroupCount:              Integer read GetGroupCount;
  property GroupByIndex[i: Integer]: string read GetGroupByIndex;
  property KeyCount[const group: string]: Integer read GetKeyCount;
  property KeyByIndex[const group: string; i: Integer]: string Read GetKeyByIndex;
end;

//------------------------------------------------------------------------------

implementation

uses
  {$if defined(ANDROID)}
    FMX.Dialogs,
  {$elseif defined(USE_NAMESPACES)}
    Vcl.Dialogs,
  {$else}
    Dialogs,
  {$ifend}
  Classes, uSidexVariant, uSidexErrors;

//------------------------------------------------------------------------------

{ TSIDEXDocument }

constructor TSIDEXDocument.Create;
begin
  Create('Sidex');
end;

constructor TSIDEXDocument.Create(const aname: string);
begin
  Inherited Create;

  FIsOwner   := True;
  FLastError := sidex_Create(PSIDEX_STRING(aname), FSidexHandle);
  if FLastError <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('cannot create sidex handle (%d)',  [FLastError]);
  end;
end;

constructor TSIDEXDocument.Create(ahandle: SIDEX_HANDLE; TakeOwnership: Boolean);
begin
  Inherited Create;

  if ahandle = SIDEX_HANDLE_NULL then
  begin
    raise Exception.Create('invalid sidex handle');
  end;

  FIsOwner     := TakeOwnership;
  FLastError   := SIDEX_SUCCESS;
  FSidexHandle := ahandle;
end;

destructor TSIDEXDocument.Destroy;
begin
  if FIsOwner then sidex_Free(FSidexHandle);
  FSidexHandle := SIDEX_HANDLE_NULL;
  inherited Destroy;
end;

function TSIDEXDocument.GetDocumentName: string;
var
  err: SIDEX_INT32;
  pcs: PSIDEX_STRING;
begin
  Result := '';
  pcs    := nil;
  err    := sidex_Get_DocumentName(FSidexHandle, pcs);  // borrowed
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Get_DocumentName error (%d)', [err]);
  end;
  if Assigned(pcs) then Result := pcs;
end;

procedure TSIDEXDocument.SetDocumentName(const Value: string);
var
  err: SIDEX_INT32;
begin
  err := sidex_Set_DocumentName(FSidexHandle, PSIDEX_STRING(Value));
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Set_DocumentName error (%d)', [err]);
  end;
end;

function TSIDEXDocument.GetContent: string;
var
  lcontent:  PSIDEX_STRING;
  size, err: SIDEX_INT32;
begin
  size := 0;
  err := sidex_Get_Content(FSidexHandle, lcontent, size);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Get_Content error (%d)', [err]);
  end;
  Result := lcontent;
  sidex_Free_Content(lcontent);
end;

function TSIDEXDocument.GetContentLength: Integer;
var
  size, err: SIDEX_INT32;
begin
  size := 0;
  err := sidex_Get_Content_Length(FSidexHandle, size);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Get_Content_Length error (%d)', [err]);
  end;
  Result := size;
end;

procedure TSIDEXDocument.Clear;
begin
  sidex_Clear(FSidexHandle);
end;

function TSIDEXDocument.GetGroupByIndex(i: Integer): string;
var
  groups:     SIDEX_VARIANT;
  value:      SIDEX_VARIANT;
  pstring:    PSIDEX_STRING;
  asize, err: SIDEX_INT32;
begin
  err := sidex_GetGroups(FSidexHandle, groups);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetGroups error (%d)', [err]);
  end;

  err := sidex_Variant_List_Get(groups, i, value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(groups);
    raise ESidexError.CreateFmt('sidex_Variant_List_Get  error (%d)', [err]);
  end;

  asize := 0;
  err := sidex_Variant_As_String(value, pstring, asize);  // borrowed
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(groups);
    raise ESidexError.CreateFmt('sidex_Variant_As_String  error (%d)', [err]);
  end;

  Result := pstring;
  sidex_Variant_DecRef(groups);
end;

function TSIDEXDocument.GetGroupCount: Integer;
var
  groups:     SIDEX_VARIANT;
  asize, err: SIDEX_INT32;
begin
  err := sidex_GetGroups(FSidexHandle, groups);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetGroups error (%d)', [err]);
  end;

  asize := 0;
  err := sidex_Variant_List_Size(groups, asize);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(groups);
    raise ESidexError.CreateFmt('sidex_Variant_List_Size  error (%d)', [err]);
  end;

  Result := asize;
  sidex_Variant_DecRef(groups);
end;

function TSIDEXDocument.GetKeyByIndex(const group: string; i: Integer): string;
var
  keys:       SIDEX_VARIANT;
  value:      SIDEX_VARIANT;
  pstring:    PSIDEX_STRING;
  asize, err: SIDEX_INT32;
begin
  err := sidex_GetKeys(FSidexHandle, PSIDEX_STRING(group), keys);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetKeys error (%d)', [err]);
  end;

  err := sidex_Variant_List_Get(keys, i, value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(keys);
    raise ESidexError.CreateFmt('sidex_Variant_List_Get  error (%d)', [err]);
  end;

  asize := 0;
  err := sidex_Variant_As_String(value, pstring, asize);  // borrowed
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(keys);
    raise ESidexError.CreateFmt('sidex_Variant_As_String  error (%d)', [err]);
  end;

  Result := pstring;
  sidex_Variant_DecRef(keys);
end;

function TSIDEXDocument.GetKeyCount(const group: string): Integer;
var
  keys:       SIDEX_VARIANT;
  asize, err: SIDEX_INT32;
begin
  err := sidex_GetKeys(FSidexHandle, PSIDEX_STRING(group), keys);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetKeys error (%d)', [err]);
  end;

  asize := 0;
  err := sidex_Variant_List_Size(keys, asize);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(keys);
    raise ESidexError.CreateFmt('sidex_Variant_List_Size  error (%d)', [err]);
  end;

  Result := asize;
  sidex_Variant_DecRef(keys);
end;

function TSIDEXDocument.GetSdxTypeOfValue(const group, key: string): SIDEX_DATA_TYPE;
var
  value: SIDEX_VARIANT;
  err:   SIDEX_INT32;
begin
  err := sidex_Variant_Read(FSidexHandle, PSIDEX_STRING(group),
                                          PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Read error (%d)', [err]);
  end;

  Result := sidex_Variant_GetType(value);
  sidex_Variant_DecRef(value);
end;

function TSIDEXDocument.GetTypeOfValue(const group, key: string): TVarType;
begin
  case GetSdxTypeOfValue(group, key) of
    SIDEX_DATA_TYPE_NONE      : Result := varNull;
    SIDEX_DATA_TYPE_BOOLEAN   : Result := varBoolean;
    SIDEX_DATA_TYPE_INTEGER   : Result := varInt64;
    SIDEX_DATA_TYPE_FLOAT     : Result := varDouble;
    SIDEX_DATA_TYPE_STRING    : Result := varString;
    SIDEX_DATA_TYPE_LIST      : Result := varSidexList;
    SIDEX_DATA_TYPE_DICT      : Result := varSidexDict;
    SIDEX_DATA_TYPE_TABLE     : Result := varSidexTable;
    SIDEX_DATA_TYPE_DATETIME  : Result := varDate;
    else raise ESidexError.Create('unknown SIDEX type');
  end;
end;

function TSIDEXDocument.HasGroup(const group: string): Boolean;
begin
  Result := (sidex_HasGroup(FSidexHandle,
                            PSIDEX_STRING(group)) <> SIDEX_FALSE);
end;

function TSIDEXDocument.HasKey(const group, key: string): Boolean;
begin
  Result := (sidex_HasKey(FSidexHandle, PSIDEX_STRING(group),
                          PSIDEX_STRING(key)) <> SIDEX_FALSE);
end;

function TSIDEXDocument.GetGroups: TSIDEXStringArray;
var
  err, i, n:  SIDEX_INT32;
  sdxGrpList: SIDEX_VARIANT;
  vGroups:    Variant;
begin
  err := sidex_GetGroups(FSidexHandle, sdxGrpList);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetGroups error (%d)', [err]);
  end;

  try
    vGroups := SidexVariantAsVariant(sdxGrpList);
    n := VarSidexListCount(vGroups);
    SetLength(Result, n);
    for i := 0 to n - 1 do
    begin
      Result[i] := VarSidexListGet(vGroups, i);
    end;
  finally
    sidex_Variant_DecRef(sdxGrpList);
  end;
end;

function TSIDEXDocument.GetKeys(const GroupName: string): TSIDEXStringArray;
var
  err, i, n:  SIDEX_INT32;
  sdxKeyList: SIDEX_VARIANT;
  vKeys:      Variant;
begin
  err := sidex_GetKeys(FSidexHandle, PSIDEX_STRING(GroupName), sdxKeyList);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_GetKeys error (%d)', [err]);
  end;

  try
    vKeys := SidexVariantAsVariant(sdxKeyList);
    n := VarSidexListCount(vKeys);
    SetLength(Result, n);
    for i := 0 to n - 1 do
    begin
      Result[i] := VarSidexListGet(vKeys, i);
    end;
  finally
    sidex_Variant_DecRef(sdxKeyList);
  end;
end;

procedure TSIDEXDocument.DeleteGroup(const GroupName: string);
begin
  FLastError := sidex_DeleteGroup(FSidexHandle, PSIDEX_STRING(GroupName));
end;

procedure TSIDEXDocument.DeleteKey(const GroupName, KeyName: string);
begin
  FLastError := sidex_DeleteKey(FSidexHandle, PSIDEX_STRING(GroupName),
                                              PSIDEX_STRING(KeyName));
end;

procedure TSIDEXDocument.LoadContent(const afilename: string);
var
  err: SIDEX_INT32;
begin
  err := sidex_Load_Content(FSidexHandle, PSIDEX_STRING(afilename));
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Load_Content error (%d)', [err]);
  end;
end;

function TSIDEXDocument.ReadBoolean(const grp, key: string;
                                    const adefault: Boolean): Boolean;
var
  bValue: SIDEX_BOOL;
  err:    SIDEX_INT32;
begin
  if not HasKey(grp, key) then
  begin
    Result := adefault;
  end else
  begin
    err := sidex_Boolean_Read(FSidexHandle, PSIDEX_STRING(grp),
                                            PSIDEX_STRING(key), bValue);
    FLastError := err;
    if err <> SIDEX_SUCCESS then
      raise ESidexError.CreateFmt('sidex_Boolean_Read error (%d)', [err])
    else
      Result := (bValue <> SIDEX_FALSE);
  end;
end;

function TSIDEXDocument.ReadDateTime(const grp, key: string;
                                     const adefault: TDateTime): TDateTime;
var
  err: SIDEX_INT32;
  dtv: SIDEX_VARIANT;
begin
  err := sidex_DateTime_Read(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key), dtv);
  FLastError := err;
  if err <> SIDEX_SUCCESS then Result := adefault
  else
  begin
    try
      Result := SidexVariantAsVariant(dtv);
    except
      Result := adefault;
    end;
    sidex_Variant_DecRef(dtv);
  end;
end;

function TSIDEXDocument.ReadFloat(const grp, key: string;
                                  const adefault: SIDEX_DOUBLE): SIDEX_DOUBLE;
var
  err: SIDEX_INT32;
begin
  if not self.HasKey(grp, key) then
  begin
    Result := adefault;
  end else
  begin
    err := sidex_Float_Read(FSidexHandle, PSIDEX_STRING(grp),
                                          PSIDEX_STRING(key), Result);
    FLastError := err;
    if err <> SIDEX_SUCCESS then
      raise ESidexError.CreateFmt('sidex_Float_Read error (%d)', [err]);
  end;
end;

function TSIDEXDocument.ReadInt(const grp, key: string;
                                const adefault: Int64): Int64;
var
  err: SIDEX_INT32;
begin
  if not self.HasKey(grp, key) then
  begin
    Result := adefault;
  end else
  begin
    err := sidex_Integer_Read(FSidexHandle, PSIDEX_STRING(grp),
                                            PSIDEX_STRING(key), Result);
    FLastError := err;
    if err <> SIDEX_SUCCESS then
      raise ESidexError.CreateFmt('sidex_Integer_Read error (%d)', [err]);
  end;
end;

function TSIDEXDocument.ReadBinary(const grp, key : string;
                                   var data; maxDataLength : Integer) : Integer;
var
  err:   SIDEX_INT32;
  pData: PByte;
  nData: SIDEX_INT32;
begin
  Result := 0;
  pData  := nil;
  nData  := 0;
  err := sidex_Binary_Read(FSidexHandle, PSIDEX_STRING(grp),
                                         PSIDEX_STRING(key),
                                         pData, nData);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    Result := -2; // error occurred
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_Binary_Read error (%d)', [err])
    end;
  end
  else if (nData >= 0) and Assigned(pData) then
  begin
    if nData <= maxDataLength then
    begin
      Result := nData;
      Move(pData^, data, Result);
    end
    else Result := -1;  // buffer not big enough
  end;
  if Assigned(pData) then sidex_Free_Binary_ReadString(pData);
end;

function TSIDEXDocument.ReadSdxDict(const grp, key: string): SIDEX_VARIANT;
var
  err: SIDEX_INT32;
begin
  Result := SIDEX_VARIANT_NULL;
  err := sidex_Dict_Read(FSidexHandle, PSIDEX_STRING(grp),
                                       PSIDEX_STRING(key), Result);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_Dict_Read error (%d)', [err]);
    end
    else Result := SIDEX_VARIANT_NULL;
  end;
end;

function TSIDEXDocument.ReadSdxList(const grp, key: string): SIDEX_VARIANT;
var
  err: SIDEX_INT32;
begin
  Result := SIDEX_VARIANT_NULL;
  err := sidex_List_Read(FSidexHandle, PSIDEX_STRING(grp),
                                       PSIDEX_STRING(key), Result);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_List_Read error (%d)', [err]);
    end
    else Result := SIDEX_VARIANT_NULL;
  end;
end;

function TSIDEXDocument.ReadSdxTable(const grp, key: string): SIDEX_VARIANT;
var
  err: SIDEX_INT32;
begin
  Result := SIDEX_VARIANT_NULL;
  err := sidex_Table_Read(FSidexHandle, PSIDEX_STRING(grp),
                                        PSIDEX_STRING(key), Result);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_Table_Read error (%d)', [err]);
    end
    else Result := SIDEX_VARIANT_NULL;
  end;
end;

function TSIDEXDocument.ReadSdxVariant(const grp, key: string;
                                       const adefault: SIDEX_VARIANT): SIDEX_VARIANT;
var
  err: SIDEX_INT32;
begin
  err := sidex_Variant_Read(FSidexHandle, PSIDEX_STRING(grp),
                                          PSIDEX_STRING(key), Result);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    if adefault = SIDEX_VARIANT_NULL then Result := SIDEX_VARIANT_NULL
                                     else Result := sidex_Variant_Copy(adefault);
  end;
end;

function TSIDEXDocument.ReadString(const grp, key, adefault: string): string;
var
  length, err: SIDEX_INT32;
  value:       PSIDEX_STRING;
begin
  if not self.HasKey(grp, key) then
  begin
    Result := adefault;
  end else
  begin
    value  := nil;
    length := 0;
    err := sidex_String_Read(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key),
                                           value, length);
    FLastError := err;
    if (err = SIDEX_SUCCESS) and Assigned(value) then
    begin
      Result := value;
      sidex_Free_ReadString(value);
      value := nil;
    end
    else raise ESidexError.CreateFmt('sidex_String_Read error (%d)', [err]);
  end;
end;

function TSIDEXDocument.ReadValue(const group, key: string;
                                  const def: Variant): Variant;
var
  lvalue:   SIDEX_VARIANT;
  ldefault: SIDEX_VARIANT;
begin
  ldefault := VariantAsSidexVariant(def);
  try
    lvalue := ReadSdxVariant(group,key,ldefault);
  finally
    sidex_Variant_DecRef(ldefault);
  end;

  try
    Result := SidexVariantAsVariant(lvalue);
  finally
    sidex_Variant_DecRef(lvalue);
  end;
end;

procedure TSIDEXDocument.SaveContent(const afilename: string);
var
  err: SIDEX_INT32;
begin
  err := sidex_Save_Content(FSidexHandle, PSIDEX_STRING(afilename));
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Save_Content error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.SetContent(const Value: string);
var
  err: SIDEX_INT32;
begin
  err := sidex_Set_Content(FSidexHandle, PSIDEX_STRING(Value));
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Set_Content error (%d)', [err]);
  end;
end;

function TSIDEXDocument.GetStringLength(const grp, key : string) : Integer;
var
  err, nData: SIDEX_INT32;
begin
  Result := 0;
  nData  := 0;
  err := sidex_String_Length(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key), nData);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    Result := -2; // error occurred
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_String_Length error (%d)', [err]);
    end;
  end
  else if nData >= 0 then Result := nData;
end;

function TSIDEXDocument.GetBinaryLength(const grp, key : string) : Integer;
var
  err, nData: SIDEX_INT32;
begin
  Result := 0;
  nData  := 0;
  err := sidex_Binary_Length(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key), nData);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    Result := -2; // error occurred
    if uSidexLib.sidexUseExceptions then
    begin
      raise ESidexError.CreateFmt('sidex_Binary_Length error (%d)', [err]);
    end;
  end
  else if nData >= 0 then Result := nData;
end;

procedure TSIDEXDocument.WriteBoolean(const grp, key: string;
                                      const value: Boolean);
var
  err: SIDEX_INT32;
begin
  if value then
    err := sidex_Boolean_Write(FSidexHandle, PSIDEX_STRING(grp),
                                             PSIDEX_STRING(key), SIDEX_TRUE)
  else
    err := sidex_Boolean_Write(FSidexHandle, PSIDEX_STRING(grp),
                                             PSIDEX_STRING(key), SIDEX_FALSE);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Boolean_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteDateTime(const grp, key: string;
                                       const value: TDateTime);
var
  err: SIDEX_INT32;
  dtv: SIDEX_VARIANT;
begin
  dtv := VariantAsSidexVariant(value);
  try
    err := sidex_DateTime_Write(FSidexHandle, PSIDEX_STRING(grp),
                                              PSIDEX_STRING(key), dtv);
    FLastError := err;
  finally
    sidex_Variant_DecRef(dtv);
  end;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_DateTime_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteFloat(const grp, key: string;
                                    const value: SIDEX_DOUBLE);
var
  err: SIDEX_INT32;
begin
  err := sidex_Float_Write(FSidexHandle, PSIDEX_STRING(grp),
                                         PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Float_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteInt(const grp, key: string;
                                  const value: Int64);
var
  err: SIDEX_INT32;
begin
  err := sidex_Integer_Write(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Integer_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteBinary(const grp, key : string;
                                     const data; dataLength : Integer);
var
  err: SIDEX_INT32;
begin
  err := sidex_Binary_Write(FSidexHandle, PSIDEX_STRING(grp),
                                          PSIDEX_STRING(key),
                                          PByte(@data), dataLength);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Binary_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteSdxDict(const grp, key: string;
                                      const value: SIDEX_VARIANT);
var
  err: SIDEX_INT32;
begin
  err := sidex_Dict_Write(FSidexHandle, PSIDEX_STRING(grp),
                                        PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidexsidex_Dict_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteSdxList(const grp, key: string;
                                      const value: SIDEX_VARIANT);
var
  err: SIDEX_INT32;
begin
  err := sidex_List_Write(FSidexHandle, PSIDEX_STRING(grp),
                                        PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_List_Write  error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteSdxTable(const grp, key: string;
                                       const value: SIDEX_VARIANT);
var
  err: SIDEX_INT32;
begin
  err := sidex_Table_Write(FSidexHandle, PSIDEX_STRING(grp),
                                         PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Table_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteSdxVariant(const grp, key: string;
                                         const value: SIDEX_VARIANT);
var
  err: SIDEX_INT32;
begin
  err := sidex_Variant_Write(FSidexHandle, PSIDEX_STRING(grp),
                                           PSIDEX_STRING(key), value);
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteString(const grp, key, value: string);
var
  err: SIDEX_INT32;
begin
  err := sidex_String_Write(FSidexHandle, PSIDEX_STRING(grp),
                                          PSIDEX_STRING(key),
                                          PSIDEX_STRING(value));
  FLastError := err;
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_String_Write error (%d)', [err]);
  end;
end;

procedure TSIDEXDocument.WriteValue(const group, key: string;
                                    const val: Variant);
var
  lvalue: SIDEX_VARIANT;
begin
  lvalue := VariantAsSidexVariant(val);
  try
    WriteSdxVariant(group, key, lvalue);
  finally
    sidex_Variant_DecRef(lvalue);
  end;
end;

//------------------------------------------------------------------------------

end.

