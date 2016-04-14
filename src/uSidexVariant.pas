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

unit uSidexVariant;

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
  Variants, uSidexLib, uSidexTypes;

//------------------------------------------------------------------------------

{ interface für sidex variants }

function VarSidexGetHandle(const val : Variant) : SIDEX_VARIANT;

{ sidex binary }

function  VarSidexBinary: TVarType;
function  VarSidexBinaryCreate : Variant; overload;
function  VarSidexBinaryCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
function  VarIsSidexBinary(const val : Variant) : Boolean;
function  VarSidexBinaryLength(const vbin : Variant) : Integer;
procedure VarSidexBinarySetData(const vbin : Variant; const data; dataLength : Integer);
function  VarSidexBinaryGetData(const vbin : Variant; var data; maxDataLength : Integer) : Integer;

{ sidex string }

function  VarSidexString: TVarType;
function  VarSidexStringCreate(const s : string) : Variant; overload;
function  VarSidexStringCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
function  VarIsSidexString(const val : Variant) : Boolean;
procedure VarsidexStringSetFormat(const val : Variant; const s : string);
function  VarsidexStringGetFormat(const val : Variant) : string;


{ sidex list }

function  VarSidexList: TVarType;
function  VarSidexListCreate : Variant; overload;
function  VarSidexListCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
function  VarIsSidexList(const val : Variant) : Boolean;
function  VarSidexListAdd(const lst : Variant; const val : Variant) : Integer;
procedure VarSidexListSet(const lst : Variant; i : Integer; const val : Variant);
function  VarSidexListGet(const lst : Variant; i : Integer) : Variant;
function  VarSidexListGetDef(const lst : Variant; i : Integer; adefault : Variant) : Variant;
function  VarSidexListIndex(const lst : Variant; const val : Variant) : Integer;
procedure VarSidexListClear(const lst : Variant);
procedure VarSidexListDelete(const lst : Variant; i : Integer);
procedure VarSidexListInsert(const lst : Variant; i : Integer; const val : Variant);
function  VarSidexListCount(const lst : Variant) : Integer;

{ sidex dictionary }

function  VarSidexDict: TVarType;
function  VarSidexDictCreate : Variant; overload;
function  VarSidexDictCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
function  VarIsSidexDict(const val : Variant) : Boolean;
procedure VarSidexDictSet(const dct : Variant; akey : string; const val : Variant);
function  VarSidexDictHaskey(const dct : Variant; akey : string): Boolean;
function  VarSidexDictKeys(const dct : Variant): TSIDEXStringArray;
function  VarSidexDictGet(const dct : Variant; akey : string): Variant;
function  VarSidexDictGetDef(const dct : Variant; akey : string; adefault : Variant): Variant;
procedure VarSidexDictClear(const dct : Variant);
procedure VarSidexDictDelete(const dct : Variant; akey : string);
function  VarSidexDictCount(const dct : Variant): Integer;
procedure VarSidexDictFirst(const dct : Variant);
function  VarSidexDictNext(const dct : Variant; var akey : string; var avalue : Variant): Boolean;

{ sidex table }

function  VarSidexTable: TVarType;
function  VarSidexTableCreate : Variant; overload;
function  VarSidexTableCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
function  VarIsSidexTable(const val : Variant) : Boolean;
function  VarSidexTableHasColumn(const tab : Variant; col : string) : Boolean;
function  VarSidexTableGetColumnName(const tab : Variant; index : Integer) : string;
function  VarSidexTableGetField(const tab : Variant; col : string) : Variant;
procedure VarSidexTableClear(const tab : Variant);
procedure VarSidexTableFirst(const tab : Variant);
procedure VarSidexTableNext(const tab : Variant);
procedure VarSidexTableAppendRow(const tab : Variant);
procedure VarSidexTableDeleteRow(const tab : Variant);
procedure VarSidexTableAddColumn(const tab : Variant; col : string);
procedure VarSidexTableDeleteColumn(const tab : Variant; col : string);
procedure VarSidexTableSetField(const tab : Variant; col : string; val : Variant);
function  VarSidexTableRowCount(const tab : Variant) : Integer;
function  VarSidexTableColCount(const tab : Variant) : Integer;
function  VarSidexTableEof(const tab : Variant) : Boolean;
function  VarSidexTableGetCurrentRow(const tab : Variant) : Integer;
procedure VarSidexTableSetCurrentRow(const tab : Variant; arow : Integer);
function  VarSidexTableGetRow(const tab : Variant) : Variant;

//#---------------------------------------------------------------------
//#---------------------------------------------------------------------
//# converting Variants into SIDEX Variants and vice versa
//#---------------------------------------------------------------------
//#---------------------------------------------------------------------
function  SidexVariantAsVariant(obj : SIDEX_VARIANT) : Variant;
function  VariantAsSidexVariant(const V : Variant) : SIDEX_VARIANT;

//------------------------------------------------------------------------------

implementation

uses
  SysUtils, Classes, TypInfo, cSidexDocument, uSidexErrors;

//------------------------------------------------------------------------------

type

  { custom variant for sidex binary string }
  TsidexVarBinary = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(
      {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
      V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  { custom variant for sidex binary string }
  TsidexVarString = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(
      {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
      V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  { custom variant for sidex list }
  TsidexVarList = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(
      {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
      V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  { custom variant for sidex dictionary }
  TsidexVarDict = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(
      {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
      V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  { custom variant for sidex table }
  TsidexVarTable = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(
      {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
      V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

//------------------------------------------------------------------------------

var
  sidexVarBinary : TsidexVarBinary = nil;
  sidexVarList   : TsidexVarList   = nil;
  sidexVarDict   : TsidexVarDict   = nil;
  sidexVarTable  : TsidexVarTable  = nil;
  sidexVarString : TsidexVarString = nil;

//------------------------------------------------------------------------------

type
  { base class for sidex variant types }
  TsidexVariantBase = class(TObject)
  protected
    FSdxVarHandle : SIDEX_VARIANT;

    function  NewSidexHandle : SIDEX_VARIANT; virtual; abstract;
    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; virtual; abstract;
    procedure OnCreate; virtual;
    procedure OnDestroy; virtual;

  public
    property SdxVarHandle : SIDEX_VARIANT read FSdxVarHandle write FSdxVarHandle;

    constructor Create(ahandle : SIDEX_VARIANT); overload;        // assign an existing handle
    constructor Create(asource : TsidexVariantBase); overload;    // copy an existing object

    destructor Destroy; override;
  end;

  { variant data for binary class }
  TsidexBinaryData = class(TsidexVariantBase)
  private
    function  GetLength: Integer;

  protected
    function  NewSidexHandle: SIDEX_VARIANT; override;
    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; override;

  public
    property  Length: Integer read GetLength;

    constructor Create; overload;                                 // create a complete new instance

    procedure SetData(const data; dataLength : Integer);
    function  GetData(var data; maxDataLength : Integer) : Integer;
  end;

  { Helper record that helps crack open TVarData }
  TsidexBinaryVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VsidexData : TsidexBinaryData;
    {$if defined(CPU64) or defined(CPUX64)}
    Reserved4  : array[0..7] of Byte;
    {$else}
    Reserved4  : array[0..3] of Byte;
    {$ifend}
  end;

  { variant data for string class }
  TsidexStringData = class(TsidexVariantBase)
  private
    function  GetLength: Integer;
    function  GetValue: string;
    procedure SetValue(const Value: string);
    function  GetFormat: string;
    procedure SetFormat(const Value: string);

  protected
    function  NewSidexHandle: SIDEX_VARIANT; overload; override;
    function  NewSidexHandleWithValue(const s : string): SIDEX_VARIANT;

    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; override;

  public
    constructor Create(const s : string); overload;               // create a complete new instance
    constructor Create; overload;                                 // create a complete new instance

    property  Length: Integer read GetLength;
    property  Value:  string  read GetValue  write SetValue;
    property  Format: string  read GetFormat write SetFormat;
  end;

  { Helper record that helps crack open TVarData }
  TsidexStringVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VsidexData : TsidexStringData;
    {$if defined(CPU64) or defined(CPUX64)}
    Reserved4  : array[0..7] of Byte;
    {$else}
    Reserved4  : array[0..3] of Byte;
    {$ifend}
  end;

  { variant data for list class }
  TsidexListData = class(TsidexVariantBase)
  private
    function  GetCount: Integer;
    function  GetItems(Index: Integer): Variant;
    procedure SetItems(Index: Integer; const Value: Variant);

  protected
    function  NewSidexHandle: SIDEX_VARIANT; override;
    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; override;

  public
    property  Count: Integer read GetCount;
    property  Items[Index: Integer]: Variant read GetItems write SetItems; default;

    constructor Create; overload;                                 // create a complete new instance

    function  Append(i: Variant) : Integer;         // append a value to the list
    function  Add(i: Variant) : Integer;            // same like append
    procedure Insert(Index : Integer; V : Variant); // insert a value at a position
    procedure Clear;                                // delete all values and free the memory
    procedure Delete(index : Integer);

    // check if a value is in the list
    // return -1 if not found else the index of the item
    function  Index(val: Variant): Integer;
  end;

  { Helper record that helps crack open TVarData }
  TsidexListVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VsidexData : TsidexListData;
    {$if defined(CPU64) or defined(CPUX64)}
    Reserved4  : array[0..7] of Byte;
    {$else}
    Reserved4  : array[0..3] of Byte;
    {$ifend}
  end;

  { variant data for dict class }
  TsidexDictData = class(TsidexVariantBase)
  private
    FWalkerIndex : Integer;

    function  GetCount: Integer;
    function  GetItems(Index: string): Variant;
    function  GetKeys(Index: Integer): String;
    procedure SetItems(Index: string; const Value: Variant);

  protected
    function  NewSidexHandle: SIDEX_VARIANT; override;
    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; override;

  public
    property  Items[Index: string]: Variant read GetItems write SetItems; default;
    property  Count: Integer read GetCount;
    property  Keys[Index: Integer]: String read GetKeys;

    constructor Create; overload;                                 // create a complete new instance

    procedure Clear;
    procedure Delete(i : string);
    function  HasKey(i : string) : Boolean;
    procedure First;
    function  Next(out akey : string; out avalue : Variant): Boolean;
  end;

  { Helper record that helps crack open TVarData }
  TsidexDictVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VsidexData : TsidexDictData;
    {$if defined(CPU64) or defined(CPUX64)}
    Reserved4  : array[0..7] of Byte;
    {$else}
    Reserved4  : array[0..3] of Byte;
    {$ifend}
  end;

  { variant data for dict class }
  TsidexTableData = class(TsidexVariantBase)
  private
    { internal data storage }
    FCurrentRow : Integer;
    FEof        : Boolean;

    { private functions }
    function  GetColCount: Integer;
    function  GetColumnNames: Variant;
    function  GetEof: Boolean;
    function  GetField(ix: string): Variant;
    function  GetRowCount: Integer;
    procedure SetField(ix: string; const Value: Variant);
    procedure SetCurrentRow(const Value: Integer);

  protected
    function  NewSidexHandle: SIDEX_VARIANT; override;
    function  CheckSidexType(aval : SIDEX_VARIANT): Boolean; override;
    procedure OnCreate; override;

  public
    property  Eof : Boolean read GetEof;
    property  RowCount : Integer read GetRowCount;
    property  ColCount : Integer read GetColCount;
    property  ColumnNames : Variant read GetColumnNames;
    property  Field[ix : string] : Variant read GetField write SetField;
    property  CurrentRow : Integer read FCurrentRow write SetCurrentRow;

    constructor Create; overload;                                 // create a complete new instance

    procedure AppendRow;
    procedure DeleteRow;
    function  GetRow: TsidexDictData;

    procedure AddColumn(cname : string);
    procedure DeleteColumn(cname : string);
    function  HasColumn(cname : string) : Boolean;
    function  GetColumnName(index: Integer): string;

    // Naviagation
    procedure First;
    procedure Next;
    procedure Clear;
  end;

  { Helper record that helps crack open TVarData }
  TsidexTableVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VsidexData : TsidexTableData;
    {$if defined(CPU64) or defined(CPUX64)}
    Reserved4  : array[0..7] of Byte;
    {$else}
    Reserved4  : array[0..3] of Byte;
    {$ifend}
  end;

//------------------------------------------------------------------------------

{ TsidexVarBinary }

procedure TsidexVarBinary.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TsidexBinaryVarData(V).VsidexData);
end;

procedure TsidexVarBinary.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    {$if not defined(FPC)}
      VarDataInit(Dest);
    {$ifend}
    with TsidexBinaryVarData(Dest) do
    begin
      VType      := VarSidexBinary;
      VsidexData := TsidexBinaryData.Create(TsidexBinaryVarData(Source).VsidexData);
    end;
  end;
end;

function TsidexVarBinary.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := False;
  { execute functions }
  if Name = 'GETDATA' then
  begin
    if Length(Arguments) = 2 then
    begin
      if (FindVarData(Variant(Arguments[1]))^.VType and not varByRef) = varInteger then
      begin
        Variant(Dest) := TsidexBinaryVarData(V).VsidexData.GetData(Variant(FindVarData(Variant(Arguments[0]))^),
                                                                   Variant(FindVarData(Variant(Arguments[1]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for GetData');
    end
    else raise Exception.Create('invalid number of Arguments for GetData');
  end;
end;

function TsidexVarBinary.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := False;
  { execute procedures }
  if Name = 'SETDATA' then
  begin
    if Length(Arguments) = 2 then
    begin
      if (FindVarData(Variant(Arguments[1]))^.VType and not varByRef) = varInteger then
      begin
        TsidexBinaryVarData(V).VsidexData.SetData(Variant(FindVarData(Variant(Arguments[0]))^),
                                                  Variant(FindVarData(Variant(Arguments[1]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for SetData');
    end
    else raise Exception.Create('invalid number of Arguments for SetData');
  end;
end;

function TsidexVarBinary.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := False;
  if Name = 'LENGTH' then
  begin
    Variant(Dest) := TsidexBinaryVarData(V).VsidexData.Length;
    Result := true;
  end;
end;

function TsidexVarBinary.SetProperty(
  {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
  V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := False;
  if Name = 'LENGTH' then
  begin
    raise Exception.Create('property Length is read only');
  end;
end;

//------------------------------------------------------------------------------

{ TsidexBinaryData }

function TsidexBinaryData.CheckSidexType(aval: SIDEX_VARIANT): Boolean;
begin
  Result := (sidex_Variant_Binary_Check(aval) <> SIDEX_FALSE);
end;

function TsidexBinaryData.NewSidexHandle: SIDEX_VARIANT;
var
  err, Dummy: SIDEX_INT32;
begin
  Dummy := 0;
  err   := sidex_Variant_New_Binary(PByte(@Dummy), 0, Result);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_New_Binary error (%d)', [err]);
  end;
end;

function TsidexBinaryData.GetLength: Integer;
var
  err, nData: SIDEX_INT32;
begin
  Result := 0;
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    nData := 0;
    err := sidex_Variant_As_Binary_Length(FSdxVarHandle, nData);
    if err <> SIDEX_SUCCESS then
    begin
      Result := -2; // error occurred
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_As_Binary_Length error (%d)', [err]);
      end;
    end
    else if nData > 0 then Result := nData;
  end;
end;

procedure TsidexBinaryData.SetData(const data; dataLength : Integer);
var
  err: SIDEX_INT32;
begin
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
    sidex_Variant_DecRef(FSdxVarHandle);

  err := sidex_Variant_New_Binary(PByte(@data), dataLength, FSdxVarHandle);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_New_Binary error (%d)', [err]);
  end;
end;

constructor TsidexBinaryData.Create;
begin
  inherited;
  FSdxVarHandle := NewSidexHandle;
  OnCreate();
end;

function TsidexBinaryData.GetData(var data; maxDataLength : Integer) : Integer;
var
  err:   SIDEX_INT32;
  pData: PByte;
  nData: SIDEX_INT32;
begin
  Result := 0;
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    pData  := nil;
    nData  := 0;
    err := sidex_Variant_As_Binary(FSdxVarHandle, pData, nData);
    if err <> SIDEX_SUCCESS then
    begin
      Result := -2; // error occurred
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_As_Binary error (%d)', [err])
      end;
    end
    else if (nData > 0) and Assigned(pData) then
    begin
      Result := nData;
      if maxDataLength > 0 then
      begin
        if Result <= maxDataLength then
        begin
          Move(pData^, data, Result);
        end
        else Result := -1;  // buffer not big enough
      end;
    end;
    if Assigned(pData) then sidex_Free_Binary_ReadString(pData);
  end;
end;

//------------------------------------------------------------------------------

{ TsidexVarList }

procedure TsidexVarList.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TsidexListVarData(V).VsidexData);
end;

procedure TsidexVarList.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    {$if not defined(FPC)}
      VarDataInit(Dest);
    {$ifend}
    with TsidexListVarData(Dest) do
    begin
      VType      := VarSidexList;
      VsidexData := TsidexListData.Create(TsidexListVarData(Source).VsidexData);
    end;
  end;
end;

function TsidexVarList.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute functions }
  if Name = 'ADD' then
  begin
    if Length(Arguments) = 1 then
    begin
      Variant(Dest) := TsidexListVarData(V).VsidexData.Add(Variant(FindVarData(Variant(Arguments[0]))^));
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for add');
  end
  else if Name = 'INDEX' then
  begin
    if Length(Arguments) = 1 then
    begin
      Variant(Dest) := TsidexListVarData(V).VsidexData.Index(Variant(FindVarData(Variant(Arguments[0]))^));
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for index');
  end;
  if Name = 'GET' then
  begin
    if Length(Arguments) = 1 then
    begin
      if (FindVarData(Variant(Arguments[0]))^.VType and not varByRef) = varInteger then
      begin
        Variant(Dest) := TsidexListVarData(V).VsidexData[Variant(FindVarData(Variant(Arguments[0]))^)];
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for get');
    end
    else raise Exception.Create('invalid number of Arguments for get');
  end;
end;

function TsidexVarList.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute procedures }
  if Name = 'CLEAR' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexListVarData(V).VsidexData.Clear;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for clear');
  end
  else if Name = 'DELETE' then
  begin
    if Length(Arguments) = 1 then
    begin
      if (FindVarData(Variant(Arguments[0]))^.VType and not varByRef) = varInteger then
      begin
        TsidexListVarData(V).VsidexData.Delete(Variant(FindVarData(Variant(Arguments[0]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for delete');
    end
    else raise Exception.Create('invalid number of Arguments for delete');
  end
  else if Name = 'SETVALUE' then
  begin
    if Length(Arguments) = 2 then
    begin
      if (FindVarData(Variant(Arguments[0]))^.VType and not varByRef) = varInteger then
      begin
        TsidexListVarData(V).VsidexData[Variant(FindVarData(Variant(Arguments[0]))^)] := Variant(FindVarData(Variant(Arguments[1]))^);
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for set');
    end
    else raise Exception.Create('invalid number of Arguments for set');
  end
  else if Name = 'INSERT' then
  begin
    if Length(Arguments) = 2 then
    begin
      if (FindVarData(Variant(Arguments[0]))^.VType and not varByRef) = varInteger then
      begin
        TsidexListVarData(V).VsidexData.Insert(Variant(FindVarData(Variant(Arguments[0]))^),Variant(FindVarData(Variant(Arguments[1]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for insert');
    end
    else raise Exception.Create('invalid number of Arguments for insert');
  end;
end;

function TsidexVarList.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := False;
  if Name = 'COUNT' then
  begin
    Variant(Dest) := TsidexListVarData(V).VsidexData.Count;
    Result := true;
  end;
end;

function TsidexVarList.SetProperty(
  {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
  V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := False;
  if Name = 'COUNT' then
  begin
    raise Exception.Create('property count is read only');
  end;
end;

//------------------------------------------------------------------------------

{ TsidexListData }

function TsidexListData.Add(i: Variant): Integer;
begin
  Result := Append(i);
end;

function TsidexListData.Append(i: Variant): Integer;
var
  lvalue : SIDEX_VARIANT;
  iVal   : SIDEX_INT32;
  err    : Integer;
begin
  lvalue := VariantAsSidexVariant(i);
  err := sidex_Variant_List_Append(SdxVarHandle, lvalue, iVal);
  sidex_Variant_DecRef(lvalue);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
  end;
  Result := iVal;
end;

function TsidexListData.CheckSidexType(aval: SIDEX_VARIANT): Boolean;
begin
  Result := (sidex_Variant_List_Check(aval) <> SIDEX_FALSE);
end;

procedure TsidexListData.Clear;
var
  err : Integer;
begin
  err := sidex_Variant_List_Clear(SdxVarHandle);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Clear error (%d)',[err]);
  end;
end;

constructor TsidexListData.Create;
begin
  inherited;
  FSdxVarHandle := NewSidexHandle;
  OnCreate();
end;

procedure TsidexListData.Delete(index: Integer);
var
  err : Integer;
begin
  err := sidex_Variant_List_DeleteItem(SdxVarHandle, index);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_DeleteItem error (%d)',[err]);
  end;
end;

function TsidexListData.GetCount: Integer;
var
  iVal: SIDEX_INT32;
  err : Integer;
begin
  err := sidex_Variant_List_Size(SdxVarHandle, iVal);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Size error (%d)',[err]);
  end;
  Result := iVal;
end;

function TsidexListData.GetItems(Index: Integer): Variant;
var
  err : Integer;
  value : SIDEX_VARIANT;
begin
  err := sidex_Variant_List_Get(SdxVarHandle, Index, value);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Get error (%d)',[err]);
  end;
  Result := SidexVariantAsVariant(value);
end;

function TsidexListData.Index(val: Variant): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i] = val then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TsidexListData.Insert(Index: Integer; V: Variant);
var
  lvalue : SIDEX_VARIANT;
  err    : Integer;
begin
  lvalue := VariantAsSidexVariant(V);
  err := sidex_Variant_List_Insert(SdxVarHandle,lvalue,Index);
  sidex_Variant_DecRef(lvalue);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Insert error (%d)',[err]);
  end;
end;

function TsidexListData.NewSidexHandle: SIDEX_VARIANT;
begin
  Result := sidex_Variant_New_List();
end;

procedure TsidexListData.SetItems(Index: Integer; const Value: Variant);
var
  err  : Integer;
  sval : SIDEX_VARIANT;
begin
  sval := VariantAsSidexVariant(Value);
  err := sidex_Variant_List_Set(SdxVarHandle,sval,Index);
  sidex_Variant_DecRef(sval);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_List_Set error (%d)',[err]);
  end;
end;

//------------------------------------------------------------------------------

{ interface implementation }

{ general }

function IsSidexVariant(const V : Variant) : Boolean;
begin
  Result := (TVarData(V).VType = VarSidexList) or
            (TVarData(V).VType = VarSidexDict) or
            (TVarData(V).VType = VarSidexTable) or
            (TVarData(V).VType = VarSidexBinary) or
            (TVarData(V).VType = VarSidexString);
end;

function VarSidexGetHandle(const val : Variant) : SIDEX_VARIANT;
begin
  if IsSidexVariant(val) then
  begin
    Result := (TsidexListVarData(val).VsidexData as TsidexVariantBase).SdxVarHandle;
  end
  else raise Exception.Create('variant is not a sidex type');
end;

//------------------------------------------------------------------------------

{ sidex binary string }

function VarSidexBinary: TVarType;
begin
  Result := sidexVarBinary.VarType;
end;

procedure VarSidexBinaryCreateInto(var ADest: Variant; const ASidexBinary: TsidexBinaryData);
begin
  VarClear(ADest);
  with TsidexBinaryVarData(ADest) do
  begin
    VType      := VarSidexBinary;
    VsidexData := ASidexBinary
  end;
end;

function VarSidexBinaryCreate : Variant;
begin
  VarSidexBinaryCreateInto(Result, TsidexBinaryData.Create);
end;

function VarSidexBinaryCreate(ahandle : SIDEX_VARIANT) : Variant;
begin
  VarSidexBinaryCreateInto(Result, TsidexBinaryData.Create(ahandle));
end;

function VarIsSidexBinary(const val : Variant) : Boolean;
begin
  Result := (TVarData(val).VType and not varByRef) = VarSidexBinary;
end;

function VarSidexBinaryLength(const vbin : Variant) : Integer;
begin
  if VarIsSidexBinary(vbin) then
  begin
    Result := TsidexBinaryVarData(vbin).VsidexData.Length;
  end
  else raise Exception.Create('variant is not a sidex binary string');
end;

procedure VarSidexBinarySetData(const vbin : Variant; const data; dataLength : Integer);
begin
  if VarIsSidexBinary(vbin) then
  begin
    TsidexBinaryVarData(vbin).VsidexData.SetData(data, dataLength);
  end
  else raise Exception.Create('variant is not a sidex binary string');
end;

function VarSidexBinaryGetData(const vbin : Variant; var data; maxDataLength : Integer) : Integer;
begin
  if VarIsSidexBinary(vbin) then
  begin
    Result := TsidexBinaryVarData(vbin).VsidexData.GetData(data, maxDataLength);
  end
  else raise Exception.Create('variant is not a sidex binary string');
end;
//------------------------------------------------------------------------------

{ sidex string }

function  VarSidexString: TVarType;
begin
  Result := sidexVarBinary.VarType;
end;

procedure VarSidexStringCreateInto(var ADest: Variant; const ASidexString: TsidexStringData);
begin
  VarClear(ADest);
  with TsidexStringVarData(ADest) do
  begin
    VType      := VarSidexString;
    VsidexData := ASidexString;
  end;
end;

function  VarSidexStringCreate(const s : string) : Variant; overload;
begin
    VarSidexStringCreateInto(Result, TsidexStringData.Create(s));
end;

function  VarSidexStringCreate(ahandle : SIDEX_VARIANT) : Variant; overload;
begin
  VarSidexStringCreateInto(Result, TsidexStringData.Create(ahandle));
end;

function  VarIsSidexString(const val : Variant) : Boolean;
begin
  Result := (TVarData(val).VType and not varByRef) = VarSidexString;
end;

procedure VarsidexStringSetFormat(const val : Variant; const s : string);
begin
  if VarIsSidexString(val) then
  begin
    TsidexStringVarData(val).VsidexData.SetFormat(s);
  end
  else raise Exception.Create('variant is not a sidex string');
end;

function  VarsidexStringGetFormat(const val : Variant) : string;
begin
  if VarIsSidexString(val) then
  begin
    Result := TsidexStringVarData(val).VsidexData.GetFormat;
  end
  else raise Exception.Create('variant is not a sidex string');
end;

//------------------------------------------------------------------------------

{ sidex list }

function VarSidexList: TVarType;
begin
  Result := sidexVarList.VarType;
end;

procedure VarSidexListCreateInto(var ADest: Variant; const ASidexList: TsidexListData);
begin
  VarClear(ADest);
  with TsidexListVarData(ADest) do
  begin
    VType      := VarSidexList;
    VsidexData := ASidexList
  end;
end;

function VarSidexListCreate : Variant;
begin
  VarSidexListCreateInto(Result, TsidexListData.Create);
end;

function VarSidexListCreate(ahandle : SIDEX_VARIANT) : Variant;
begin
  VarSidexListCreateInto(Result, TsidexListData.Create(ahandle));
end;

function VarIsSidexList(const val : Variant) : Boolean;
begin
  Result := (TVarData(val).VType and not varByRef) = VarSidexList;
end;

function VarSidexListAdd(const lst : Variant; const val : Variant): Integer;
begin
  if VarIsSidexList(lst) then
  begin
    Result := TsidexListVarData(lst).VsidexData.Add(val);
  end
  else raise Exception.Create('variant is not a sidex list');
end;

procedure VarSidexListSet(const lst : Variant; i : Integer; const val : Variant);
begin
  if VarIsSidexList(lst) then
  begin
    TsidexListVarData(lst).VsidexData.Items[i] := val;
  end
  else raise Exception.Create('variant is not a sidex list');
end;

function VarSidexListGet(const lst : Variant; i : Integer) : Variant;
begin
  Result := VarSidexListGetDef(lst, i, Null);
end;

function VarSidexListGetDef(const lst : Variant; i : Integer; adefault : Variant) : Variant;
var
  err   : Integer;
  value : SIDEX_VARIANT;
begin
  if VarIsSidexList(lst) then
  begin
    err := sidex_Variant_List_Get(TsidexListVarData(lst).VsidexData.SdxVarHandle,
                                  i, value);
    if (err <> SIDEX_SUCCESS) and (err <> SIDEX_ERR_NOCONTENT) then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_List_Get error (%d)',[err]);
    end;
    if err = SIDEX_ERR_NOCONTENT then Result := adefault
                                 else Result := SidexVariantAsVariant(value);
  end
  else raise Exception.Create('variant is not a sidex list');
end;

function VarSidexListIndex(const lst : Variant; const val : Variant) : Integer;
begin
  if VarIsSidexList(lst) then
  begin
    Result := TsidexListVarData(lst).VsidexData.Index(val);
  end
  else raise Exception.Create('variant is not a sidex list');
end;

procedure VarSidexListClear(const lst : Variant);
begin
  if VarIsSidexList(lst) then
  begin
    TsidexListVarData(lst).VsidexData.Clear;
  end
  else raise Exception.Create('variant is not a sidex list');
end;

procedure VarSidexListDelete(const lst : Variant; i : Integer);
begin
  if VarIsSidexList(lst) then
  begin
    TsidexListVarData(lst).VsidexData.Delete(i);
  end
  else raise Exception.Create('variant is not a sidex list');
end;

procedure VarSidexListInsert(const lst : Variant; i : Integer; const val : Variant);
begin
  if VarIsSidexList(lst) then
  begin
    TsidexListVarData(lst).VsidexData.Insert(i, val);
  end
  else raise Exception.Create('variant is not a sidex list');
end;

function VarSidexListCount(const lst : Variant) : Integer;
begin
  if VarIsSidexList(lst) then
  begin
    Result := TsidexListVarData(lst).VsidexData.Count;
  end
  else raise Exception.Create('variant is not a sidex list');
end;

//------------------------------------------------------------------------------

{ sidex dictionary }

function VarSidexDict: TVarType;
begin
  Result := sidexVarDict.VarType;
end;

procedure VarSidexDictCreateInto(var ADest: Variant; const ASidexDict: TsidexDictData);
begin
  VarClear(ADest);
  with TsidexDictVarData(ADest) do
  begin
    VType      := VarSidexDict;
    VsidexData := ASidexDict
  end;
end;

function VarSidexDictCreate : Variant;
begin
  VarSidexDictCreateInto(Result, TsidexDictData.Create);
end;

function VarSidexDictCreate(ahandle : SIDEX_VARIANT) : Variant;
begin
  VarSidexDictCreateInto(Result, TsidexDictData.Create(ahandle));
end;

function VarIsSidexDict(const val : Variant) : Boolean;
begin
  Result := (TVarData(val).VType and not varByRef)  = VarSidexDict;
end;

procedure VarSidexDictSet(const dct : Variant; akey : string; const val : Variant);
begin
  if VarIsSidexDict(dct) then
  begin
    TsidexDictVarData(dct).VsidexData.Items[akey] := val;
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

function VarSidexDictHaskey(const dct : Variant; akey : string): Boolean;
begin
  if VarIsSidexDict(dct) then
  begin
    Result := TsidexDictVarData(dct).VsidexData.HasKey(akey);
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

function VarSidexDictKeys(const dct : Variant): TSIDEXStringArray;
var
  i, n: Integer;
begin
  if VarIsSidexDict(dct) then
  begin
    n := TsidexDictVarData(dct).VsidexData.Count;
    SetLength(Result, n);
    for i := 0 to n - 1 do
    begin
      Result[i] := TsidexDictVarData(dct).VsidexData.Keys[i];
    end;
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

function VarSidexDictGet(const dct : Variant; akey : string): Variant;
begin
  Result := VarSidexDictGetDef(dct, akey, Null);
end;

function VarSidexDictGetDef(const dct : Variant; akey : string; adefault : Variant): Variant;
var
  err   : Integer;
  value : SIDEX_VARIANT;
begin
  if VarIsSidexDict(dct) then
  begin
    err := sidex_Variant_Dict_Get(TsidexDictVarData(dct).VsidexData.SdxVarHandle,
                                  PSIDEX_STRING(akey), value);
    if (err <> SIDEX_SUCCESS) and (err <> SIDEX_ERR_NOCONTENT) then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_Dict_Get error (%d)',[err]);
    end;
    if err = SIDEX_ERR_NOCONTENT then Result := adefault
                                 else Result := SidexVariantAsVariant(value);
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

procedure VarSidexDictClear(const dct : Variant);
begin
  if VarIsSidexDict(dct) then
  begin
    TsidexDictVarData(dct).VsidexData.Clear;
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

procedure VarSidexDictDelete(const dct : Variant; akey : string);
begin
  if VarIsSidexDict(dct) then
  begin
    TsidexDictVarData(dct).VsidexData.Delete(akey);
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

function VarSidexDictCount(const dct : Variant): Integer;
begin
  if VarIsSidexDict(dct) then
  begin
    Result := TsidexDictVarData(dct).VsidexData.Count;
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

procedure VarSidexDictFirst(const dct : Variant);
begin
  if VarIsSidexDict(dct) then
  begin
    TsidexDictVarData(dct).VsidexData.First;
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

function VarSidexDictNext(const dct : Variant; var akey : string; var avalue : Variant): Boolean;
begin
  if VarIsSidexDict(dct) then
  begin
    Result := TsidexDictVarData(dct).VsidexData.Next(akey,avalue);
  end
  else raise Exception.Create('variant is not a sidex dictionary');
end;

//------------------------------------------------------------------------------

{ sidex table }

function VarSidexTable: TVarType;
begin
  Result := sidexVarTable.VarType;
end;

procedure VarSidexTableCreateInto(var ADest: Variant; const ASidexTable: TsidexTableData);
begin
  VarClear(ADest);
  with TsidexTableVarData(ADest) do
  begin
    VType      := VarSidexTable;
    VsidexData := ASidexTable
  end;
end;

function VarSidexTableCreate : Variant;
begin
  VarSidexTableCreateInto(Result, TsidexTableData.Create);
end;

function VarSidexTableCreate(ahandle : SIDEX_VARIANT) : Variant;
begin
  VarSidexTableCreateInto(Result, TsidexTableData.Create(ahandle));
end;

function VarIsSidexTable(const val : Variant) : Boolean;
begin
  Result := (TVarData(val).VType and not varByRef) = VarSidexTable;
end;

function VarSidexTableHasColumn(const tab : Variant; col : string) : Boolean;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.HasColumn(col);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableGetColumnName(const tab : Variant; index : Integer) : string;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.GetColumnName(index);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableGetField(const tab : Variant; col : string) : Variant;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.GetField(col);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableClear(const tab : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.Clear;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableFirst(const tab : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.First;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableNext(const tab : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.Next;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableAppendRow(const tab : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.AppendRow;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableDeleteRow(const tab : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.DeleteRow;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableAddColumn(const tab : Variant; col : string);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.AddColumn(col);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableDeleteColumn(const tab : Variant; col : string);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.DeleteColumn(col);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableSetField(const tab : Variant; col : string; val : Variant);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.SetField(col,val);
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableRowCount(const tab : Variant) : Integer;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.RowCount;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableColCount(const tab : Variant) : Integer;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.ColCount;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableEof(const tab : Variant) : Boolean;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.eof;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableGetCurrentRow(const tab : Variant) : Integer;
begin
  if VarIsSidexTable(tab) then
  begin
    Result := TsidexTableVarData(tab).VsidexData.CurrentRow;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

procedure VarSidexTableSetCurrentRow(const tab : Variant; arow : Integer);
begin
  if VarIsSidexTable(tab) then
  begin
    TsidexTableVarData(tab).VsidexData.CurrentRow := arow;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

function VarSidexTableGetRow(const tab : Variant) : Variant;
var
  rowdict: TsidexDictData;
begin
  if VarIsSidexTable(tab) then
  begin
    rowdict := TsidexTableVarData(tab).VsidexData.GetRow;
    if Assigned(rowDict) then
    begin
      Result := SidexVariantAsVariant(rowdict.SdxVarHandle);
      FreeAndNil(rowdict);
    end
    else Result := Null;
  end
  else raise Exception.Create('variant is not a sidex table');
end;

//------------------------------------------------------------------------------

{ TsidexVarDict }

procedure TsidexVarDict.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TsidexListVarData(V).VsidexData);
end;

procedure TsidexVarDict.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    {$if not defined(FPC)}
      VarDataInit(Dest);
    {$ifend}
    with TsidexDictVarData(Dest) do
    begin
      VType      := VarType;
      VsidexData := TsidexDictData.Create(TsidexDictVarData(Source).VsidexData);
    end;
  end;
end;

function TsidexVarDict.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute functions }
  if Name = 'HASKEY' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        Variant(Dest) := TsidexDictVarData(V).VsidexData.HasKey(Variant(FindVarData(Variant(Arguments[0]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for haskey');
    end
    else raise Exception.Create('invalid number of Arguments for haskey');
  end
  else if Name = 'GET' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        Variant(Dest) := TsidexDictVarData(V).VsidexData.Items[Variant(FindVarData(Variant(Arguments[0]))^)];
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for get');
    end
    else raise Exception.Create('invalid number of Arguments for get');
  end;
end;

function TsidexVarDict.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute procedures }
  if Name = 'CLEAR' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexDictVarData(V).VsidexData.Clear;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for clear');
  end
  else if Name = 'DELETE' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        TsidexDictVarData(V).VsidexData.Delete(Variant(FindVarData(Variant(Arguments[0]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for delete');
    end
    else raise Exception.Create('invalid number of Arguments for delete');
  end
  else if Name = 'SETVALUE' then
  begin
    if Length(Arguments) = 2 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        TsidexDictVarData(V).VsidexData.Items[Variant(FindVarData(Variant(Arguments[0]))^)] :=
          Variant(FindVarData(Variant(Arguments[1]))^);
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for set');
    end
    else raise Exception.Create('invalid number of Arguments for set');
  end;
end;

function TsidexVarDict.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := False;
  if TsidexDictVarData(V).VsidexData.HasKey(Name) then
  begin
    Variant(Dest) := TsidexDictVarData(V).VsidexData.Items[Name];
    Result := True;
  end;
end;

function TsidexVarDict.SetProperty(
  {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
  V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  TsidexDictVarData(V).VsidexData.Items[Name] := Variant(Value);
  Result := True;
end;

//------------------------------------------------------------------------------

{ TsidexVarTable }

procedure TsidexVarTable.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TsidexListVarData(V).VsidexData);
end;

procedure TsidexVarTable.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    {$if not defined(FPC)}
      VarDataInit(Dest);
    {$ifend}
    with TsidexTableVarData(Dest) do
    begin
      VType      := VarType;
      VsidexData := TsidexTableData.Create(TsidexTableVarData(Source).VsidexData);
    end;
  end;
end;

function TsidexVarTable.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute functions }
  if Name = 'HASCOLUMN' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        Variant(Dest) := TsidexTableVarData(V).VsidexData.HasColumn(Variant(FindVarData(Variant(Arguments[0]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for hascolumn');
    end
    else raise Exception.Create('invalid number of Arguments for hascolumn');
  end
  else if Name = 'GETFIELD' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        Variant(Dest) := TsidexTableVarData(V).VsidexData.Field[Variant(FindVarData(Variant(Arguments[0]))^)];
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for getField');
    end
    else raise Exception.Create('invalid number of Arguments for getField');
  end;
end;

function TsidexVarTable.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { execute procedures }
  if Name = 'CLEAR' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexTableVarData(V).VsidexData.Clear;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for clear');
  end
  else if Name = 'FIRST' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexTableVarData(V).VsidexData.First;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for first');
  end
  else if Name = 'NEXT' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexTableVarData(V).VsidexData.Next;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for next');
  end
  else if Name = 'APPENDROW' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexTableVarData(V).VsidexData.AppendRow;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for AppendRow');
  end
  else if Name = 'DELETEROW' then
  begin
    if Length(Arguments) = 0 then
    begin
      TsidexTableVarData(V).VsidexData.DeleteRow;
      Result := True;
    end
    else raise Exception.Create('invalid number of Arguments for DeleteRow');
  end
  else if Name = 'ADDCOLUMN' then
  begin
    if Length(Arguments) = 1 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        TsidexTableVarData(V).VsidexData.AddColumn(Variant(FindVarData(Variant(Arguments[0]))^));
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for AddColumn');
    end
    else raise Exception.Create('invalid number of Arguments for AddColumn');
  end
  else if Name = 'SETFIELD' then
  begin
    if Length(Arguments) = 2 then
    begin
      if ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varString) or
         ((FindVarData(Variant(Arguments[0]))^.VType and not varByRef ) = varOleStr) then
      begin
        TsidexTableVarData(V).VsidexData.Field[Variant(FindVarData(Variant(Arguments[0]))^)] := Variant(FindVarData(Variant(Arguments[1]))^);
        Result := True;
      end
      else raise Exception.Create('parameter type mismatch for SetField');
    end
    else raise Exception.Create('invalid number of Arguments for SetField');
  end;
end;

function TsidexVarTable.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := False;
  if Name = 'ROWCOUNT' then
  begin
    Variant(Dest) := TsidexTableVarData(V).VsidexData.RowCount;
    Result := true;
  end
  else if Name = 'COLCOUNT' then
  begin
    Variant(Dest) := TsidexTableVarData(V).VsidexData.ColCount;
    Result := true;
  end
  else if Name = 'EOF' then
  begin
    Variant(Dest) := TsidexTableVarData(V).VsidexData.eof;
    Result := true;
  end;
end;

function TsidexVarTable.SetProperty(
  {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
  V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

{ TsidexVariantBase }


constructor TsidexVariantBase.Create(ahandle: SIDEX_VARIANT);
begin
  inherited Create;
  if CheckSidexType(ahandle) then
  begin
    FSdxVarHandle := ahandle;
    sidex_Variant_IncRef(FSdxVarHandle);
  end
  else raise ESidexError.Create('invalid sidex type');
  OnCreate();
end;

constructor TsidexVariantBase.Create(asource: TsidexVariantBase);
begin
  inherited Create;
  if CheckSidexType(asource.SdxVarHandle) then
  begin
    FSdxVarHandle := sidex_Variant_Copy(asource.SdxVarHandle);
  end
  else raise ESidexError.Create('invalid sidex type');
  OnCreate();
end;

destructor TsidexVariantBase.Destroy;
begin
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    sidex_Variant_DecRef(FSdxVarHandle);
    FSdxVarHandle := SIDEX_VARIANT_NULL;
  end;
  OnDestroy();
  inherited Destroy;
end;

procedure TsidexVariantBase.OnCreate;
begin
  // nothing to do here
end;

procedure TsidexVariantBase.OnDestroy;
begin
  // nothing to do here
end;

//------------------------------------------------------------------------------

{ TsidexDictData }

function TsidexDictData.CheckSidexType(aval: SIDEX_VARIANT): Boolean;
begin
  Result := (sidex_Variant_Dict_Check(aval) <> SIDEX_FALSE);
end;

procedure TsidexDictData.Clear;
var
  err : Integer;
begin
  err := sidex_Variant_Dict_Clear(SdxVarHandle);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Clear error (%d)',[err]);
  end;
end;

constructor TsidexDictData.Create;
begin
  inherited;
  FSdxVarHandle := NewSidexHandle;
  OnCreate();
end;

procedure TsidexDictData.Delete(i: String);
var
  err : Integer;
begin
  err := sidex_Variant_Dict_Delete(SdxVarHandle, PSIDEX_STRING(i));
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Delete error (%d)',[err]);
  end;
end;

procedure TsidexDictData.First;
var
  err : Integer;
begin
  FWalkerIndex := 0;
  if Assigned(sidex_Variant_Dict_Next) then // <- Next is correct here!
  begin
    err := sidex_Variant_Dict_First(SdxVarHandle);
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_Dict_First error (%d)',[err]);
    end;
  end;
end;

function TsidexDictData.GetCount: Integer;
var
  iVal: SIDEX_INT32;
  err : Integer;
begin
  err := sidex_Variant_Dict_Size(SdxVarHandle, iVal);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Size error (%d)',[err]);
  end;
  Result := iVal;
end;

function TsidexDictData.GetItems(Index: string): Variant;
var
  err : Integer;
  value : SIDEX_VARIANT;
begin
  err := sidex_Variant_Dict_Get(SdxVarHandle, PSIDEX_STRING(Index), value);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Get error (%d)',[err]);
  end;
  Result := SidexVariantAsVariant(value);
end;

function TsidexDictData.GetKeys(Index: Integer): String;
var
  err    : Integer;
  svkeys : SIDEX_VARIANT;
  sval   : SIDEX_VARIANT;
begin
  err := sidex_Variant_Dict_Keys(SdxVarHandle,svkeys);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Keys error (%d)',[err]);
  end;

  err := sidex_Variant_List_Get(svkeys, Index, sval);
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(svkeys);
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Keys error (%d)',[err]);
  end;

  try
    Result := SidexVariantAsVariant(sval);
  finally
    sidex_Variant_DecRef(svkeys);
  end;
end;

function TsidexDictData.HasKey(i: string): Boolean;
var
  err:     Integer;
  bHasKey: SIDEX_BOOL;
begin
  bHasKey := SIDEX_FALSE;
  err := sidex_Variant_Dict_HasKey(SdxVarHandle, PSIDEX_STRING(i), bHasKey);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Dict_HasKey error (%d)',[err]);
  end;
  Result := (bHasKey <> SIDEX_FALSE);
end;

function TsidexDictData.NewSidexHandle: SIDEX_VARIANT;
begin
  Result := sidex_Variant_New_Dict();
end;

function TsidexDictData.Next(out akey: string; out avalue: Variant): Boolean;
var
  err  : Integer;
  pKey : PSIDEX_STRING;
  sval : SIDEX_VARIANT;
begin
  if Assigned(sidex_Variant_Dict_Next) then
  begin
    err := sidex_Variant_Dict_Next(SdxVarHandle, pKey, sval);
    Result := (err = SIDEX_SUCCESS);
    if (err <> SIDEX_ERR_NOCONTENT) and not Result then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_Dict_Next  error (%d)',[err]);
    end;
    if Result then
    begin
      akey   := PSIDEX_STRING(pKey);
      avalue := SidexVariantAsVariant(sval);
    end
    else
    begin
      akey   := '';
      avalue := Null;
    end;
  end
  else
  begin
    if FWalkerIndex < Count then
    begin
      akey   := Keys[FWalkerIndex];
      avalue := Items[akey];
      Inc(FWalkerIndex);
      Result := true;
    end
    else Result := false;
  end;
end;

procedure TsidexDictData.SetItems(Index: string; const Value: Variant);
var
  err  : Integer;
  sval : SIDEX_VARIANT;
begin
  sval := VariantAsSidexVariant(Value);
  err := sidex_Variant_Dict_Set(SdxVarHandle, PSIDEX_STRING(Index), sval);
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(sval);
    raise ESidexError.CreateFmt('sidex_Variant_Dict_Set error (%d)',[err]);
  end;
  sidex_Variant_DecRef(sval);
end;

//------------------------------------------------------------------------------

{ TsidexTableData }

procedure TsidexTableData.AddColumn(cname: string);
var
  err : Integer;
begin
  err := sidex_Variant_Table_AddColumn(SdxVarHandle, PSIDEX_STRING(cname));
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_AddColumn error (%d)',[err]);
  end;
end;

procedure TsidexTableData.DeleteColumn(cname: string);
var
  err : Integer;
begin
  err := sidex_Variant_Table_DeleteColumn(SdxVarHandle, PSIDEX_STRING(cname));
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_DeleteColumn error (%d)',[err]);
  end;
end;

procedure TsidexTableData.AppendRow;
var
  iVal: SIDEX_INT32;
  err : Integer;
begin
  err := sidex_Variant_Table_AddRow(SdxVarHandle, iVal);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_AddRow error (%d)',[err]);
  end;
  FCurrentRow := iVal;
end;

function TsidexTableData.CheckSidexType(aval: SIDEX_VARIANT): Boolean;
begin
  Result := (sidex_Variant_Table_Check(aval) <> SIDEX_FALSE);
end;

procedure TsidexTableData.Clear;
var
  err : Integer;
begin
  err := sidex_Variant_Table_DeleteRows(SdxVarHandle);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_DeleteRows error (%d)',[err]);
  end;
  First();
end;

constructor TsidexTableData.Create;
begin
  inherited;
  FSdxVarHandle := NewSidexHandle;
  OnCreate();
end;

procedure TsidexTableData.DeleteRow;
var
  err : Integer;
begin
  err := sidex_Variant_Table_DeleteRow(SdxVarHandle,FCurrentRow);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_DeleteRow error (%d)',[err]);
  end;
  if FCurrentRow > 0 then Dec(FCurrentRow);
end;

function TsidexTableData.GetRow: TsidexDictData;
var
  err : Integer;
  hSdxVarDict : SIDEX_VARIANT;
begin
  err := sidex_Variant_Table_GetRow(SdxVarHandle,FCurrentRow,hSdxVarDict);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_DeleteRow error (%d)',[err]);
  end;
  Result := TsidexDictData.Create(hSdxVarDict);
end;

procedure TsidexTableData.First;
begin
  FCurrentRow := 0;
  FEof := (RowCount = 0);
end;

function TsidexTableData.GetColCount: Integer;
var
  iVal: SIDEX_INT32;
  err : Integer;
begin
  err := sidex_Variant_Table_Columns(SdxVarHandle, iVal);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_Columns error (%d)',[err]);
  end;
  Result := iVal;
end;

function TsidexTableData.GetColumnNames: Variant;
var
  err : Integer;
  value : SIDEX_VARIANT;
begin
  err := sidex_Variant_Table_ColumnNames(SdxVarHandle,value);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_ColumnNames error (%d)',[err]);
  end;
  Result := VarSidexListCreate(value);
end;

function TsidexTableData.GetEof: Boolean;
begin
  Result := FEof;
end;

function TsidexTableData.GetField(ix: string): Variant;
var
  err : Integer;
  value : SIDEX_VARIANT;
begin
  err := sidex_Variant_Table_GetField (SdxVarHandle,FCurrentRow,PSIDEX_STRING(ix),value);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_GetField error (%d)',[err]);
  end;
  Result := SidexVariantAsVariant(value);
end;

function TsidexTableData.GetRowCount: Integer;
var
  iVal: SIDEX_INT32;
  err : Integer;
begin
  err := sidex_Variant_Table_Rows(SdxVarHandle, iVal);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_Rows error (%d)',[err]);
  end;
  Result := iVal;
end;

function TsidexTableData.HasColumn(cname: string): Boolean;
var
  err:     Integer;
  bHasCol: SIDEX_BOOL;
begin
  bHasCol := SIDEX_FALSE;
  err := sidex_Variant_Table_HasColumn(SdxVarHandle, PSIDEX_STRING(cname), bHasCol);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_HasColumn error (%d)',[err]);
  end;
  Result := (bHasCol <> SIDEX_FALSE);
end;

function TsidexTableData.GetColumnName(index: Integer): string;
var
  err:      Integer;
  pColName: PSIDEX_STRING;
begin
  err := sidex_Variant_Table_GetColumnName(SdxVarHandle, index, pColName);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_Table_GetColumnName error (%d)',[err]);
  end;
  Result := pColName;
end;

function TsidexTableData.NewSidexHandle: SIDEX_VARIANT;
begin
  Result := sidex_Variant_New_Table();
end;

procedure TsidexTableData.Next;
begin
  if FCurrentRow < 0 then
  begin
    raise Exception.Create('no first called before calling next');
  end;

  Inc(FCurrentRow);
  if FCurrentRow >= RowCount then
  begin
    FEof := true;
    Dec(FCurrentRow);
  end
  else FEof := false;
end;

procedure TsidexTableData.OnCreate;
begin
  inherited OnCreate;
  FCurrentRow := -1;
end;


procedure TsidexTableData.SetCurrentRow(const Value: Integer);
begin
  if Value >= RowCount then
  begin
    raise Exception.Create('invalid row number');
  end;
  FCurrentRow := Value;
end;

procedure TsidexTableData.SetField(ix: string; const Value: Variant);
var
  err  : Integer;
  sval : SIDEX_VARIANT;
begin
  if not HasColumn(ix) then
    AddColumn(ix);

  sval := VariantAsSidexVariant(Value);
  err := sidex_Variant_Table_SetField(SdxVarHandle,FCurrentRow,PSIDEX_STRING(ix),sval);
  if err <> SIDEX_SUCCESS then
  begin
    sidex_Variant_DecRef(sval);
    raise ESidexError.CreateFmt('sidex_Variant_Table_SetField error (%d)',[err]);
  end;
  sidex_Variant_DecRef(sval);
end;

//#---------------------------------------------------------------------
//#---------------------------------------------------------------------
//# converting Delphi Variants into SIDEX Variants and vice versa
//#---------------------------------------------------------------------
//#---------------------------------------------------------------------
function VariantAsSidexVariant(const V: Variant) : SIDEX_VARIANT;

  function ArrayVarDim1 : SIDEX_VARIANT;
  var
    i    : Integer;
    val  : SIDEX_VARIANT;
    err  : Integer;
    lpos : SIDEX_INT32;
  begin
    Result := sidex_Variant_New_List();
    for i := VarArrayLowBound( V, 1 ) to VarArrayHighBound( V, 1 ) do
    begin
      val := VariantAsSidexVariant(V[i]);
      err := sidex_Variant_List_Append(Result,val,lpos);
      if err <> SIDEX_SUCCESS then
      begin
        sidex_Variant_DecRef(val);
        sidex_Variant_DecRef(Result);
        raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
      end;
      sidex_Variant_DecRef(val);
    end;
  end;

  function ArrayVarDim2 : SIDEX_VARIANT;
  var
    i, j   : Integer;
    L, val : SIDEX_VARIANT;
    err    : Integer;
    lpos   : SIDEX_INT32;
  begin
    Result := sidex_Variant_New_List();
    for i := VarArrayLowBound( V, 1 ) to VarArrayHighBound( V, 1 ) do
    begin
      L := sidex_Variant_New_List();
      err := sidex_Variant_List_Append(Result,L,lpos);
      if err <> SIDEX_SUCCESS then
      begin
        sidex_Variant_DecRef(L);
        sidex_Variant_DecRef(Result);
        raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
      end;
      for j := VarArrayLowBound( V, 2 ) to VarArrayHighBound( V, 2 ) do
      begin
        val := VariantAsSidexVariant(V[i, j]);
        err := sidex_Variant_List_Append(L,val,lpos);
        if err <> SIDEX_SUCCESS then
        begin
          sidex_Variant_DecRef(val);
          sidex_Variant_DecRef(L);
          sidex_Variant_DecRef(Result);
          raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
        end;
        sidex_Variant_DecRef(val);
      end;
      sidex_Variant_DecRef(L);
    end;
  end;

  function ArrayVarDim3 : SIDEX_VARIANT;
  var
    i, j, k    : Integer;
    L, L2, val : SIDEX_VARIANT;
    err        : Integer;
    lpos       : SIDEX_INT32;
  begin
    Result := sidex_Variant_New_List();
    for i := VarArrayLowBound( V, 1 ) to VarArrayHighBound( V, 1 ) do
    begin
      L := sidex_Variant_New_List();
      err := sidex_Variant_List_Append(Result,L,lpos);
      if err <> SIDEX_SUCCESS then
      begin
        sidex_Variant_DecRef(L);
        sidex_Variant_DecRef(Result);
        raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
      end;
      for j := VarArrayLowBound( V, 2 ) to VarArrayHighBound( V, 2 ) do
      begin
        L2  := sidex_Variant_New_List();
        err := sidex_Variant_List_Append(L,L2,lpos);
        if err <> SIDEX_SUCCESS then
        begin
          sidex_Variant_DecRef(L);
          sidex_Variant_DecRef(L2);
          sidex_Variant_DecRef(Result);
          raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
        end;
        for k := VarArrayLowBound( V, 3 ) to VarArrayHighBound( V, 3 ) do
        begin
          val := VariantAsSidexVariant(V[i, j, k]);
          err := sidex_Variant_List_Append(L2,val,lpos);
          if err <> SIDEX_SUCCESS then
          begin
            sidex_Variant_DecRef(val);
            sidex_Variant_DecRef(L);
            sidex_Variant_DecRef(L2);
            sidex_Variant_DecRef(Result);
            raise ESidexError.CreateFmt('sidex_Variant_List_Append error (%d)',[err]);
          end;
          sidex_Variant_DecRef(val);
        end;
        sidex_Variant_DecRef(L2);
      end;
      sidex_Variant_DecRef(L);
    end;
  end;

var
  y, m, d, h, mi, sec, ms : WORD;
  dt     : TDateTime;
  dts, s : string;
  err    : Integer;
begin
  case VarType(V) and VarTypeMask of
    varBoolean:
    begin
      if V then Result := sidex_Variant_New_Boolean(SIDEX_TRUE)
           else Result := sidex_Variant_New_Boolean(SIDEX_FALSE);
    end;
    varSmallint,
    varByte,
    varShortInt,
    varWord,
    varLongWord,
    varInteger:  Result := sidex_Variant_New_Integer(V);
    varInt64:    Result := sidex_Variant_New_Integer(V);
    varSingle,
    varDouble,
    varCurrency: Result := sidex_Variant_New_Float(V);
    varDate:
    begin
      dt := V;
      DecodeDate(dt, y, m, d);
      DecodeTime(dt, h, mi, sec, ms);
      dts := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d:%.3d',[y,m,d,h,mi,sec,ms]);
      err := sidex_Variant_New_DateTime(PSIDEX_STRING(dts), Result);
      if err <> SIDEX_SUCCESS then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_New_DateTime error (%d)',[err]);
      end;
    end;
    varOleStr:
    begin
      if (TVarData(V).VOleStr = nil) or
         (TVarData(V).VOleStr^ = #0) then s := ''
                                     else s := V;
      err := sidex_Variant_New_String(PSIDEX_STRING(s), Result);
      if err <> SIDEX_SUCCESS then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_New_String error (%d)',[err]);
      end;
    end;
    {$if defined(UNICODE)}
    varUString,
    {$ifend}
    varString:
    begin
      s := V;
      err := sidex_Variant_New_String(PSIDEX_STRING(s), Result);
      if err <> SIDEX_SUCCESS then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_New_String error (%d)',[err]);
      end;
    end;
    else
    begin
      if VarType(V) and varArray <> 0 then
      begin
        case VarArrayDimCount(V) of
          1: Result := ArrayVarDim1();
          2: Result := ArrayVarDim2();
          3: Result := ArrayVarDim3();
          else
          begin
            raise Exception.Create('Can''t convert a variant array of more than 3 dimensions to a SIDEX Variant List');
          end;
        end;
      end
      else if VarIsNull(V) or VarIsEmpty(V) then
      begin
        Result := sidex_Variant_New_None();
      end
      else if VarIsSidexList(V) or VarIsSidexDict(V) or VarIsSidexTable(V)
                  or VarIsSidexBinary(V) or VarIsSidexString(V) then
      begin
        Result := sidex_Variant_Copy(VarSidexGetHandle(V));
      end
      else
      begin
        raise Exception.Create('Datatype not supported by SIDEX');
      end;
    end;
  end; // of case
end;

function SidexVariantAsVariant(obj : SIDEX_VARIANT) : Variant;

  function GetSequenceItem(sequence : SIDEX_VARIANT; idx : Integer) : Variant;
  var
    val : SIDEX_VARIANT;
    err : Integer;
  begin
    err := sidex_Variant_List_Get(sequence,idx,val);
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_List_Get error (%d)',[err]);
    end;
    Result := SidexVariantAsVariant(val);
  end;

  function ParseDateTimeString(const dtstr : string; var ly, lm, ld, lh, lmi, lsec, lmsec : WORD): Boolean;
  var
    slist: TStringList;
  begin
    // parse value of the form YYYY-MM-DD hh:mm:ss:ttt into values
    Result := false;
    slist := TStringList.Create;
    if Assigned(slist) then
    begin
      try
        ExtractStrings([':', ' ', '-'], [], PChar(dtstr), slist);
        if slist.Count = 7 then
        begin
          ly     := StrToInt(slist.Strings[0]);
          lm     := StrToInt(slist.Strings[1]);
          ld     := StrToInt(slist.Strings[2]);
          lh     := StrToInt(slist.Strings[3]);
          lmi    := StrToInt(slist.Strings[4]);
          lsec   := StrToInt(slist.Strings[5]);
          lmsec  := StrToInt(slist.Strings[6]);
          Result := True;
        end;
      finally
        FreeAndNil(slist);
      end;
    end;
  end;

var
  floatval      : SIDEX_DOUBLE;
  boolval       : SIDEX_BOOL;
  intval        : SIDEX_INT64;
  pstrval       : PSIDEX_STRING;
  pslength      : SIDEX_INT32;
  dt            : TDateTime;
  y, m, d, h, mi, sec, msec : WORD;
  err           : Integer;
  sdxVar        : SIDEX_VARIANT;
begin
  if sidex_Variant_Float_Check(obj) <> SIDEX_FALSE then
  begin
    floatval := 0.0;
    err := sidex_Variant_As_Float(obj,floatval);
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_As_Float error (%d)',[err]);
    end;
    Result := floatval;
  end
  else if sidex_Variant_Boolean_Check(obj) <> SIDEX_FALSE then  // we must check Bool before Int, as Boolean type inherits from Int.
  begin
    boolval := SIDEX_FALSE;
    err := sidex_Variant_As_Boolean(obj,boolval);
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_As_Boolean error (%d)',[err]);
    end;
    Result := (boolval <> SIDEX_FALSE);
  end
  else if sidex_Variant_Integer_Check(obj) <> SIDEX_FALSE then
  begin
    intval := 0;
    err := sidex_Variant_As_Integer(obj,intval);
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_As_Integer error (%d)',[err]);
    end;
    Result := intval;
  end
  else if sidex_Variant_String_Check(obj) <> SIDEX_FALSE then
  begin
    err := sidex_Variant_As_String(obj, pstrval, pslength); // borrowed
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_As_String error (%d)',[err]);
    end;
    Result := string(pstrval);
  end
  else if sidex_Variant_DateTime_Check(obj) <> SIDEX_FALSE then
  begin
    err := sidex_Variant_As_DateTime(obj, pstrval); // borrowed
    if err <> SIDEX_SUCCESS then
    begin
      raise ESidexError.CreateFmt('sidex_Variant_As_DateTime error (%d)',[err]);
    end;
    y := 0; m := 0; d := 0; h := 0; mi := 0; sec := 0; msec := 0;
    if ParseDateTimeString(string(pstrval), y, m, d, h, mi, sec, msec) then
    begin
      dt := EncodeDate( y, m, d ) + EncodeTime( h, mi, sec, msec );
      Result := dt;
    end
    else
    begin
      raise ESidexError.CreateFmt('invalid date/time format (%s)',[string(pstrval)]);
    end;
  end
  else if sidex_Variant_List_Check( obj ) <> SIDEX_FALSE then
  begin
    sdxVar := sidex_Variant_Copy(obj);
    Result := VarSidexListCreate(sdxVar);
    sidex_Variant_DecRef(sdxVar);
  end
  else if sidex_Variant_Dict_Check( obj ) <> SIDEX_FALSE then
  begin
    sdxVar := sidex_Variant_Copy(obj);
    Result := VarSidexDictCreate(sdxVar);
    sidex_Variant_DecRef(sdxVar);
  end
  else if sidex_Variant_Table_Check( obj ) <> SIDEX_FALSE then
  begin
    sdxVar := sidex_Variant_Copy(obj);
    Result := VarSidexTableCreate(sdxVar);
    sidex_Variant_DecRef(sdxVar);
  end
  else
  begin
    Result := Null;
  end;
end;

//------------------------------------------------------------------------------

{ TsidexVarString }

procedure TsidexVarString.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TsidexStringVarData(V).VsidexData);
end;

//------------------------------------------------------------------------------

procedure TsidexVarString.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    {$if not defined(FPC)}
      VarDataInit(Dest);
    {$ifend}
    with TsidexStringVarData(Dest) do
    begin
      VType      := VarSidexString;
      VsidexData := TsidexStringData.Create(TsidexStringVarData(Source).VsidexData);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TsidexVarString.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
function TsidexVarString.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
function TsidexVarString.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
function TsidexVarString.SetProperty(
  {$if defined(FPC) and (FPC_VERSION >= 3)} var {$else} const {$ifend}
  V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
{ TsidexStringData }
//------------------------------------------------------------------------------

constructor TsidexStringData.Create;
begin
  inherited Create;
  FSdxVarHandle := NewSidexHandle;
  OnCreate();
end;

constructor TsidexStringData.Create(const s: string);
begin
  inherited Create;
  FSdxVarHandle := NewSidexHandleWithValue(s);
  OnCreate();
end;

function TsidexStringData.CheckSidexType(aval: SIDEX_VARIANT): Boolean;
begin
  Result := (sidex_Variant_String_Check(aval) <> SIDEX_FALSE);
end;

function TsidexStringData.GetLength: Integer;
var
  err, nData: SIDEX_INT32;
begin
  Result := 0;
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    nData := 0;
    err := sidex_Variant_As_String_Length(FSdxVarHandle, nData);
    if err <> SIDEX_SUCCESS then
    begin
      Result := -2; // error occurred
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_As_String_Length error (%d)', [err]);
      end;
    end
    else if nData > 0 then Result := nData;
  end;
end;

function TsidexStringData.GetFormat: string;
var
  err  : SIDEX_INT32;
  pstr : PSIDEX_STRING;
begin
  Result := SIDEX_STRING_FORMAT_UNKNOWN;
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    err := sidex_Variant_String_GetFormat(FSdxVarHandle, pstr);
    if err <> SIDEX_SUCCESS then
    begin
      // error occurred
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_String_GetFormat error (%d)', [err]);
      end;
    end
    else Result := pstr;
  end;
end;

procedure TsidexStringData.SetFormat(const Value: string);
var
  err : SIDEX_INT32;
begin
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    err := sidex_Variant_String_SetFormat(FSdxVarHandle, PSIDEX_STRING(Value));
    if err <> SIDEX_SUCCESS then
    begin
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_String_SetFormat error (%d)', [err]);
      end;
    end
  end;
end;

function TsidexStringData.GetValue: string;
var
  err, iLength : SIDEX_INT32;
  pstr         : PSIDEX_STRING;
begin
  Result := '';
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    pstr    := nil;
    iLength := 0;
    err     := sidex_Variant_As_String(FSdxVarHandle, pstr, iLength);
    if err <> SIDEX_SUCCESS then
    begin
      // error occurred
      if uSidexLib.sidexUseExceptions then
      begin
        raise ESidexError.CreateFmt('sidex_Variant_As_String error (%d)', [err]);
      end;
    end
    else Result := pstr;
  end;
end;

procedure TsidexStringData.SetValue(const Value: string);
var
  sFormat : string;
begin
  sFormat := GetFormat;
  if FSdxVarHandle <> SIDEX_VARIANT_NULL then
  begin
    sidex_Variant_DecRef(FSdxVarHandle);
  end;
  FSdxVarHandle := NewSidexHandleWithValue(Value);
  SetFormat(sFormat);
end;

function TsidexStringData.NewSidexHandle: SIDEX_VARIANT;
begin
  Result := NewSidexHandleWithValue('');
end;

function TsidexStringData.NewSidexHandleWithValue(const s: string): SIDEX_VARIANT;
var
  err : SIDEX_INT32;
begin
  err := sidex_Variant_New_String(PSIDEX_STRING(s), Result);
  if err <> SIDEX_SUCCESS then
  begin
    raise ESidexError.CreateFmt('sidex_Variant_New_String error (%d)', [err]);
  end;
end;

//------------------------------------------------------------------------------

initialization

  sidexVarBinary := TsidexVarBinary.Create;
  sidexVarList   := TsidexVarList.Create;
  sidexVarDict   := TsidexVarDict.Create;
  sidexVarTable  := TsidexVarTable.Create;
  sidexVarString := TsidexVarString.Create;

//------------------------------------------------------------------------------

finalization

  FreeAndNil(sidexVarTable);
  FreeAndNil(sidexVarDict);
  FreeAndNil(sidexVarList);
  FreeAndNil(sidexVarBinary);
  FreeandNil(sidexVarString);

//------------------------------------------------------------------------------

end.

